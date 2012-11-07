(ns monads.core
  (:refer-clojure :exclude [do seq map])
  (:require [clojure.set :as set]
            [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Protocols
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Monad
  (do-result [_ v])
  (bind [mv f]))

(defprotocol MonadZero
  (zero [_])
  (plus-step [mv mvs])
  (plus-step* [mv mvs]))

(defprotocol MonadDev
  "Used in conjunction with the return type checker."
  (val-types [_])
  (name-monad [_]))

(defprotocol MonadWriter
  "Accumulation of values into containers."
  (writer-m-empty [_]
    "return an empty container")
  (writer-m-add [container value]
    "add value to container, return new container")
  (writer-m-combine [container1 container2]
    "combine two containers, return new container"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Return type checker
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *throw-on-mismatch* false)
(def ^:dynamic *warn-on-mismatch*  false)

(defn- mismatch-message [mv rt ts]
  (let [tw (if (= 1 (count ts))
             "type"
             "types")]
    (str "Type mismatch between bound monadic value and return value of monadic function. "
         "Function returned type "
         (second (string/split (str rt) #" "))
         ". The protocol-monad "
         (str "<" (name-monad mv) ">")
         " specifies value "
         tw
         " "
         (string/join ", " (clojure.core/map #(second (string/split (str %) #" ")) ts))
         ".")))

(defn- check-return-type [mv f ts warn-on-mismatch throw-on-mismatch]
  (fn [v]
    (let [rv (f v)]
      (if-not (clojure.core/seq ts)
        rv
        (let [rt (class rv)]
          (if (some #(= rt %) ts)
            rv
            (cond
              (and warn-on-mismatch (not throw-on-mismatch))
              (let []
                (println (mismatch-message mv rt ts))
                rv)
              throw-on-mismatch
              (throw (Exception. (mismatch-message mv rt ts)))
              :else
              rv)))))))

(defn- wrap-check [mv f]
  (if-not (or *throw-on-mismatch*
              *warn-on-mismatch*)
    f
    (check-return-type mv f (val-types mv) *warn-on-mismatch* *throw-on-mismatch*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Monad and utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn plus [[mv & mvs]]
  (plus-step mv mvs))

(defmacro plus*
  "Lazy variant of plus. Implemented as a macro to avoid eager
  argument evaluation."
  [[mv & mvs]]
  `(plus-step* ~mv (list ~@(clojure.core/map (fn thunk [m] `(fn [] ~m)) mvs))))

(defmacro do
  "Monad comprehension. Takes the name of a monadic value factory
   function (like vector, hash-set, m/maybe), a vector of steps given as
   binding-form/monadic-expression pairs, and a result value specified
   by expr. The monadic-expression terms can use the binding variables
   of the previous steps.

   If the monad contains a definition of m-zero, the step list can also
   contain conditions of the form :when p, where the predicate p can
   contain the binding variables from all previous steps.

   A clause of the form :let [binding-form expr ...], where the bindings
   are given as a vector as for the use in let, establishes additional
   bindings that can be used in the following steps. "
  [mv-factory bindings expr]
  (let [steps (partition 2 bindings)]
    ;; The "dummy value" [nil] is used in several expressions below to
    ;; couple calls to bind, zero and do-result to a particular
    ;; protocol-monad, as determined by the return type of the monadic
    ;; value factory functin
    `(monads.core/bind (~mv-factory [nil])
                       (fn [_#]
                         ~(reduce (fn [expr [sym mv]]
                                    (cond
                                      (= :when sym) `(if ~mv
                                                       ~expr
                                                       (monads.core/zero (~mv-factory [nil])))
                                      (= :let sym) `(let ~mv
                                                      ~expr)
                                      :else `(monads.core/bind ~mv (fn [~sym]
                                                                     ~expr))))
                                  `(monads.core/do-result (~mv-factory [nil]) ~expr)
                                  (reverse steps))))))

(defn- comprehend [f mvs]
  (let [mv (first mvs)
        rest-steps (reduce (fn [steps mv]
                             (fn [acc x]
                               (bind mv (partial steps (conj acc x)))))
                           (fn [acc x]
                             (do-result mv (f (conj acc x))))
                           (reverse (rest mvs)))]
    (bind mv (partial rest-steps []))))

(defn seq
  "'Executes' the monadic values in 'mvs' and returns a sequence of the
   basic values contained in them."
  ([mvs]
     (assert (clojure.core/seq mvs)
             "At least one monadic value is required by monads.core/seq")
     (seq (first mvs) mvs))
  ([mv-factory mvs]
     (if (clojure.core/seq mvs)
       (comprehend identity mvs)
       (mv-factory []))))

(defn lift
  "Converts a function f to a function of monadic arguments
   returning a monadic value."
  [f]
  (fn [& mvs]
    (comprehend (partial apply f) mvs)))

(defn join
  "Converts a monadic value containing a monadic value into a 'simple'
   monadic value."
  [mv]
  (bind mv identity))

(defn fmap
  "Bind the monadic value mv to the function f. Returning (f x) for argument x"
  [f mv]
  (bind mv (fn [x] (do-result mv (f x)))))

(defn map
  "'Executes' the sequence of monadic values resulting from mapping
   f onto the values xs. f must return a monadic value."
  [f xs]
  (seq (clojure.core/map f xs)))

(defn chain
  "Chains together monadic computation steps that are each functions
   of one parameter. Each step is called with the result of the previous
   step as its argument. (m-chain (step1 step2)) is equivalent to
   (fn [x] (domonad [r1 (step1 x) r2 (step2 r1)] r2))."
  [steps]
  (fn [x]
    (let [mv ((first steps) x)
          chain (reduce (fn [chain step]
                          (fn [x]
                            (bind (step x) chain)))
                        (partial do-result mv)
                        (reverse (rest steps)))]
      (bind mv chain))))

(defn- lazy-concat
  ([l] l)
  ([l ls]
     (lazy-seq
       (cond
         (clojure.core/seq l) (cons (first l)
                                    (lazy-concat (rest l) ls))
         (clojure.core/seq ls) (lazy-concat (first ls) (rest ls))
         :else (lazy-seq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistenList
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monadd describing multi-valued computations, i.e. computations that
;; can yield multiple values as lists.

(extend-type clojure.lang.PersistentList
  Monad
  (do-result [_ v]
    (list v))
  (bind [mv f]
    (apply list (mapcat (wrap-check mv f) mv)))

  MonadZero
  (zero [_]
    (list))
  (plus-step [mv mvs]
    (apply concat mv mvs))
  (plus-step* [mv mvs]
    (apply concat mv (clojure.core/map #(%) mvs)))

  MonadDev
  (val-types [_]
    [clojure.lang.PersistentList
     clojure.lang.PersistentList$EmptyList])
  (name-monad [_]
    "list")

  MonadWriter
  (writer-m-empty [_] (list))
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentList$EmptyList
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monadd describing multi-valued computations, i.e. computations that
;; can yield multiple values as lists.

(extend-type clojure.lang.PersistentList$EmptyList
  Monad
  (do-result [_ v]
    (list v))
  (bind [mv f]
    (apply list (mapcat (wrap-check mv f) mv)))

  MonadZero
  (zero [_]
    (list))
  (plus-step [mv mvs]
    (apply concat mv mvs))
  (plus-step* [mv mvs]
    (apply concat mv (clojure.core/map #(%) mvs)))

  MonadDev
  (val-types [_]
    [clojure.lang.PersistentList
     clojure.lang.PersistentList$EmptyList])
  (name-monad [_]
    "list")

  MonadWriter
  (writer-m-empty [_] (list))
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentVector
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monadd describing multi-valued computations, i.e. computations that
;; can yield multiple values as vectors.

(extend-type clojure.lang.PersistentVector
  Monad
  (do-result [_ v]
    [v])
  (bind [mv f]
    (vec (mapcat (wrap-check mv f) mv)))

  MonadZero
  (zero [_]
    [])
  (plus-step [mv mvs]
    (vec (apply concat mv mvs)))
  (plus-step* [mv mvs]
    (vec (apply concat mv (clojure.core/map #(%) mvs))))

  MonadDev
  (val-types [_]
    [clojure.lang.PersistentVector])
  (name-monad [_]
    "vector")

  MonadWriter
  (writer-m-empty [_] [])
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (vec (concat c1 c2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.LazySeq
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monadd describing multi-valued computations, i.e. computations that
;; can yield multiple values as lazy sequences.

(extend-type clojure.lang.LazySeq
  Monad
  (do-result [_ v]
    (lazy-seq [v]))
  (bind [mv f]
    (mapcat (wrap-check mv f) mv))

  MonadZero
  (zero [_]
    (lazy-seq))
  (plus-step [mv mvs]
    (lazy-concat mv mvs))
  (plus-step* [mv mvs]
    (lazy-concat mv (clojure.core/map #(%) mvs)))

  MonadDev
  (val-types [_]
    [clojure.lang.LazySeq])
  (name-monad [_]
    "lazy-seq")

  MonadWriter
  (writer-m-empty [_] (list))
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentHashSet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monadd describing multi-valued computations, i.e. computations that
;; can yield multiple values as sets.

(extend-type clojure.lang.PersistentHashSet
  Monad
  (do-result [_ v]
    (hash-set v))
  (bind [mv f]
    (apply set/union
           (clojure.core/map (wrap-check mv f) mv)))

  MonadZero
  (zero [_]
    #{})
  (plus-step [mv mvs]
    (apply set/union mv mvs))
  (plus-step* [mv mvs]
    (apply set/union mv (clojure.core/map #(%) mvs)))

  MonadDev
  (val-types [_]
    [clojure.lang.PersistentHashSet])
  (name-monad [_]
    "hash-set")

  MonadWriter
  (writer-m-empty [_] #{})
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (clojure.set/union c1 c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Maybe monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing computations with possible failures. Failure is
;; represented by maybe-zero-val, any other value is considered valid.
;; As soon as a step returns maybe-zero-val, the whole computation
;; will yield maybe-zero-val as well. For convenience `(maybe nil)`
;; returns maybe-zero-val, but this is not the case for constructor
;; call `(Maybe. nil)` in order to satisfy the first Monad Law.

(declare maybe-zero-val)

(deftype Maybe [v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (Maybe. v))
  (bind [mv f]
    (if (= mv maybe-zero-val)
      maybe-zero-val
      ((wrap-check mv f) @mv)))

  MonadZero
  (zero [_]
    maybe-zero-val)
  (plus-step [mv mvs]
    (let [mv (->> (cons mv mvs)
                  (drop-while #(= maybe-zero-val %))
                  first)]
      (if (nil? mv)
        maybe-zero-val
        mv)))
  (plus-step* [mv mvs]
    (if-not (= maybe-zero-val mv)
      mv
      (let [mv (->> mvs
                    (drop-while #(= maybe-zero-val (%)))
                    first)]
        (if (nil? mv)
          maybe-zero-val
          (mv)))))

  MonadDev
  (val-types [_]
    [Maybe])
  (name-monad [_]
    "maybe"))

(def maybe-zero-val (Maybe. ::nothing))

(defn maybe
  [v]
  (if (nil? v)
    maybe-zero-val
    (Maybe. v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  State monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing stateful computations. The monadic values have the
;; structure (fn [old-state] [result new-state]).

(deftype State [v mv f]
  clojure.lang.IFn
  (invoke [_ s]
    (if f
      (let [[v ss] (mv s)]
        ((f v) ss))
      [v s]))

  Monad
  (do-result [_ v]
    (State. v nil nil))
  (bind [mv f]
    (State. nil mv (wrap-check mv f)))

  MonadDev
  (val-types [_]
    [State])
  (name-monad [_]
    "state"))

(defn state
  [v]
  (State. v nil nil))

(defn update-state
  "Return a State monad value that replaces the current state by the
   result of f applied to the current state and that returns the old state."
  [f]
  (reify
    clojure.lang.IFn
    (invoke [_ s]
      [s (f s)])

    Monad
    (do-result [_ v]
      (State. v nil nil))
    (bind [mv f]
      (State. nil mv (wrap-check mv f)))

    MonadDev
    (val-types [_]
      [State])
    (name-monad [_]
      "state")))

(defn set-state
  "Return a State monad value that replaces the current state by s and
   returns the previous state."
  [s]
  (update-state (constantly s)))

(defn get-state
  "Return a State monad value that returns the current state and does not
   modify it."
  []
  (update-state identity))

(defn get-val
  "Return a State monad value that assumes the state to be a map and
   returns the value corresponding to the given key. The state is not modified."
  [key]
  (bind (get-state)
        #(state (get % key))))

(defn update-val
  "Return a State monad value that assumes the state to be a map and
   replaces the value associated with the given key by the return value
   of f applied to the old value and args. The old value is returned."
  [key f & args]
  (bind (update-state #(apply update-in % [key] f args))
        #(state (get % key))))

(defn set-val
  "Return a State monad value that assumes the state to be a map and
   replaces the value associated with key by val. The old value is returned."
  [key val]
  (update-val key (constantly val)))

(defn get-in-val [path & [default]]
  (bind (get-state)
        #(state (get-in % path default))))

(defn assoc-in-val [path val]
  (bind (update-state #(assoc-in % path val))
        #(state (get-in % path))))

(defn update-in-val [path f & args]
  (bind (update-state #(apply update-in % path f args))
        #(state (get-in % path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Continuation monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing computations in continuation-passing style. The
;; monadic values are functions that are called with a single argument
;; representing the continuation of the computation, to which they
;; pass their result.

(deftype Continuation [v mv f]
  clojure.lang.IDeref
  (deref [mv]
    (mv identity))

  clojure.lang.IFn
  (invoke [_ c]
    (if f
      (mv (fn [v] ((f v) c)))
      (c v)))

  Monad
  (do-result [_ v]
    (Continuation. v nil nil))
  (bind [mv f]
    (Continuation. nil mv (wrap-check mv f)))

  MonadDev
  (val-types [_]
    [Continuation])
  (name-monad [_]
    "cont"))

(defn cont
  [v]
  (Continuation. v nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CallWithCurrentContinuation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A computation in the Continuation monad that calls function f with
;; a single argument representing the current continuation. The
;; function f should return a continuation (which becomes the return
;; value of call-cc), or call the passed-in current continuation to
;; terminate.

;; Not yet implemented.
(defn call-cc
  [f]
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Writer monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing computations that accumulate data on the side,
;; e.g. for logging. The monadic values have the structure [value
;; log]. Any of the accumulators from clojure.contrib.accumulators can
;; be used for storing the log data. Its empty value is passed as a
;; parameter.

(deftype Writer [v accumulator]
  clojure.lang.IDeref
  (deref [_]
    [v accumulator])

  Monad
  (do-result [_ v]
    (Writer. v (writer-m-empty accumulator)))
  (bind [mv f]
    (let [[v1 a1] (deref mv)
          [v2 a2] (deref ((wrap-check mv f) v1))]
      (Writer. v2 (writer-m-combine a1 a2))))

  MonadDev
  (val-types [_]
    [Writer])
  (name-monad [_]
    "writer"))

(defn writer
  [accumulator]
  (fn [v]
    (Writer. v accumulator)))

(defn write [m-result val-to-write]
  (let [[_ a] (deref (m-result nil))]
    (Writer. nil (writer-m-add a val-to-write))))

(defn listen [mv]
  (let [[v a :as va] (deref mv)]
    (Writer. va a)))

(defn censor [f mv]
  (let [[v a] (deref mv)]
    (Writer. v (f a))))

(extend-type java.lang.String
  MonadWriter
  (writer-m-empty [_] "")
  (writer-m-add [c v] (str c v))
  (writer-m-combine [c1 c2] (str c1 c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  StateTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad m into a monad of
;; stateful computations that have the base monad type as their
;; result.

(deftype StateTransformer [m v mv f alts lzalts]
  clojure.lang.IFn
  (invoke [_ s]
    (cond
      ;; Use of clojure.core/doall "enforces" the non-laziness of
      ;; regular plus / plus-step. Otherwise you can end up with
      ;; monadic computations that make use of plus / plus-step which
      ;; sometimes are lazy enough and sometimes aren't (apparently
      ;; owing to sequence chunking, per a conversation in IRC).  If
      ;; laziness is desirable, then one should use plus* /
      ;; plus-step*.
      alts (plus-step ((first alts) s) (clojure.core/doall (clojure.core/map #(% s) (second alts))))
      lzalts (plus-step* ((first lzalts) s) (clojure.core/map #(fn [] ((%) s)) (second lzalts)))
      f (bind (mv s)
              (fn [[v ss]]
                ((f v) ss)))
      :else (if (= v (zero (m nil)))
              v
              (m [v s]))))

  Monad
  (do-result [_ v]
    (StateTransformer. m v nil nil nil nil))
  (bind [mv f]
    (StateTransformer. m nil mv (wrap-check mv f) nil nil))

  MonadZero
  (zero [_]
    (StateTransformer. m nil
                        (fn [s] (zero (m [nil])))
                        (fn [v]
                          (StateTransformer. m v nil nil nil nil))
                        nil
                        nil))
  (plus-step [mv mvs]
    (StateTransformer. m nil nil nil (list mv mvs) nil))
  (plus-step* [mv mvs]
    (StateTransformer. m nil nil nil nil (list mv mvs)))

  MonadDev
  (val-types [_]
    [StateTransformer])
  (name-monad [_]
    "state-t"))

(defn state-t
  [m]
  (if (= m maybe)
    (fn [v]
      (let [v (if (nil? v) maybe-zero-val v)]
        (StateTransformer. m v nil nil nil nil)))
    (fn [v]
      (StateTransformer. m v nil nil nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MaybeTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad m into a monad in which
;; the base values can be invalid (represented by :nothing).

(deftype MaybeTransformer [m v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (MaybeTransformer. m (m (maybe v))))
  (bind [mv f]
    (let [v (deref mv)]
      (MaybeTransformer. m (bind v (fn [x]
                                      (if (= x maybe-zero-val)
                                        (m maybe-zero-val)
                                        (deref ((wrap-check mv f) (deref x)))))))))

  MonadZero
  (zero [_]
    (MaybeTransformer. m (m maybe-zero-val)))
  (plus-step [mv mvs]
    (MaybeTransformer.
     m (bind (deref mv)
             (fn [x]
               (cond
                 (and (= x maybe-zero-val) (empty? mvs)) (m maybe-zero-val)
                 (= x maybe-zero-val) (deref (plus mvs))
                 :else (m x))))))

  MonadDev
  (val-types [_]
    [MaybeTransformer])
  (name-monad [_]
    "maybe-t"))

(defn maybe-t
  [m]
  (fn [v]
    (MaybeTransformer. m (m (maybe v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ListTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad m into a monad in which
;; the base values are lists.

(deftype ListTransformer [m v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (ListTransformer. m (m (list v))))
  (bind [mv f]
    (let [v (deref mv)]
      (ListTransformer. m (bind v (fn [xs]
                                     (if (clojure.core/seq xs)
                                       (->> xs
                                            (map (comp deref (wrap-check mv f)))
                                            (fmap (partial apply lazy-concat)))
                                       (m '())))))))
  MonadZero
  (zero [_]
    (ListTransformer. m (m '())))
  (plus-step [mv mvs]
    (ListTransformer.
     m (reduce (lift concat)
               (m '())
               (clojure.core/map deref (cons mv mvs)))))

  MonadDev
  (val-types [_]
    [ListTransformer])
  (name-monad [_]
    "list-t"))

(defn list-t
  [m]
  (fn [v]
    (ListTransformer. m (m (list v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  VectorTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad m into a monad in which
;; the base values are vectors.

(deftype VectorTransformer [m v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (VectorTransformer. m (m (vector v))))
  (bind [mv f]
    (let [v (deref mv)]
      (VectorTransformer. m (bind v (fn [xs]
                                       (if (clojure.core/seq xs)
                                         (->> xs
                                              (map (comp deref (wrap-check mv f)))
                                              (fmap (partial apply lazy-concat)))
                                         (m [])))))))

  MonadZero
  (zero [_]
    (VectorTransformer. m (m [])))
  (plus-step [mv mvs]
    (VectorTransformer.
     m (reduce (lift (comp vec concat))
               (m [])
               (clojure.core/map deref (cons mv mvs)))))

  MonadDev
  (val-types [_]
    [VectorTransformer])
  (name-monad [_]
    "vector-t"))

(defn vector-t
  [m]
  (fn [v]
    (VectorTransformer. m (m (vector v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SetTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad m into a monad in which
;; the base values are sets.

(deftype SetTransformer [m v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (SetTransformer. m (m (hash-set v))))
  (bind [mv f]
    (let [v (deref mv)]
      (SetTransformer. m (bind v (fn [xs]
                                    (if (clojure.core/seq xs)
                                      (->> xs
                                           (map (comp deref (wrap-check mv f)))
                                           (fmap (partial apply lazy-concat)))
                                      (m #{})))))))

  MonadZero
  (zero [_]
    (SetTransformer. m (m #{})))
  (plus-step [mv mvs]
    (SetTransformer.
     m (reduce (lift set/union)
               (m #{})
               (clojure.core/map deref (cons mv mvs)))))

  MonadDev
  (val-types [_]
    [SetTransformer])
  (name-monad [_]
    "set-t"))

(defn set-t
  [m]
  (fn [v]
    (SetTransformer. m (m (hash-set v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  WriterTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype WriterTransformer [m mv writer-m]
  clojure.lang.IDeref
  (deref [_]
    mv)

  Monad
  (do-result [_ v]
    (WriterTransformer.
     m (m (writer-m v)) writer-m))
  (bind [mv f]
    (let [mv (deref mv)]
      (WriterTransformer.
       m (bind mv (fn [v]
                    (let [[v1 a1] (deref v)]
                      (bind (deref ((wrap-check mv f) v1))
                            (fn [v]
                              (let [[v2 a2] (deref v)]
                                (m (Writer. v2 (writer-m-combine a1 a2)))))))))
       writer-m)))

  MonadZero
  (zero [mv]
    (let [v (deref mv)]
      (WriterTransformer. m (zero v) writer-m)))
  (plus-step [mv mvs]
    (WriterTransformer.
     m (plus (clojure.core/map deref (cons mv mvs)))
     writer-m))

  MonadDev
  (val-types [_]
    [WriterTransformer])
  (name-monad [_]
    "writer-t"))

(defn writer-t [m accumulator]
  (let [writer-m (writer accumulator)]
    (fn [v]
      (WriterTransformer. m (m (writer-m v)) writer-m))))
