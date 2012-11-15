(ns monads.core
  (:refer-clojure :exclude [do seq map list vector vec lazy-seq hash-set])
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
;;  Factory functions for clojure.core types, namespaced in monads.core
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def list clojure.core/list)

(def vector clojure.core/vector)

(def vec clojure.core/vec)

(def ^{:macro true} lazy-seq #'clojure.core/lazy-seq)

(defmacro lazy-seq*
  [& vs]
  `(lazy-seq (list ~@vs)))

(def hash-set clojure.core/hash-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def seq* clojure.core/seq)

(def map* clojure.core/map)

(defn lazy-concat
  ([l] l)
  ([l ls]
     (lazy-seq
       (cond
         (seq* l) (cons (first l)
                        (lazy-concat (rest l) ls))
         (seq* ls) (lazy-concat (first ls) (rest ls))
         :else (lazy-seq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Return type checker
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *throw-on-mismatch* false)
(def ^:dynamic *warn-on-mismatch*  false)

(defn- mismatch-message [mv mv-types return-val]
  (let [return-type (class return-val)
        tw (if (= 1 (count mv-types))
             "type"
             "types")]
    (str "Type mismatch between bound monadic value and return value of monadic function. "
         "Function returned type "
         (second (string/split (str return-type) #" "))
         ". The protocol-monad "
         (str "<" (name-monad mv) ">")
         " specifies value "
         tw
         " "
         (str mv-types)
         #_(string/join ", " (map* #(second (string/split (str %) #" ")) mv-types))
         ".")))

(defn- check-return-type [mv f warn-on-mismatch throw-on-mismatch]
  (fn [v]
    (let [return-val (f v)
          mv-types (val-types mv)]
      (if-not (seq* mv-types)
        return-val
        (if (some identity
                  (map* (fn [icps]
                          (every? (fn [icp]
                                    (instance? icp return-val))
                                  icps))
                        mv-types))
          return-val
          (cond
            (and warn-on-mismatch (not throw-on-mismatch))
            (let []
              (println (mismatch-message mv mv-types return-val))
              return-val)
            throw-on-mismatch
            (throw (Exception. (mismatch-message mv mv-types return-val)))
            :else
            return-val))))))

(defn- wrap-check [mv f]
  (if-not (or *throw-on-mismatch*
              *warn-on-mismatch*)
    f
    (check-return-type mv f *warn-on-mismatch* *throw-on-mismatch*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The "dummy value" [nil] is used in several expressions below to
;; couple calls to bind, zero and do-result to a particular
;; protocol-monad, as determined by the return type of the monadic
;; value factory function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentList monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing multi-valued computations, i.e. computations that
;; can yield multiple values as a list of those values.

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
    (apply list (apply concat mv mvs)))
  (plus-step* [mv mvs]
    (apply list (apply concat mv (map* #(%) mvs))))

  MonadDev
  (val-types [_]
    [[java.util.List]])
  (name-monad [_]
    "list")

  MonadWriter
  (writer-m-empty [_] (list))
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentList$EmptyList monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing multi-valued computations, i.e. computations that
;; can yield multiple values as a list of those values.

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
    (apply list (apply concat mv mvs)))
  (plus-step* [mv mvs]
    (apply list (apply concat mv (map* #(%) mvs))))

  MonadDev
  (val-types [_]
    [[java.util.List]])
  (name-monad [_]
    "list")

  MonadWriter
  (writer-m-empty [_] (list))
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentVector monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing multi-valued computations, i.e. computations that
;; can yield multiple values as vectors of those values.

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
    (vec (apply concat mv (map* #(%) mvs))))

  MonadDev
  (val-types [_]
    [[java.util.List]])
  (name-monad [_]
    "vector")

  MonadWriter
  (writer-m-empty [_] [])
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (vec (concat c1 c2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.LazySeq monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing multi-valued computations, i.e. computations that
;; can yield multiple values as lazy sequences of those values.

(extend-type clojure.lang.LazySeq
  Monad
  (do-result [_ v]
    (lazy-seq* v))
  (bind [mv f]
    (lazy-seq (mapcat (wrap-check mv f) mv)))

  MonadZero
  (zero [_]
    (lazy-seq))
  (plus-step [mv mvs]
    (lazy-concat mv mvs))
  (plus-step* [mv mvs]
    (lazy-concat mv (map* #(%) mvs)))

  MonadDev
  (val-types [_]
    [[java.util.List]])
  (name-monad [_]
    "lazy-seq")

  MonadWriter
  (writer-m-empty [_] (lazy-seq))
  (writer-m-add [c v] (cons v c))
  (writer-m-combine [c1 c2] (lazy-concat c1 (lazy-seq* c2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentHashSet monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing multi-valued computations, i.e. computations that
;; can yield multiple values as sets of those values.

(extend-type clojure.lang.PersistentHashSet
  Monad
  (do-result [_ v]
    #{v})
  (bind [mv f]
    (apply hash-set
           (apply set/union
                  (map* (wrap-check mv f) mv))))

  MonadZero
  (zero [_]
    #{})
  (plus-step [mv mvs]
    (apply set/union mv mvs))
  (plus-step* [mv mvs]
    (apply set/union mv (map* #(%) mvs)))

  MonadDev
  (val-types [_]
    [[java.util.Set]])
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
;; represented by monads.core/Nothing, any other value is considered
;; valid. As soon as a step returns monads.core/Nothing, the whole
;; computation will yield monads.core/Nothing as well. For convenience
;; `(monads.core/maybe monads.core/*Nothing*)` returns
;; monads.core/Nothing, but this is not the case for constructor call
;; `(monads.core.Maybe. monads.core/*Nothing*)` in order to satisfy
;; the First Monad Law for all values. The default value for
;; monads.core/*Nothing* is nil. Instances of monads.core.Maybe can be
;; dereferenced to yield the values they encapsulate;
;; monads.core/Nothing is a special case - when dereferenced, it
;; returns the value bound to monads.core/*Nothing*.

(def ^:dynamic *Nothing* nil)

(declare Nothing)

(deftype Maybe [v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str Maybe))
             (.hashCode this)))
  (hashCode [_]
    (hash v))
  (equals [this that]
    (and (= Maybe (class that))
         (= (.v that) v)))

  clojure.lang.IDeref
  (deref [mv]
    (if (= Nothing mv)
      *Nothing*
      v))

  Monad
  (do-result [_ v]
    (Maybe. v))
  (bind [mv f]
    (if (= mv Nothing)
      Nothing
      ((wrap-check mv f) (deref mv))))

  MonadZero
  (zero [_]
    Nothing)
  (plus-step [mv mvs]
    (let [mv (->> (cons mv mvs)
                  (drop-while #(= Nothing %))
                  first)]
      (if (nil? mv)
        Nothing
        mv)))
  (plus-step* [mv mvs]
    (if-not (= Nothing mv)
      mv
      (let [mv (->> mvs
                    (drop-while #(= Nothing (%)))
                    first)]
        (if (nil? mv)
          Nothing
          (mv)))))

  MonadDev
  (val-types [_]
    [[Maybe]])
  (name-monad [_]
    "maybe"))

(def Nothing (Maybe. ::Nothing))

(defn maybe
  [v]
  (if (= *Nothing* v)
    Nothing
    (Maybe. v)))

(let [pr-on (ns-resolve 'clojure.core 'pr-on)
      print-sequential (ns-resolve 'clojure.core 'print-sequential)]
  (defmethod print-method monads.core.Maybe [o ^java.io.Writer w]
    (print-sequential (format "#<%s@%x%s: "
                              (.getSimpleName (class o))
                              (System/identityHashCode o)
                              "")
                      pr-on
                      ""
                      ">"
                      (list (let [vo (.v o)]
                              (if (= ::Nothing vo)
                                'Nothing
                                vo)))
                      w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  State monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing stateful computations. The monadic values have the
;; structure (fn [old-state] [result new-state]).

(defprotocol IState
  (i-state [_]))

(deftype State [v mv f]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str State))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash v)
             (hash mv)
             (hash f)))
  (equals [this that]
    (and (= State (class that))
         (and (= (.v that) v)
              (= (.mv that) mv)
              (= (.f that) f))))

  clojure.lang.IFn
  (invoke [_ s]
    (if f
      (let [[v ss] (mv s)]
        ((f v) ss))
      [v s]))

  Monad
  (do-result [_ v]
    (State. v
            nil
            nil))
  (bind [mv f]
    (State. nil
            mv
            (wrap-check mv f)))

  MonadDev
  (val-types [_]
    [[monads.core.IState]])
  (name-monad [_]
    "state")

  IState
  (i-state [_]))

(defn state
  ([v]
     (State. v
             nil
             nil))
  ([mv f]
     (State. nil
             mv
             f)))

(defn update-state
  "Return a State monad value that replaces the current state by the
   result of f applied to the current state and that returns the old state."
  [f]
  (reify
    clojure.lang.IHashEq
    (hasheq [this]
      (bit-xor (hash (str (class this)))
               (.hashCode this)))
    (hashCode [_]
      (hash f))
    (equals [this that]
      (and (= (class this) (class that))
           (= (.hashCode this)
              (.hashCode that))))

    clojure.lang.IFn
    (invoke [_ s]
      [s (f s)])

    Monad
    (do-result [_ v]
      (State. v
              nil
              nil))
    (bind [mv f]
      (State. nil
              mv
              (wrap-check mv f)))

    MonadDev
    (val-types [_]
      [[monads.core.IState]])
    (name-monad [_]
      "state")

    IState
    (i-state [_])))

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

(defn get-state-val
  "Return a State monad value that assumes the state to be a map and
   returns the value corresponding to the given key. The state is not modified."
  [key]
  (bind (get-state)
        #(state (get % key))))

(defn update-state-val
  "Return a State monad value that assumes the state to be a map and
   replaces the value associated with the given key by the return value
   of f applied to the old value and args. The old value is returned."
  [key f & args]
  (bind (update-state #(apply update-in % [key] f args))
        #(state (get % key))))

(defn set-state-val
  "Return a State monad value that assumes the state to be a map and
   replaces the value associated with key by val. The old value is returned."
  [key val]
  (update-state-val key (constantly val)))

(defn get-in-state-val
  [path & [default]]
  (bind (get-state)
        #(state (get-in % path default))))

(defn assoc-in-state-val
  [path val]
  (bind (update-state #(assoc-in % path val))
        #(state (get-in % path))))

(defn update-in-state-val
  [path f & args]
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
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str Continuation))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash v)
             (hash mv)
             (hash f)))
  (equals [this that]
    (and (= Continuation (class that))
         (and (= (.v that) v)
              (= (.mv that) mv)
              (= (.f that) f))))

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
    (Continuation. v
                   nil
                   nil))
  (bind [mv f]
    (Continuation. nil
                   mv
                   (wrap-check mv f)))

  MonadDev
  (val-types [_]
    [[Continuation]])
  (name-monad [_]
    "cont"))

(defn cont
  [v]
  (Continuation. v
                 nil
                 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  call-cc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A computation in the Continuation monad that calls function f with
;; a single argument representing the current continuation. The
;; function f should return a continuation (which becomes the return
;; value of call-cc), or call the passed-in current continuation to
;; terminate.

(comment

  "call-cc is not yet implemented."

  (defn call-cc
    [f]
    )

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

;; The comment above may not be accurate with respect to
;; clojure.contrib.accumulators; probably need to adjust in light of
;; protocol MonadWriter.

(deftype Writer [v accumulator]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str Writer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash v)
             (hash accumulator)))
  (equals [this that]
    (and (= Writer (class that))
         (and (= (.v that) v)
              (= (.accumulator that) accumulator))))

  clojure.lang.IDeref
  (deref [_]
    [v accumulator])

  Monad
  (do-result [_ v]
    (Writer. v
             (writer-m-empty accumulator)))
  (bind [mv f]
    (let [[v1 a1] (deref mv)
          [v2 a2] (deref ((wrap-check mv f) v1))]
      (Writer. v2
               (writer-m-combine a1 a2))))

  MonadDev
  (val-types [_]
    [[Writer]])
  (name-monad [_]
    "writer"))

(defn writer
  [accumulator]
  (fn [v]
    (Writer. v
             accumulator)))

(defn write-writer
  [writer-factory val-to-write]
  (let [[_ a] (deref (writer-factory [nil]))]
    (Writer. nil
             (writer-m-add a val-to-write))))

(defn listen-writer
  [writer-mv]
  (let [[v a :as va] (deref writer-mv)]
    (Writer. va
             a)))

(defn censor-writer
  [f writer-mv]
  (let [[v a] (deref writer-mv)]
    (Writer. v
             (f a))))

(extend-type java.lang.String
  MonadWriter
  (writer-m-empty [_] "")
  (writer-m-add [c v] (str c v))
  (writer-m-combine [c1 c2] (str c1 c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Monadic functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do
  "Monad comprehension. Takes the name of a monadic value factory
   function (like vector, hash-set, maybe), a vector of steps given as
   binding-form/monadic-expression pairs, and a result value specified
   by expr. The monadic-expression terms can use the binding variables
   of the previous steps.

   If the monad contains a definition of monadic zero, the step list can
   also contain conditions of the form :when p, where the predicate p
   can contain the binding variables from all previous steps.

   A clause of the form :let [binding-form expr ...], where the bindings
   are given as a vector as for the use in let, establishes additional
   bindings that can be used in the following steps. "
  [mv-factory bindings expr]
  (let [steps (partition 2 bindings)]
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

(defn plus [[mv & mvs]]
  (plus-step mv mvs))

(defmacro plus*
  "Lazy variant of plus. Implemented as a macro to avoid eager
  argument evaluation. Takes one argument, a sequence of monadic values,
  which must be expressed as a vector literal. Each member of the
  argument-sequence is wrapped in a thunk, which is later evaluated by
  protocol method mondas.core/plus-step*."
  [[mv & mvs]]
  `(plus-step* ~mv (list ~@(map* (fn [m] `(fn thunk [] ~m)) mvs))))

(defn comprehend [f mvs]
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
     (assert
      (seq* mvs)
      (str
       "At least one monadic value is required by monads.core/seq when "
       "no monadic value factory function has been specified."))
     (seq (first mvs) mvs))
  ([mv-factory mvs]
     (if (seq* mvs)
       (comprehend (partial apply list) mvs)
       (do-result (mv-factory [nil]) (list)))))

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
  (seq (map* f xs)))

(defn chain
  "Chains together monadic computation steps that are each functions
   of one parameter. Each step is called with the result of the previous
   step as its argument. (monads.core/chain (step1 step2)) is equivalent
   to (fn [x] (m/do <mv-factory> [r1 (step1 x) r2 (step2 r1)] r2))."
  [steps]
  (fn [x]
    (let [mv ((first steps) x)
          chain (reduce (fn [chain step]
                          (fn [x]
                            (bind (step x) chain)))
                        (partial do-result mv)
                        (reverse (rest steps)))]
      (bind mv chain))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  List transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which
;; the base values are lists.

(deftype ListTransformer [do-result-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str ListTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash do-result-m)
             (hash v)))
  (equals [this that]
    (and (= ListTransformer (class that))
         (and (= (.do-result-m that) do-result-m)
              (= (.v that) v))))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (ListTransformer. do-result-m
                      (do-result-m (list v))))
  (bind [mv f]
    (let [v (deref mv)]
      (ListTransformer. do-result-m
                        (bind v (fn [xs]
                                  (if (seq* xs)
                                    (->> xs
                                         (map (comp deref (wrap-check mv f)))
                                         (fmap plus))
                                    (do-result-m (list))))))))
  MonadZero
  (zero [_]
    (ListTransformer. do-result-m
                      (do-result-m (list))))
  (plus-step [mv mvs]
    (ListTransformer. do-result-m
                      (apply
                       (lift (fn [& vs] (plus vs)))
                       (map* deref (cons mv mvs)))))

  MonadDev
  (val-types [_]
    [[ListTransformer]])
  (name-monad [_]
    "list-t"))

(defn list-t
  [mv-factory]
  (let [do-result-m (partial do-result (mv-factory [nil]))]
    (if (= mv-factory maybe)
      (fn [& vs]
        (ListTransformer. do-result-m
                          (do-result-m
                           (if-not (seq* vs)
                             (list)
                             (plus (fmap
                                    #(if (= (maybe %) Nothing)
                                       (list)
                                       (list %))
                                    (vec vs)))))))
      (fn [& vs]
        (ListTransformer. do-result-m
                          (do-result-m (apply list vs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Vector transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which
;; the base values are vectors.

(deftype VectorTransformer [do-result-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str VectorTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash do-result-m)
             (hash v)))
  (equals [this that]
    (and (= VectorTransformer (class that))
         (and (= (.do-result-m that) do-result-m)
              (= (.v that) v))))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (VectorTransformer. do-result-m
                        (do-result-m [v])))
  (bind [mv f]
    (let [v (deref mv)]
      (VectorTransformer. do-result-m
                          (bind v (fn [xs]
                                    (if (seq* xs)
                                      (->> xs
                                           (map (comp deref (wrap-check mv f)))
                                           (fmap plus))
                                      (do-result-m [])))))))

  MonadZero
  (zero [_]
    (VectorTransformer. do-result-m
                        (do-result-m [])))
  (plus-step [mv mvs]
    (VectorTransformer. do-result-m
                        (apply
                         (lift (fn [& vs] (plus vs)))
                         (map* deref (cons mv mvs)))))

  MonadDev
  (val-types [_]
    [[VectorTransformer]])
  (name-monad [_]
    "vector-t"))

(defn vector-t
  [mv-factory]
  (let [do-result-m (partial do-result (mv-factory [nil]))]
    (if (= mv-factory maybe)
      (fn [& vs]
        (VectorTransformer. do-result-m
                            (do-result-m
                             (if-not (seq* vs)
                               []
                               (plus (fmap
                                      #(if (= (maybe %) Nothing)
                                         []
                                         [%])
                                      (vec vs)))))))
      (fn [& vs]
        (VectorTransformer. do-result-m
                            (do-result-m (vec vs)))))))

(def vec-t vector-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LazySeq transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which
;; the base values are lazy sequences.

(deftype LazySeqTransformer [do-result-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str LazySeqTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash do-result-m)
             (hash v)))
  (equals [this that]
    (and (= LazySeqTransformer (class that))
         (and (= (.do-result-m that) do-result-m)
              (= (.v that) v))))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (LazySeqTransformer. do-result-m
                         (do-result-m (lazy-seq* v))))
  (bind [mv f]
    (let [v (deref mv)]
      (LazySeqTransformer. do-result-m
                           (bind v (fn [xs]
                                     (if (seq* xs)
                                       (->> xs
                                            (map (comp deref (wrap-check mv f)))
                                            (fmap plus))
                                       (do-result-m (lazy-seq))))))))

  MonadZero
  (zero [_]
    (LazySeqTransformer. do-result-m
                         (do-result-m (lazy-seq))))
  (plus-step [mv mvs]
    (LazySeqTransformer. do-result-m
                         (apply
                          (lift (fn [& vs] (plus vs)))
                          (map* deref (cons mv mvs)))))

  MonadDev
  (val-types [_]
    [[LazySeqTransformer]])
  (name-monad [_]
    "lazy-seq-t"))

(defn lazy-seq-t
  [mv-factory]
  (let [do-result-m (partial do-result (mv-factory [nil]))]
    (if (= mv-factory maybe)
      (fn [& vs]
        (LazySeqTransformer. do-result-m
                             (do-result-m
                              (if-not (seq* vs)
                                (lazy-seq)
                                (plus (fmap
                                       #(if (= (maybe %) Nothing)
                                          (lazy-seq)
                                          (lazy-seq* %))
                                       (vec vs)))))))
      (fn [& vs]
        (LazySeqTransformer. do-result-m
                             (do-result-m (lazy-seq vs)))))))

(def lazy-t lazy-seq-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Set transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which
;; the base values are sets.

(deftype SetTransformer [do-result-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str SetTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash do-result-m)
             (hash v)))
  (equals [this that]
    (and (= SetTransformer (class that))
         (and (= (.do-result-m that) do-result-m)
              (= (.v that) v))))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (SetTransformer. do-result-m
                     (do-result-m #{v})))
  (bind [mv f]
    (let [v (deref mv)]
      (SetTransformer. do-result-m
                       (bind v (fn [xs]
                                 (if (seq* xs)
                                   (->> xs
                                        (map (comp deref (wrap-check mv f)))
                                        (fmap plus))
                                   (do-result-m #{})))))))

  MonadZero
  (zero [_]
    (SetTransformer. do-result-m
                     (do-result-m #{})))
  (plus-step [mv mvs]
    (SetTransformer. do-result-m
                     (apply
                      (lift (fn [& vs] (plus vs)))
                      (map* deref (cons mv mvs)))))

  MonadDev
  (val-types [_]
    [[SetTransformer]])
  (name-monad [_]
    "set-t"))

(defn set-t
  [mv-factory]
  (let [do-result-m (partial do-result (mv-factory [nil]))]
    (if (= mv-factory maybe)
      (fn [& vs]
        (SetTransformer. do-result-m
                         (do-result-m
                          (if-not (seq* vs)
                            #{}
                            (plus (fmap
                                   #(if (= (maybe %) Nothing)
                                      #{}
                                      #{%})
                                   (vec vs)))))))
      (fn [& vs]
        (SetTransformer. do-result-m
                         (do-result-m (apply hash-set vs)))))))

(def hash-set-t set-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Maybe transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which the
;; base values can be invalid (represented by monads.core/Nothing).

(deftype MaybeTransformer [do-result-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str MaybeTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash do-result-m)
             (hash v)))
  (equals [this that]
    (and (= MaybeTransformer (class that))
         (and (= (.do-result-m that) do-result-m)
              (= (.v that) v))))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (MaybeTransformer. do-result-m
                       (do-result-m (Maybe. v))))
  (bind [mv f]
    (let [v (deref mv)]
      (MaybeTransformer. do-result-m
                         (let [bres (bind v (fn [x]
                                              (if (= x Nothing)
                                                (do-result-m Nothing)
                                                (deref ((wrap-check mv f) (deref x))))))]
                           (if-not (coll? bres)
                             bres
                             (if-let [filt (seq* (filter #(not= % Nothing) bres))]
                               (into (zero bres) filt)
                               (do-result-m Nothing)))))))

  MonadZero
  (zero [_]
    (MaybeTransformer. do-result-m
                       (do-result-m Nothing)))
  (plus-step [mv mvs]
    (MaybeTransformer. do-result-m
                       (bind (deref mv)
                             (fn [x]
                               (cond
                                 (and (= x Nothing) (empty? mvs))
                                 (do-result-m Nothing)

                                 (= x Nothing)
                                 (deref (plus mvs))

                                 :else
                                 (do-result-m x))))))

  MonadDev
  (val-types [_]
    [[MaybeTransformer]])
  (name-monad [_]
    "maybe-t"))

(defn maybe-t
  [mv-factory]
  (let [do-result-m (partial do-result (mv-factory [nil]))]
    (fn [& vs]
      (MaybeTransformer. do-result-m
                         (apply mv-factory
                                (if-not (seq* vs)
                                  [Nothing]
                                  (or (seq* (plus (fmap
                                                   #(let [v (maybe %)]
                                                      (if-not (= v Nothing)
                                                        [v]
                                                        []))
                                                   (vec vs))))
                                      [Nothing])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  State transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad of stateful
;; computations that have the base monad type as their result.

(deftype StateTransformer [do-result-m v mv f alts lzalts]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str StateTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash do-result-m)
             (hash v)
             (hash mv)
             (hash f)
             (hash alts)
             (hash lzalts)))
  (equals [this that]
    (and (= StateTransformer (class that))
         (and (= (.do-result-m that) do-result-m)
              (= (.v that) v)
              (= (.mv that) mv)
              (= (.f that) f)
              (= (.alts that) alts)
              (= (.lzalts that) lzalts))))

  clojure.lang.IFn
  (invoke [_ s]
    (cond
      ;; Use of doall "enforces" the non-laziness of regular plus /
      ;; plus-step. Otherwise you can end up with monadic computations
      ;; that make use of plus / plus-step which sometimes are lazy
      ;; enough and sometimes aren't (apparently owing to sequence
      ;; chunking).  If laziness is desirable, then one should use
      ;; plus* / plus-step*.
      alts (plus-step ((first alts) s)
                      (doall (map* #(% s) (second alts))))
      lzalts (plus-step* ((first lzalts) s)
                         (map* #(fn [] ((%) s)) (second lzalts)))
      f (bind (mv s)
              (fn [[v ss]]
                ((f v) ss)))
      :else (if (= v (zero (do-result-m [nil])))
              v
              (do-result-m [v s]))))

  Monad
  (do-result [_ v]
    (StateTransformer. do-result-m
                       v
                       nil
                       nil
                       nil
                       nil))
  (bind [mv f]
    (StateTransformer. do-result-m
                       nil
                       mv
                       (wrap-check mv f)
                       nil
                       nil))

  MonadZero
  (zero [_]
    (StateTransformer. do-result-m
                       nil
                       (fn [s] (zero (do-result-m [nil])))
                       (fn [v]
                         (StateTransformer. do-result-m
                                            v
                                            nil
                                            nil
                                            nil
                                            nil))
                       nil
                       nil))
  (plus-step [mv mvs]
    (StateTransformer. do-result-m
                       nil
                       nil
                       nil
                       (list mv mvs)
                       nil))
  (plus-step* [mv mvs]
    (StateTransformer. do-result-m
                       nil
                       nil
                       nil
                       nil
                       (list mv mvs)))

  MonadDev
  (val-types [_]
    [[monads.core.IState]])
  (name-monad [_]
    "state-t")

  IState
  (i-state [_]))

(defn state-t
  [mv-factory]
  (let [do-result-m (partial do-result (mv-factory [nil]))]
    (if (= mv-factory maybe)
      (fn
        ([v]
           (let [v (if (= *Nothing* v) Nothing v)]
             (StateTransformer. do-result-m
                                v
                                nil
                                nil
                                nil
                                nil)))
        ([mv f]
           (StateTransformer. do-result-m
                              nil
                              mv
                              f
                              nil
                              nil)))
      (fn
        ([v]
           (StateTransformer. do-result-m
                              v
                              nil
                              nil
                              nil
                              nil))
        ([mv f]
           (StateTransformer. do-result-m
                              nil
                              mv
                              f
                              nil
                              nil))))))

(defn update-state-t
  "Return a function that returns a StateTransformer monad value (for
   the monad specified by mv-factory) that replaces the current state by
   the result of f applied to the current state and that returns the old
   state."
  [mv-factory]
  (let [do-result-m (partial do-result (mv-factory [nil]))]
    (fn [f]
      (reify
        clojure.lang.IHashEq
        (hasheq [this]
          (bit-xor (hash (str (class this)))
                   (.hashCode this)))
        (hashCode [_]
          (bit-xor (hash do-result-m)
                   (hash f)))
        (equals [this that]
          (and (= (class this) (class that))
               (= (.hashCode this)
                  (.hashCode that))))

        clojure.lang.IFn
        (invoke [_ s]
          (do-result-m [s (f s)]))

        Monad
        (do-result [_ v]
          (StateTransformer. do-result-m
                             v
                             nil
                             nil
                             nil
                             nil))
        (bind [mv f]
          (StateTransformer. do-result-m
                             nil
                             mv
                             (wrap-check mv f)
                             nil
                             nil))

        MonadZero
        (zero [_]
          (StateTransformer. do-result-m
                             nil
                             (fn [s] (zero (do-result-m [nil])))
                             (fn [v]
                               (StateTransformer. do-result-m
                                                  v
                                                  nil
                                                  nil
                                                  nil
                                                  nil))
                             nil
                             nil))
        (plus-step [mv mvs]
          (StateTransformer. do-result-m
                             nil
                             nil
                             nil
                             (list mv mvs)
                             nil))
        (plus-step* [mv mvs]
          (StateTransformer. do-result-m
                             nil
                             nil
                             nil
                             nil (list mv mvs)))

        MonadDev
        (val-types [_]
          [[monads.core.IState]])
        (name-monad [_]
          "state-t")

        IState
        (i-state [_])))))

(defn set-state-t
  "Return a function that returns a StateTransformer monad value (for
   the monad specified by mv-factory) that replaces the current state by
   s and returns the previous state."
  [mv-factory]
  (let [u (update-state-t mv-factory)]
    (fn [s]
      (u (constantly s)))))

(defn get-state-t
  "Return a function that returns a StateTransformer monad value (for
   the monad specified by mv-factory) that returns the current state
   and does not modify it."
  [mv-factory]
  (let [u (update-state-t mv-factory)]
    (fn []
      (u identity))))

(defn get-state-t-val
  "Return a function that returns a StateTransformer monad value (for
   the monad specified by mv-factory) that assumes the state to be a map
   and returns the value corresponding to the given key. The state is
   not modified."
  [mv-factory]
  (let [g (get-state-t mv-factory)
        do-result-m-state (partial do-result ((state-t mv-factory) [nil]))]
    (fn [key]
      (bind (g)
            #(do-result-m-state (get % key))))))

(defn update-state-t-val
  "Return a function that returns a StateTransformer monad value (for
   the monad specified by mv-factory) that assumes the state to be a map
   and replaces the value associated with the given key by the return
   value of f applied to the old value and args. The old value is
   returned."
  [mv-factory]
  (let [u (update-state-t mv-factory)
        do-result-m-state (partial do-result ((state-t mv-factory) [nil]))]
    (fn [key f & args]
      (bind (u #(apply update-in % [key] f args))
            #(do-result-m-state (get % key))))))

(defn set-state-t-val
  "Return a function that returns a StateTransformer monad value (for
   the monad specified by mv-factory) that assumes the state to be a map
   and replaces the value associated with key by val. The old value is
   returned."
  [mv-factory]
  (let [uv (update-state-t-val mv-factory)]
    (fn [key val]
      (uv key (constantly val)))))

(defn get-in-state-t-val
  [mv-factory]
  (let [g (get-state-t mv-factory)
        do-result-m-state (partial do-result ((state-t mv-factory) [nil]))]
    (fn [path & [default]]
      (bind (g)
            #(do-result-m-state (get-in % path default))))))

(defn assoc-in-state-t-val
  [mv-factory]
  (let [u (update-state-t mv-factory)
        do-result-m-state (partial do-result ((state-t mv-factory) [nil]))]
    (fn [path val]
      (bind (u #(assoc-in % path val))
            #(do-result-m-state (get-in % path))))))

(defn update-in-state-t-val
  [mv-factory]
  (let [u (update-state-t mv-factory)
        do-result-m-state (partial do-result ((state-t mv-factory) [nil]))]
    (fn [path f & args]
      (bind (u #(apply update-in % path f args))
            #(do-result-m-state (get-in % path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Writer transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype WriterTransformer [do-result-m mv writer-m]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str WriterTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash do-result-m)
             (hash mv)
             (hash writer-m)))
  (equals [this that]
    (and (= WriterTransformer (class that))
         (and (= (.do-result-m that) do-result-m)
              (= (.mv that) mv)
              (= (.writer-m that) writer-m))))

  clojure.lang.IDeref
  (deref [_]
    mv)

  Monad
  (do-result [_ v]
    (WriterTransformer. do-result-m
                        (do-result-m (writer-m v))
                        writer-m))
  (bind [mv f]
    (WriterTransformer. do-result-m
                        (bind (deref mv)
                              (fn [v]
                                (let [[v1 a1] (deref v)]
                                  (bind (deref ((wrap-check mv f) v1))
                                        (fn [v]
                                          (let [[v2 a2] (deref v)]
                                            (do-result-m (Writer. v2
                                                                  (writer-m-combine a1 a2)))))))))
                        writer-m))

  MonadZero
  (zero [mv]
    (let [v (deref mv)]
      (WriterTransformer. do-result-m
                          (zero v)
                          writer-m)))
  (plus-step [mv mvs]
    (WriterTransformer. do-result-m
                        (plus (map* deref (cons mv mvs)))
                        writer-m))

  MonadDev
  (val-types [_]
    [[WriterTransformer]])
  (name-monad [_]
    "writer-t"))

(defn writer-t [mv-factory accumulator]
  (let [do-result-m (partial do-result (mv-factory [nil]))
        writer-m (writer accumulator)]
    (if (= mv-factory maybe)
      (fn [v]
        (let [mv (if (= *Nothing* v)
                   Nothing
                   (do-result-m (writer-m v)))]
          (WriterTransformer. do-result-m
                              mv
                              writer-m)))
      (fn [v]
        (let [mv (do-result-m (writer-m v))]
          (WriterTransformer. do-result-m
                              mv
                              writer-m))))))

(defn write-writer-t
  []
  (fn [writer-factory val-to-write]
    (let [[_ a] (deref (writer-factory [nil]))]
      (Writer. nil
               (writer-m-add a val-to-write)))))

(defn listen-writer-t
  []
  (fn [writer-mv]
    (let [[v a :as va] (deref writer-mv)]
      (Writer. va
               a))))

(defn censor-writer-t
  []
  (fn [f writer-mv]
    (let [[v a] (deref writer-mv)]
      (Writer. v
               (f a)))))
