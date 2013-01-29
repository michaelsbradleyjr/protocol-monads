(ns monads.core
  (:refer-clojure :exclude [do seq map reduce list vector vec lazy-seq hash-set set])
  (:require [clojure.set :as cset]
            [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Protocols
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Monad
  (bind [mv f])
  (return [_ v]))

(defprotocol MonadDev
  "Used in conjunction with the type checker."
  (types [_]))

(defprotocol MonadTransformer
  (default-plus [_])
  (default-return-m [_]))

(defprotocol MonadWriter
  "Accumulation of values into containers."
  (writer-m-add [container value]
    "add value to container, return new container")
  (writer-m-combine [container1 container2]
    "combine two containers, return new container")
  (writer-m-empty [_]
    "return an empty container"))

(defprotocol MonadZero
  (plus' [mv mvs])
  (plus'* [mv mvs])
  (zero [_]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Factory functions for clojure.core types, namespaced in monads.core
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def hash-set clojure.core/hash-set)

(def ^{:macro true} lazy-seq #'clojure.core/lazy-seq)

(def list clojure.core/list)

(defmacro lazy-seq*
  [& vs]
  `(lazy-seq (list ~@vs)))

(def set clojure.core/set)

(def vec clojure.core/vec)

(def vector clojure.core/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private map* clojure.core/map)

(def ^:private reduce* clojure.core/reduce)

(def ^:private seq* clojure.core/seq)

(defn- lazy-concat
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

(def ^:dynamic *check-types* false)

(declare mismatch-message)

(defn- check-return-type [mv f check-types]
  (fn [v]
    (let [return-val (f v)
          mv-types (types mv)]
      (if-not (seq* mv-types)
        return-val
        (let []
          (assert (some identity
                        (map* (fn [icps]
                                (every? (fn [icp]
                                          (instance? icp return-val))
                                        icps))
                              mv-types))
                  (mismatch-message mv mv-types return-val))
          return-val)))))

(defn- wrap-check [mv f]
  (if-not *check-types*
    f
    (check-return-type mv f *check-types*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The "dummy value" [::dummy] is used in several expressions below to
;; couple calls to bind, zero and return to a particular
;; protocol-monad, as determined by the return type of the monadic
;; value factory function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private dummy [::dummy])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentList monad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad describing multi-valued computations, i.e. computations that
;; can yield multiple values as a list of those values.

(extend-type clojure.lang.PersistentList
  Monad
  (return [_ v]
    (list v))
  (bind [mv f]
    (apply list (mapcat (wrap-check mv f) mv)))

  MonadZero
  (zero [_]
    (list))
  (plus' [mv mvs]
    (apply list (apply concat mv mvs)))
  (plus'* [mv mvs]
    (apply list (apply concat mv (map* #(%) mvs))))

  MonadDev
  (types [_]
    [[java.util.List]])

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
  (return [_ v]
    (list v))
  (bind [mv f]
    (apply list (mapcat (wrap-check mv f) mv)))

  MonadZero
  (zero [_]
    (list))
  (plus' [mv mvs]
    (apply list (apply concat mv mvs)))
  (plus'* [mv mvs]
    (apply list (apply concat mv (map* #(%) mvs))))

  MonadDev
  (types [_]
    [[java.util.List]])

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
  (return [_ v]
    [v])
  (bind [mv f]
    (vec (mapcat (wrap-check mv f) mv)))

  MonadZero
  (zero [_]
    [])
  (plus' [mv mvs]
    (vec (apply concat mv mvs)))
  (plus'* [mv mvs]
    (vec (apply concat mv (map* #(%) mvs))))

  MonadDev
  (types [_]
    [[java.util.List]])

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
  (return [_ v]
    (lazy-seq* v))
  (bind [mv f]
    (lazy-seq (mapcat (wrap-check mv f) mv)))

  MonadZero
  (zero [_]
    (lazy-seq))
  (plus' [mv mvs]
    (lazy-concat mv mvs))
  (plus'* [mv mvs]
    (lazy-concat mv (map* #(%) mvs)))

  MonadDev
  (types [_]
    [[java.util.List]])

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
  (return [_ v]
    #{v})
  (bind [mv f]
    (set (apply cset/union (map* (wrap-check mv f) mv))))

  MonadZero
  (zero [_]
    #{})
  (plus' [mv mvs]
    (apply cset/union mv mvs))
  (plus'* [mv mvs]
    (apply cset/union mv (map* #(%) mvs)))

  MonadDev
  (types [_]
    [[java.util.Set]])

  MonadWriter
  (writer-m-empty [_] #{})
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (cset/union c1 c2)))

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

(deftype Maybe [factory v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str Maybe))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash factory)
             (hash v)))
  (equals [this that]
    (and (= Maybe (class that))
         (= (.factory that) factory)
         (= (.v that) v)))

  clojure.lang.IDeref
  (deref [mv]
    (if (= Nothing mv)
      *Nothing*
      v))

  Monad
  (return [_ v]
    (factory v))
  (bind [mv f]
    (if (= mv Nothing)
      Nothing
      ((wrap-check mv f) (deref mv))))

  MonadZero
  (zero [_]
    Nothing)
  (plus' [mv mvs]
    (let [mv (->> (cons mv mvs)
                  (drop-while #(= Nothing %))
                  first)]
      (if (nil? mv)
        Nothing
        mv)))
  (plus'* [mv mvs]
    (if-not (= Nothing mv)
      mv
      (let [mv (->> mvs
                    (drop-while #(= Nothing (%)))
                    first)]
        (if (nil? mv)
          Nothing
          (mv)))))

  MonadDev
  (types [_]
    [[Maybe]]))

(defn- maybe*
  [v]
  (Maybe. maybe* v))

(defn maybe
  [v]
  (if (= *Nothing* v)
    Nothing
    (Maybe. maybe* v)))

(def Nothing (Maybe. maybe* ::Nothing))

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
;; effective structure (fn [old-state] [result new-state]).

(deftype State [factory v mv f]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str State))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash factory)
             (hash v)
             (hash mv)
             (hash f)))
  (equals [this that]
    (and (= State (class that))
         (= (.factory that) factory)
         (= (.v that) v)
         (= (.mv that) mv)
         (= (.f that) f)))

  clojure.lang.IFn
  (invoke [_ s]
    (if f
      (let [[v ss] (mv s)]
        ((f v) ss))
      [v s]))

  Monad
  (return [_ v]
    (factory v))
  (bind [mv f]
    (factory mv f))

  MonadDev
  (types [_]
    [[State]]))

(declare state-mv-dummy)

(defn state
  ([v]
     (State. state
             v
             nil
             nil))
  ([mv f]
     (State. state
             nil
             mv
             (wrap-check state-mv-dummy f)))
  ([mv f & fs]
     (let [f* (first fs)]
       (if-let [fs (seq* (rest fs))]
         (apply state (concat [(state mv f) f*] fs))
         (state (state mv f) f*)))))

(def ^:private state-mv-dummy (state dummy))

(defn update-state
  [f]
  (state (fn [s] [s (f s)])
         state))

(defn set-state
  "Return a State monad value that replaces the current state by s and
  returns the previous state."
  [s]
  (update-state (constantly s)))

(defn get-state
  "Return a State monad value that returns the current state and does
  not modify it."
  []
  (update-state identity))

(defn get-state-val
  "Return a State monad value that assumes the state to be a map and
  returns the value corresponding to the given key. The state is not
  modified."
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
  replaces the value associated with key by val. The old value is
  returned."
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

(deftype Continuation [factory v mv f]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str Continuation))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash factory)
             (hash v)
             (hash mv)
             (hash f)))
  (equals [this that]
    (and (= Continuation (class that))
         (= (.factory that) factory)
         (= (.v that) v)
         (= (.mv that) mv)
         (= (.f that) f)))

  clojure.lang.IDeref
  (deref [mv]
    (mv identity))

  clojure.lang.IFn
  (invoke [_ c]
    (if f
      (mv (fn [v] ((f v) c)))
      (c v)))

  Monad
  (return [_ v]
    (factory v))
  (bind [mv f]
    (factory mv f))

  MonadDev
  (types [_]
    [[Continuation]]))

(declare cont-mv-dummy)

(defn cont
  ([v]
     (Continuation. cont
                    v
                    nil
                    nil))
  ([mv f]
     (Continuation. cont
                    nil
                    mv
                    (wrap-check cont-mv-dummy f)))
  ([mv f & fs]
     (let [f* (first fs)]
       (if-let [fs (seq* (rest fs))]
         (apply cont (concat [(cont mv f) f*] fs))
         (cont (cont mv f) f*)))))

(def ^:private cont-mv-dummy (cont dummy))

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
         (= (.v that) v)
         (= (.accumulator that) accumulator)))

  clojure.lang.IDeref
  (deref [_]
    [v accumulator])

  Monad
  (return [_ v]
    (Writer. v
             (writer-m-empty accumulator)))
  (bind [mv f]
    (let [[v1 a1] (deref mv)
          [v2 a2] (deref ((wrap-check mv f) v1))]
      (Writer. v2
               (writer-m-combine a1 a2))))

  MonadDev
  (types [_]
    [[Writer]]))

(defn writer
  [accumulator]
  (fn [v]
    (Writer. v
             accumulator)))

(extend-type java.lang.String
  MonadWriter
  (writer-m-empty [_] "")
  (writer-m-add [c v] (str c v))
  (writer-m-combine [c1 c2] (str c1 c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Monad utilities
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
  (let [steps (partition 2 bindings)
        mv-dummy (gensym "mv-dummy")]
    `(let [~mv-dummy (~mv-factory ~dummy)]
       (monads.core/bind ~mv-dummy
                         (fn [_#]
                           ~(monads.core/reduce*
                             (fn [expr [sym mv]]
                               (cond
                                (= :when sym)
                                `(if ~mv
                                   ~expr
                                   (monads.core/zero ~mv-dummy))

                                (= :let sym)
                                `(let ~mv
                                   ~expr)

                                :else
                                `(monads.core/bind ~mv (fn [~sym] ~expr))))
                             `(monads.core/return ~mv-dummy ~expr)
                             (reverse steps)))))))

(defn plus [[mv & mvs]]
  (plus' mv mvs))

(defmacro plus*
  "Lazy variant of plus. Implemented as a macro to avoid eager
  argument evaluation. Takes one argument, a sequence of monadic values,
  which must be expressed as a vector literal. Each member of the
  argument-sequence is wrapped in a thunk, which is later evaluated by
  protocol method mondas.core/plus'*."
  [[mv & mvs]]
  `(monads.core/plus'* ~mv (list ~@(map* (fn [m] `(fn thunk [] ~m)) mvs))))

(defn- comprehend [f mvs]
  (let [mv (first mvs)
        rest-steps (reduce* (fn [steps mv]
                              (fn [acc x]
                                (bind mv (partial steps (conj acc x)))))
                            (fn [acc x]
                              (return mv (f (conj acc x))))
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
       (return (mv-factory dummy) (list)))))

(defn lift
  "Converts a function f to a function of monadic arguments returning a
  monadic value."
  [f]
  (fn [& mvs]
    (comprehend (partial apply f) mvs)))

(defn reduce
  "Return the reduction of (monads.core/lift f) over the list of monadic
  values mvs, with optional initial value
  (monads.core/return (first mvs) val)."
  ([f mvs]
     (assert
      (seq* mvs)
      "At least one monadic value is required by monads.core/reduce.")
     (let [lf (lift f)]
       (reduce* lf mvs)))
  ([f val mvs]
     (assert
      (seq* mvs)
      "At least one monadic value is required by monads.core/reduce.")
     (let [lf (lift f)
           mv (return (first mvs) val)]
       (reduce* lf mv mvs))))

(defn join
  "Converts a monadic value containing a monadic value into a 'simple'
  monadic value."
  [mv]
  (bind mv identity))

(defn fmap
  "Bind the monadic value mv to the function f. Returning (f x) for
  argument x"
  [f mv]
  (bind mv (fn [x] (return mv (f x)))))

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
          chain (reduce* (fn [chain step]
                           (fn [x]
                             (bind (step x) chain)))
                         (partial return mv)
                         (reverse (rest steps)))]
      (bind mv chain))))

(defn- mismatch-message
  [mv mv-types return-val]
  (str
   "Type mismatch between bound monadic value "
   mv
   " and monadic function return value "
   return-val
   ". Function returned type "
   (#(second (string/split (str %) #" ")) (class return-val))
   ". The protocol-monad for "
   (class mv)
   " specifies: "
   (string/join " or "
                (fmap (fn [v]
                        (let [cnt (count v)
                              als (string/join " and " v)]
                          (if (> cnt 1)
                            (str "(" als ")")
                            als)))
                      mv-types))
   "."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  List transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which
;; the base values are lists.

(deftype ListTransformer [return-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str ListTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash return-m)
             (hash v)))
  (equals [this that]
    (and (= ListTransformer (class that))
         (= (.return-m that) return-m)
         (= (.v that) v)))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (return [_ v]
    (ListTransformer. return-m
                      (return-m (list v))))
  (bind [mv f]
    (let [v (deref mv)]
      (ListTransformer. return-m
                        (bind v (fn [xs]
                                  (if (seq* xs)
                                    (->> xs
                                         (map (comp deref (wrap-check mv f)))
                                         (fmap plus))
                                    (return-m (list))))))))
  MonadZero
  (zero [_]
    (ListTransformer. return-m
                      (return-m (list))))
  (plus' [mv mvs]
    (ListTransformer. return-m
                      (apply
                       (lift (fn [& vs] (plus vs)))
                       (map* deref (cons mv mvs)))))
  (plus'* [mv mvs]
    (ListTransformer. return-m
                      (apply
                       (lift (fn [& vs] (plus vs)))
                       (map* deref (cons mv (map* #(%) mvs))))))

  MonadDev
  (types [_]
    [[ListTransformer]]))

(defn list-t
  [mv-factory]
  (let [return-m (partial return (mv-factory dummy))]
    (if (= mv-factory maybe)
      (fn [& vs]
        (ListTransformer. return-m
                          (return-m
                           (if-not (seq* vs)
                             (list)
                             (plus (fmap
                                    #(if (= (maybe %) Nothing)
                                       (list)
                                       (list %))
                                    (vec vs)))))))
      (fn [& vs]
        (ListTransformer. return-m
                          (return-m (apply list vs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Vector transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which
;; the base values are vectors.

(deftype VectorTransformer [return-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str VectorTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash return-m)
             (hash v)))
  (equals [this that]
    (and (= VectorTransformer (class that))
         (= (.return-m that) return-m)
         (= (.v that) v)))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (return [_ v]
    (VectorTransformer. return-m
                        (return-m [v])))
  (bind [mv f]
    (let [v (deref mv)]
      (VectorTransformer. return-m
                          (bind v (fn [xs]
                                    (if (seq* xs)
                                      (->> xs
                                           (map (comp deref (wrap-check mv f)))
                                           (fmap plus))
                                      (return-m [])))))))

  MonadZero
  (zero [_]
    (VectorTransformer. return-m
                        (return-m [])))
  (plus' [mv mvs]
    (VectorTransformer. return-m
                        (apply
                         (lift (fn [& vs] (plus vs)))
                         (map* deref (cons mv mvs)))))
  (plus'* [mv mvs]
    (VectorTransformer. return-m
                        (apply
                         (lift (fn [& vs] (plus vs)))
                         (map* deref (cons mv (map* #(%) mvs))))))

  MonadDev
  (types [_]
    [[VectorTransformer]]))

(defn vector-t
  [mv-factory]
  (let [return-m (partial return (mv-factory dummy))]
    (if (= mv-factory maybe)
      (fn [& vs]
        (VectorTransformer. return-m
                            (return-m
                             (if-not (seq* vs)
                               []
                               (plus (fmap
                                      #(if (= (maybe %) Nothing)
                                         []
                                         [%])
                                      (vec vs)))))))
      (fn [& vs]
        (VectorTransformer. return-m
                            (return-m (vec vs)))))))

(def vec-t vector-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LazySeq transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which
;; the base values are lazy sequences.

(deftype LazySeqTransformer [return-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str LazySeqTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash return-m)
             (hash v)))
  (equals [this that]
    (and (= LazySeqTransformer (class that))
         (= (.return-m that) return-m)
         (= (.v that) v)))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (return [_ v]
    (LazySeqTransformer. return-m
                         (return-m (lazy-seq* v))))
  (bind [mv f]
    (let [v (deref mv)]
      (LazySeqTransformer. return-m
                           (bind v (fn [xs]
                                     (if (seq* xs)
                                       (->> xs
                                            (map (comp deref (wrap-check mv f)))
                                            (fmap plus))
                                       (return-m (lazy-seq))))))))

  MonadZero
  (zero [_]
    (LazySeqTransformer. return-m
                         (return-m (lazy-seq))))
  (plus' [mv mvs]
    (LazySeqTransformer. return-m
                         (apply
                          (lift (fn [& vs] (plus vs)))
                          (map* deref (cons mv mvs)))))
  (plus'* [mv mvs]
    (LazySeqTransformer. return-m
                         (apply
                          (lift (fn [& vs] (plus vs)))
                          (map* deref (cons mv (map* #(%) mvs))))))

  MonadDev
  (types [_]
    [[LazySeqTransformer]]))

(defn lazy-seq-t
  [mv-factory]
  (let [return-m (partial return (mv-factory dummy))]
    (if (= mv-factory maybe)
      (fn [& vs]
        (LazySeqTransformer. return-m
                             (return-m
                              (if-not (seq* vs)
                                (lazy-seq)
                                (plus (fmap
                                       #(if (= (maybe %) Nothing)
                                          (lazy-seq)
                                          (lazy-seq* %))
                                       (vec vs)))))))
      (fn [& vs]
        (LazySeqTransformer. return-m
                             (return-m (lazy-seq vs)))))))

(def lazy-t lazy-seq-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Set transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which
;; the base values are sets.

(deftype SetTransformer [return-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str SetTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash return-m)
             (hash v)))
  (equals [this that]
    (and (= SetTransformer (class that))
         (= (.return-m that) return-m)
         (= (.v that) v)))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (return [_ v]
    (SetTransformer. return-m
                     (return-m #{v})))
  (bind [mv f]
    (let [v (deref mv)]
      (SetTransformer. return-m
                       (bind v (fn [xs]
                                 (if (seq* xs)
                                   (->> xs
                                        (map (comp deref (wrap-check mv f)))
                                        (fmap plus))
                                   (return-m #{})))))))

  MonadZero
  (zero [_]
    (SetTransformer. return-m
                     (return-m #{})))
  (plus' [mv mvs]
    (SetTransformer. return-m
                     (apply
                      (lift (fn [& vs] (plus vs)))
                      (map* deref (cons mv mvs)))))
  (plus'* [mv mvs]
    (SetTransformer. return-m
                     (apply
                      (lift (fn [& vs] (plus vs)))
                      (map* deref (cons mv (map* #(%) mvs))))))

  MonadDev
  (types [_]
    [[SetTransformer]]))

(defn set-t
  [mv-factory]
  (let [return-m (partial return (mv-factory dummy))]
    (if (= mv-factory maybe)
      (fn [& vs]
        (SetTransformer. return-m
                         (return-m
                          (if-not (seq* vs)
                            #{}
                            (plus (fmap
                                   #(if (= (maybe %) Nothing)
                                      #{}
                                      #{%})
                                   (vec vs)))))))
      (fn [& vs]
        (SetTransformer. return-m
                         (return-m (apply hash-set vs)))))))

(def hash-set-t set-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Maybe transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monad transformer that transforms a monad into a monad in which the
;; base values can be invalid (represented by monads.core/Nothing).

(deftype MaybeTransformer [return-m v]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str MaybeTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash return-m)
             (hash v)))
  (equals [this that]
    (and (= MaybeTransformer (class that))
         (= (.return-m that) return-m)
         (= (.v that) v)))

  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (return [_ v]
    (MaybeTransformer. return-m
                       (return-m (maybe* v))))
  (bind [mv f]
    (let [v (deref mv)]
      (MaybeTransformer. return-m
                         (let [bres (bind v (fn [x]
                                              (if (= x Nothing)
                                                (return-m Nothing)
                                                (deref ((wrap-check mv f) (deref x))))))]
                           (if-not (coll? bres)
                             bres
                             (if-let [filt (seq* (filter #(not= % Nothing) bres))]
                               (plus (map* return-m filt))
                               (return-m Nothing)))))))

  MonadZero
  (zero [_]
    (MaybeTransformer. return-m
                       (return-m Nothing)))
  (plus' [mv mvs]
    (MaybeTransformer. return-m
                       (bind (deref mv)
                             (fn [x]
                               (cond
                                (and (= x Nothing) (empty? mvs))
                                (return-m Nothing)

                                (= x Nothing)
                                (deref (plus mvs))

                                :else
                                (return-m x))))))
  (plus'* [mv mvs]
    (let [mv (fn thunk [] mv)]
      (MaybeTransformer. return-m
                         (bind (deref (mv))
                               (fn [x]
                                 (cond
                                  (and (= x Nothing) (empty? mvs))
                                  (return-m Nothing)

                                  (= x Nothing)
                                  (deref (plus mvs))

                                  :else
                                  (return-m x)))))))

  MonadDev
  (types [_]
    [[MaybeTransformer]]))

(defn maybe-t
  [mv-factory]
  (let [return-m (partial return (mv-factory dummy))]
    (fn [& vs]
      (MaybeTransformer. return-m
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

(deftype StateTransformer [return-m v mv f alts lzalts update? m-zero?]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str StateTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash return-m)
             (hash v)
             (hash mv)
             (hash f)
             (hash alts)
             (hash lzalts)
             (hash update?)
             (hash m-zero?)))
  (equals [this that]
    (and (= StateTransformer (class that))
         (= (.return-m that) return-m)
         (= (.v that) v)
         (= (.mv that) mv)
         (= (.f that) f)
         (= (.alts that) alts)
         (= (.lzalts that) lzalts)
         (= (.update? that) update?)
         (= (.m-zero? that) m-zero?)))

  clojure.lang.IFn
  (invoke [_ s]
    (cond
     alts (plus' ((first alts) s)
                 (map* #(% s) (second alts)))
     lzalts (plus'* ((first lzalts) s)
                    (map* #(fn [] ((%) s)) (second lzalts)))
     f (bind (mv s)
             (fn [[v ss]]
               ((f v) ss)))
     :else (if (and (not update?)
                    m-zero?
                    (= v (zero (return-m dummy))))
             v
             (return-m [v s]))))

  Monad
  (return [_ v]
    (StateTransformer. return-m
                       v
                       nil
                       nil
                       nil
                       nil
                       nil
                       m-zero?))
  (bind [mv f]
    (StateTransformer. return-m
                       nil
                       mv
                       (wrap-check mv f)
                       nil
                       nil
                       nil
                       m-zero?))

  MonadZero
  (zero [_]
    (StateTransformer. return-m
                       nil
                       (fn [s] (zero (return-m dummy)))
                       (fn [v]
                         (StateTransformer. return-m
                                            v
                                            nil
                                            nil
                                            nil
                                            nil
                                            nil
                                            m-zero?))
                       nil
                       nil
                       nil
                       m-zero?))
  (plus' [mv mvs]
    (StateTransformer. return-m
                       nil
                       nil
                       nil
                       (list mv mvs)
                       nil
                       nil
                       m-zero?))
  (plus'* [mv mvs]
    (StateTransformer. return-m
                       nil
                       nil
                       nil
                       nil
                       (list mv mvs)
                       nil
                       m-zero?))

  MonadTransformer
  (default-return-m [_]
    :return)

  MonadDev
  (types [_]
    [[StateTransformer]]))

(defn state-t
  ([mv-factory]
     (state-t mv-factory (default-return-m (StateTransformer. nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              nil))))
  ([mv-factory which-return-m]
     (let [mv-dummy (mv-factory dummy)
           return-m (case which-return-m
                      :return
                      (partial return mv-dummy)
                      :factory
                      mv-factory)
           maybe? (let [c (class mv-dummy)]
                    (or (= c Maybe)
                        (= c MaybeTransformer)))
           m-zero? (satisfies? MonadZero mv-dummy)
           state-t-mv-dummy (StateTransformer. return-m
                                               dummy
                                               nil
                                               nil
                                               nil
                                               nil
                                               nil
                                               m-zero?)]
       (fn state-t*
         ([v]
            (if (and maybe?
                     (= (maybe v) Nothing))
              (state-t* (fn [s] (zero mv-dummy))
                        (fn [v] (state-t* v)))
              (StateTransformer. return-m
                                 v
                                 nil
                                 nil
                                 nil
                                 nil
                                 nil
                                 m-zero?)))
         ([mv f]
            (StateTransformer. return-m
                               nil
                               mv
                               (wrap-check state-t-mv-dummy f)
                               nil
                               nil
                               nil
                               m-zero?))
         ([mv f & fs]
            (let [f* (first fs)]
              (if-let [fs (seq* (rest fs))]
                (apply state-t* (concat
                                 [(StateTransformer. return-m
                                                     nil
                                                     mv
                                                     (wrap-check state-t-mv-dummy f)
                                                     nil
                                                     nil
                                                     nil
                                                     m-zero?)
                                  f*] fs))
                (state-t* (StateTransformer. return-m
                                             nil
                                             mv
                                             (wrap-check state-t-mv-dummy f)
                                             nil
                                             nil
                                             nil
                                             m-zero?)
                          f*))))))))

(defn update-state-t
  [state-t-factory]
  (let [state-t-mv-dummy (state-t-factory dummy)]
    (when *check-types*
      (assert (instance? StateTransformer
                         state-t-mv-dummy)
              (str "Type mismatch: monads.core/update-state-t should be "
                   "called with a function that returns an instance of "
                   "monads.core.StateTransformer.")))
    (let [return-m (.return-m state-t-mv-dummy)
          m-zero? (.m-zero? state-t-mv-dummy)
          state-t* (fn [v] (StateTransformer. return-m
                                              v
                                              nil
                                              nil
                                              nil
                                              nil
                                              true
                                              m-zero?))]
      (fn [f]
        (state-t-factory (fn [s] (return-m [s (f s)]))
                         state-t*)))))

(defn set-state-t
  "Return a function that returns a StateTransformer monad value (for
  the monad specified by state-t-factory) that replaces the current
  state by s and returns the previous state."
  [state-t-factory]
  (let [u (update-state-t state-t-factory)]
    (fn [s]
      (u (constantly s)))))

(defn get-state-t
  "Return a function that returns a StateTransformer monad value (for
  the monad specified by state-t-factory) that returns the current
  state and does not modify it."
  [state-t-factory]
  (let [u (update-state-t state-t-factory)]
    (fn []
      (u identity))))

(defn get-state-t-val
  "Return a function that returns a StateTransformer monad value (for
  the monad specified by state-t-factory) that assumes the state to be
  a map and returns the value corresponding to the given key. The state
  is not modified."
  [state-t-factory]
  (let [g (get-state-t state-t-factory)
        return-m-state (partial return (state-t-factory dummy))]
    (fn [key]
      (bind (g)
            #(return-m-state (get % key))))))

(defn update-state-t-val
  "Return a function that returns a StateTransformer monad value (for
  the monad specified by state-t-factory) that assumes the state to be
  a map and replaces the value associated with the given key by the
  return value of f applied to the old value and args. The old value is
  returned."
  [state-t-factory]
  (let [u (update-state-t state-t-factory)
        return-m-state (partial return (state-t-factory dummy))]
    (fn [key f & args]
      (bind (u #(apply update-in % [key] f args))
            #(return-m-state (get % key))))))

(defn set-state-t-val
  "Return a function that returns a StateTransformer monad value (for
  the monad specified by state-t-factory) that assumes the state to be
  a map and replaces the value associated with key by val. The old
  value is returned."
  [state-t-factory]
  (let [uv (update-state-t-val state-t-factory)]
    (fn [key val]
      (uv key (constantly val)))))

(defn get-in-state-t-val
  [state-t-factory]
  (let [g (get-state-t state-t-factory)
        return-m-state (partial return (state-t-factory dummy))]
    (fn [path & [default]]
      (bind (g)
            #(return-m-state (get-in % path default))))))

(defn assoc-in-state-t-val
  [state-t-factory]
  (let [u (update-state-t state-t-factory)
        return-m-state (partial return (state-t-factory dummy))]
    (fn [path val]
      (bind (u #(assoc-in % path val))
            #(return-m-state (get-in % path))))))

(defn update-in-state-t-val
  [state-t-factory]
  (let [u (update-state-t state-t-factory)
        return-m-state (partial return (state-t-factory dummy))]
    (fn [path f & args]
      (bind (u #(apply update-in % path f args))
            #(return-m-state (get-in % path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Writer transformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype WriterTransformer [return-m mv writer-m]
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (hash (str WriterTransformer))
             (.hashCode this)))
  (hashCode [_]
    (bit-xor (hash return-m)
             (hash mv)
             (hash writer-m)))
  (equals [this that]
    (and (= WriterTransformer (class that))
         (= (.return-m that) return-m)
         (= (.mv that) mv)
         (= (.writer-m that) writer-m)))

  clojure.lang.IDeref
  (deref [_]
    mv)

  Monad
  (return [_ v]
    (WriterTransformer. return-m
                        (return-m (writer-m v))
                        writer-m))
  (bind [mv f]
    (WriterTransformer. return-m
                        (bind (deref mv)
                              (fn [v]
                                (let [[v1 a1] (deref v)]
                                  (bind (deref ((wrap-check mv f) v1))
                                        (fn [v]
                                          (let [[v2 a2] (deref v)]
                                            (return-m (Writer. v2
                                                               (writer-m-combine a1 a2)))))))))
                        writer-m))

  MonadZero
  (zero [mv]
    (let [v (deref mv)]
      (WriterTransformer. return-m
                          (zero v)
                          writer-m)))
  (plus' [mv mvs]
    (WriterTransformer. return-m
                        (plus (map* deref (cons mv mvs)))
                        writer-m))
  (plus'* [mv mvs]
    (WriterTransformer. return-m
                        (plus (map* deref (cons mv (map* #(%) mvs))))
                        writer-m))

  MonadDev
  (types [_]
    [[WriterTransformer]]))

(defn writer-t [mv-factory accumulator]
  (let [return-m (partial return (mv-factory dummy))
        writer-m (writer accumulator)]
    (if (= mv-factory maybe)
      (fn [v]
        (let [mv (if (= (maybe v) Nothing)
                   Nothing
                   (return-m (writer-m v)))]
          (WriterTransformer. return-m
                              mv
                              writer-m)))
      (fn [v]
        (let [mv (return-m (writer-m v))]
          (WriterTransformer. return-m
                              mv
                              writer-m))))))

(defn- write-writer**
  [writer-mv val-to-write]
  (class writer-mv))

(defmulti ^:private write-writer*
  #'write-writer**)

(defmethod write-writer*
  Writer
  [writer-mv val-to-write]
  (let [[_ a] (deref writer-mv)]
    (Writer. nil
             (writer-m-add a val-to-write))))

(defmethod write-writer*
  WriterTransformer
  [writer-t-mv val-to-write]
  (let [return-m (.return-m writer-t-mv)
        mv (bind (deref writer-t-mv)
                 (fn [writer-mv] (return-m (write-writer* writer-mv val-to-write))))]
    (WriterTransformer. return-m
                        mv
                        (.writer-m writer-t-mv))))

(defn write-writer
  [writer-factory val-to-write]
  (write-writer* (writer-factory dummy)
                 val-to-write))

(defmulti listen-writer
  class)

(defmethod listen-writer
  Writer
  [writer-mv]
  (let [[v a :as va] (deref writer-mv)]
    (Writer. va
             a)))

(defmethod listen-writer
  WriterTransformer
  [writer-t-mv]
  (let [return-m (.return-m writer-t-mv)
        mv (bind (deref writer-t-mv)
                 (fn [writer-mv] (return-m (listen-writer writer-mv))))]
    (WriterTransformer. return-m
                        mv
                        (.writer-m writer-t-mv))))

(defn- censor-writer*
  [f writer-mv]
  (class writer-mv))

(defmulti censor-writer
  #'censor-writer*)

(defmethod censor-writer
  Writer
  [f writer-mv]
  (let [[v a] (deref writer-mv)]
    (Writer. v
             (f a))))

(defmethod censor-writer
  WriterTransformer
  [f writer-t-mv]
  (let [return-m (.return-m writer-t-mv)
        mv (bind (deref writer-t-mv)
                 (fn [writer-mv] (return-m (censor-writer f writer-mv))))]
    (WriterTransformer. return-m
                        mv
                        (.writer-m writer-t-mv))))
