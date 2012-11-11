(ns monads2.core
  (:refer-clojure :exclude [do seq])
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;

(defn reduce-n [n f acc coll]
  (let [to-consume (take n coll)]
    (cond
      (not (= n (count to-consume)))
        (apply f acc (take n (concat to-consume
                                     (repeat nil))))
        ; ^ if the number of items isn't enough to satisfy
        ; the number of arguments, we fill up the seq with
        ; nil values
      :else
        (recur n f (apply f acc to-consume) (rest coll)))))

;;;;;;;;;;;;;;;;;;;;

(defprotocol Monad
  (do-result [_ v])
  (bind [mv f]))

(defprotocol MonadZero
  (zero [_])
  (plus-step [mv mvs]))

(defn plus [[mv & mvs]]
  (plus-step mv mvs))

;;;;;;;;;;;;;;;;;;;;

(declare monadic-step-reduction)

(def ^:private prepare-monadic-steps
  "Transforms the binding list into a monadic
  step list that the `process-monadic-steps`
  function will consume."
  (comp reverse
        (partial partition 2)))

(def ^:private remove-flow-keywords
  "Removes monadic steps which include keywords on the
  symbol side (e.g :if, :then, :else, :when, :let)."
  (partial remove (comp keyword? first)))

(def ^:private get-monadic-value-sample
  "Returns the first monadic value being used
  in the monad comprehension, this will serve
  as a sample to execute the `do-return` function."
  (comp second
        first
        remove-flow-keywords))

;;;;;;;;;;

(defn- process-monadic-steps
  "Process a monadic step list by reducing
  each of it's elements with a call to `monadic-step-reduction`."
  [sample expr steps]
  (reduce-n 3 (monadic-step-reduction sample)
              expr
              steps))

(defn- if-then-else-reduction
  "Processes a if-then-else statement on a monadic
   environment."
  [predicate if-steps else-steps sample expr]
  `(if '~predicate
     ~(process-monadic-steps sample
                             expr
                             (prepare-monadic-steps if-steps))
     ~(process-monadic-steps sample
                             expr
                             (prepare-monadic-steps else-steps))))

(defn- monadic-step-reduction
  "Processes all steps on a monad-comprehension."
  [sample]
  (fn [expr [sym1 mv1] [sym2 mv2] [sym3 mv3]]
    (cond
    (= :if sym1)
      expr
    (= :then sym1)
      expr
    (and (= :else sym1)
         (= :then sym2)
         (= :if   sym3))
      (if-then-else-reduction mv3 mv2 mv1 sample expr)

    (= :when sym1) `(if ~mv1
                    ~expr
                    (monads2.core/zero ~sample))
    (= :let sym1) `(let ~mv1
                   ~expr)
    :else `(monads2.core/bind ~mv1 (fn [~sym1] ~expr)))))

;;;;;;;;;;

(defmacro do [bindings expr]
  (let [steps  (prepare-monadic-steps bindings)
        sample (get-monadic-value-sample steps)]
    (process-monadic-steps sample
                           `(monads2.core/do-result ~sample ~expr)
                           steps)))

;;;;;;;;;;;;;;;;;;;;

(defmacro seq [mvs]
  (let [steps (map (fn [x]
                     (let [sym (gensym)]
                       [sym x]))
                   mvs)
        syms (map first steps)]
    `(monads2.core/do [~@(apply concat steps)] [~@syms])))

;;;;;;;;;;;;;;;;;;;;

(extend-type clojure.lang.PersistentList
  Monad
  (do-result [_ v]
          (list v))
  (bind [mv f]
        (mapcat f mv))

  MonadZero
  (zero [_]
        (list))
  (plus-step [mv mvs]
             (apply concat mv mvs)))

;;;;;;;;;;;;;;;;;;;;

(extend-type clojure.lang.PersistentList$EmptyList
  Monad
  (do-result [_ v]
          (list v))
  (bind [mv f]
        (mapcat f mv))

  MonadZero
  (zero [_]
        (list))
  (plus-step [mv mvs]
             (apply concat mv mvs)))

;;;;;;;;;;;;;;;;;;;;

(extend-type clojure.lang.PersistentVector
  Monad
  (do-result [_ v]
          [v])
  (bind [mv f]
        (vec (mapcat f mv)))

  MonadZero
  (zero [_]
        [])
  (plus-step [mv mvs]
             (vec (apply concat mv mvs))))

;;;;;;;;;;;;;;;;;;;;

(extend-type clojure.lang.LazySeq
  Monad
  (do-result [_ v]
          (list v))
  (bind [mv f]
        (mapcat f mv))

  MonadZero
  (zero [_]
        [])
  (plus-step [mv mvs]
             ; TODO: make lazy
             (apply concat mv mvs)))

;;;;;;;;;;;;;;;;;;;;

(extend-type clojure.lang.PersistentHashSet
  Monad
  (do-result [_ v]
          (hash-set v))
  (bind [mv f]
        (apply set/union
               (map f mv)))

  MonadZero
  (zero [_]
        #{})
  (plus-step [mv mvs]
             (apply set/union mv mvs)))

;;;;;;;;;;;;;;;;;;;;

(declare state)

(deftype state-binder [mv f]
  clojure.lang.IFn
  (invoke [_ s]
          (let [[v ss] (mv s)]
            ((f v) ss)))

  Monad
  (do-result [_ v]
          (state v))
  (bind [mv f]
        (state-binder. mv f)))

(deftype state-monad [v]
  clojure.lang.IFn
  (invoke [_ s]
          [v s])

  Monad
  (do-result [_ v]
          (state v))
  (bind [mv f]
        (state-binder. mv f)))

(defn state [v]
  (state-monad. v))

(defn update-state [f]
  (reify
    clojure.lang.IFn
    (invoke [_ s]
            [s (f s)])

    Monad
    (do-result [_ v]
               (state v))
    (bind [mv f]
          (state-binder. mv f))))

(defn set-state [s]
  (update-state (constantly s)))

(defn get-state []
  (update-state identity))

(defn get-val [key]
  (monads2.core/do
    [s (get-state)]
    (get s key)))

(defn update-val [key f & args]
  (monads2.core/do
    [s (update-state #(apply update-in % [key] f args))]
    (get s key)))

(defn set-val [key val]
  (update-val key (constantly val)))


;;;;;;;;;;;;;;;;;;;;

(declare cont)

(deftype cont-binder [mv f]
  clojure.lang.IFn
  (invoke [_ c]
          (mv (fn [v] ((f v) c))))

  Monad
  (do-result [_ v]
          (cont v))
  (bind [mv f]
        (cont-binder. mv f)))

(deftype cont-monad [v]
  clojure.lang.IFn
  (invoke [_ c]
          (c v))

  Monad
  (do-result [_ v]
          (cont-monad. v))
  (bind [mv f]
        (cont-binder. mv f)))

(defn cont [v]
  (cont-monad. v))

