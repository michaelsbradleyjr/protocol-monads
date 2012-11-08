(ns monads.test.core
  (:use [clojure.test])
  (:require [monads.core :as m])
  (:import [monads.core WriterTransformer StateTransformer]))

(alter-var-root (var m/*throw-on-mismatch*) (constantly true))
(alter-var-root (var m/*warn-on-mismatch*) (constantly false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Factory functions for clojure.core types, namespaced in monads.core
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest monads-core-and-clojure-core-list-factory-equiv
  (is (= (m/list 1 2 3)
         (list 1 2 3))))

(deftest monads-core-and-clojure-core-vector-factory-equiv
  (is (= (m/vector 1 2 3)
         (vector 1 2 3))))

(deftest monads-core-and-clojure-core-vec-factory-equiv
  (is (= (m/vec (list 1 2 3))
         (vec (list 1 2 3)))))

(deftest monads-core-and-clojure-core-lazy-seq-factory-equiv
  (is (= (m/lazy-seq 1 2 3)
         (lazy-seq [1 2 3]))))

(deftest monads-core-and-clojure-core-hash-set-factory-equiv
  (is (= (m/hash-set 1 2 3)
         (hash-set 1 2 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def lazy-concat (ns-resolve 'monads.core 'lazy-concat))

(deftest test-lazy-concat-laziness
  (is (= clojure.lang.LazySeq
         (class (lazy-concat (m/lazy-seq (/ 1 0)
                                         (/ 1 0))
                             (m/lazy-seq (m/lazy-seq (/ 1 0)
                                                     (/ 1 0)
                                                     (/ 1 0))))))))

(deftest test-lazy-concat-return
  (is (= (m/lazy-seq (/ 1 1) (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5))
         (lazy-concat (m/lazy-seq (/ 1 1)
                                  (/ 1 2))
                      (m/lazy-seq (m/lazy-seq (/ 1 3)
                                              (/ 1 4)
                                              (/ 1 5)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistenList
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn list-f [n]
  (list (inc n)))

(defn list-g [n]
  (list (+ n 5)))

(def do-result-list (partial m/do-result (list [nil])))
(def zero-val-list (m/zero (list [nil])))

(deftest do-result-and-list-factory-func-equiv
  (is (= (do-result-list [nil])
         (list [nil]))))

(deftest first-law-list
  (is (= (m/bind (do-result-list 10) list-f)
         (list-f 10))))

(deftest second-law-list
  (is (= (m/bind (do-result-list 10) do-result-list)
         (do-result-list 10))))

(deftest third-law-list
  (is (= (m/bind (m/bind '(4 9) list-f) list-g)
         (m/bind '(4 9) (fn [x]
                          (m/bind (list-f x) list-g))))))

(deftest zero-law-list
  (is (= (m/bind zero-val-list list-f)
         zero-val-list))
  (is (= (m/bind (do-result-list 4) (constantly zero-val-list))
         zero-val-list))
  (is (= (m/plus [(list 5 6) zero-val-list])
         (list 5 6)))
  (is (= (m/plus [zero-val-list (list 5 6)])
         (list 5 6)))
  (is (= (m/plus* [(list 5 6) zero-val-list])
         (list 5 6)))
  (is (= (m/plus* [zero-val-list (list 5 6)])
         (list 5 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentVector
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn vector-f [n]
  (vector (inc n)))

(defn vector-g [n]
  (vector (+ n 5)))

(def do-result-vector (partial m/do-result (vector [nil])))
(def zero-val-vector (m/zero (vector [nil])))

(deftest do-result-and-vector-factory-func-equiv
  (is (= (do-result-vector [nil])
         (vector [nil]))))

(deftest first-law-vector
  (is (= (m/bind (do-result-vector 10) vector-f)
         (vector-f 10))))

(deftest second-law-vector
  (is (= (m/bind (do-result-vector 10) do-result-vector)
         (do-result-vector 10))))

(deftest third-law-vector
  (is (= (m/bind (m/bind [4 9] vector-f) vector-g)
         (m/bind [4 9] (fn [x]
                         (m/bind (vector-f x) vector-g))))))

(deftest zero-law-vector
  (is (= (m/bind zero-val-vector vector-f)
         zero-val-vector))
  (is (= (m/bind (do-result-vector 4) (constantly zero-val-vector))
         zero-val-vector))
  (is (= (m/plus [(vector 5 6) zero-val-vector])
         (vector 5 6)))
  (is (= (m/plus [zero-val-vector (vector 5 6)])
         (vector 5 6)))
  (is (= (m/plus* [(vector 5 6) zero-val-vector])
         (vector 5 6)))
  (is (= (m/plus* [zero-val-vector (vector 5 6)])
         (vector 5 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.LazySeq
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lazy-seq-f [n]
  (m/lazy-seq (inc n)))

(defn lazy-seq-g [n]
  (m/lazy-seq (+ n 5)))

(def do-result-lazy-seq (partial m/do-result (m/lazy-seq [nil])))
(def zero-val-lazy-seq (m/zero (m/lazy-seq [nil])))

(deftest do-result-and-lazy-seq-factory-func-equiv
  (is (= (do-result-lazy-seq [nil])
         (m/lazy-seq [nil]))))

(deftest first-law-lazy-seq
  (is (= (m/bind (do-result-lazy-seq 10) lazy-seq-f)
         (lazy-seq-f 10))))

(deftest second-law-lazy-seq
  (is (= (m/bind (do-result-lazy-seq 10) do-result-lazy-seq)
         (do-result-lazy-seq 10))))

(deftest third-law-lazy-seq
  (is (= (m/bind (m/bind (m/lazy-seq 4 9) lazy-seq-f) lazy-seq-g)
         (m/bind (m/lazy-seq 4 9) (fn [x]
                                    (m/bind (lazy-seq-f x) lazy-seq-g))))))

(deftest zero-law-lazy-seq
  (is (= (m/bind zero-val-lazy-seq lazy-seq-f)
         zero-val-lazy-seq))
  (is (= (m/bind (do-result-lazy-seq 4) (constantly zero-val-lazy-seq))
         zero-val-lazy-seq))
  (is (= (m/plus [(m/lazy-seq 5 6) zero-val-lazy-seq])
         (m/lazy-seq 5 6)))
  (is (= (m/plus [zero-val-lazy-seq (m/lazy-seq 5 6)])
         (m/lazy-seq 5 6)))
  (is (= (m/plus* [(m/lazy-seq 5 6) zero-val-lazy-seq])
         (m/lazy-seq 5 6)))
  (is (= (m/plus* [zero-val-lazy-seq (m/lazy-seq 5 6)])
         (m/lazy-seq 5 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.PersistentHashSet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-f [n]
  (hash-set (inc n)))

(defn set-g [n]
  (hash-set (+ n 5)))

(def do-result-set (partial m/do-result (hash-set [nil])))
(def zero-val-set (m/zero (hash-set [nil])))

(deftest do-result-and-set-factory-func-equiv
  (is (= (do-result-set [nil])
         (hash-set [nil]))))

(deftest first-law-set
  (is (= (m/bind (do-result-set 10) set-f)
         (set-f 10))))

(deftest second-law-set
  (is (= (m/bind (do-result-set 10) do-result-set)
         (do-result-set 10))))

(deftest third-law-set
  (is (= (m/bind (m/bind #{4 9} set-f) set-g)
         (m/bind #{4 9} (fn [x]
                          (m/bind (set-f x) set-g))))))

(deftest zero-law-set
  (is (= (m/bind zero-val-set set-f)
         zero-val-set))
  (is (= (m/bind (do-result-set 4) (constantly zero-val-set))
         zero-val-set))
  (is (= (m/plus [(hash-set 5 6) zero-val-set])
         (hash-set 5 6)))
  (is (= (m/plus [zero-val-set (hash-set 5 6)])
         (hash-set 5 6)))
  (is (= (m/plus* [(hash-set 5 6) zero-val-set])
         (hash-set 5 6)))
  (is (= (m/plus* [zero-val-set (hash-set 5 6)])
         (hash-set 5 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.Maybe
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def do-result-maybe (partial m/do-result (m/maybe [nil])))
(def zero-val-maybe (m/zero (m/maybe [nil])))

(deftest do-result-and-maybe-factory-func-equiv
  (is (= @(do-result-maybe [nil])
         @(m/maybe [nil]))))

(deftest do-result-and-maybe-factory-func-not-equiv-for-nil
  (is (not= @(do-result-maybe nil)
            @(m/maybe nil))))

(deftest zero-val-from-factory-func
  (is (= m/maybe-zero-val
         zero-val-maybe
         (m/maybe nil))))

(defn maybe-f [n]
  (m/maybe (inc n)))

(defn maybe-g [n]
  (m/maybe (+ n 5)))

(deftest first-law-maybe
  (is (= @(m/bind (do-result-maybe 10) maybe-f)
         @(maybe-f 10))))

(deftest second-law-maybe
  (is (= @(m/bind (do-result-maybe 10) do-result-maybe)
         @(do-result-maybe 10))))

(deftest third-law-maybe
  (is (= @(m/bind (m/bind (m/maybe 5) maybe-f) maybe-g)
         @(m/bind (m/maybe 5) (fn [x]
                                (m/bind (maybe-f x) maybe-g))))))

;; Special cases -- ensure we're handling them correctly. The m/maybe
;; factory function is not used to generate the monadic value passed
;; as the first argument to m/bind, since m/maybe implements
;; "convenience logic" which short-circuits nil to maybe-zero-val. The
;; procol method do-result, as implemented for class maybe-monad does
;; not short-circuit.
(deftest first-law-maybe-nil
  (is (= @(m/bind (do-result-maybe nil) (comp m/maybe not))
         @((comp m/maybe not) nil))))

;; For the same reasons given in the previous comment, m/maybe is not
;; used as the monadic function for this test, nor to generate the
;; monadic value passed as the first argument to m/bind.
(deftest second-law-maybe-nil
  (is (= @(m/bind (do-result-maybe nil) do-result-maybe)
         @(do-result-maybe nil))))

(deftest zero-law-maybe
  (is (= (m/bind zero-val-maybe maybe-f)
         zero-val-maybe))
  (is (= (m/bind (do-result-maybe 4) (constantly zero-val-maybe))
         zero-val-maybe))
  (is (= @(m/plus [(m/maybe 6) zero-val-maybe])
         @(m/maybe 6)))
  (is (= @(m/plus [zero-val-maybe (m/maybe 6)])
         @(m/maybe 6)))
  (is (= @(m/plus* [(m/maybe 6) zero-val-maybe])
         @(m/maybe 6)))
  (is (= @(m/plus* [zero-val-maybe (m/maybe 6)])
         @(m/maybe 6))))

(deftest lazy-maybe-plus*
  (is (= :test
         @(m/plus* [(m/maybe :test)
                    (m/do m/maybe
                          [_ (m/maybe 10)
                           _ (m/maybe (/ 1 0))]
                          (throw (Exception. "Should not be thrown")))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.State
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def do-result-state (partial m/do-result (m/state [nil])))

(deftest do-result-and-state-factory-func-equiv
  (let [mv1 (do-result-state [nil])
        mv2 (m/state [nil])]
    (is (= (mv1 {}) (mv2 {})))))

(defn state-f [n]
  (m/state (inc n)))

(defn state-g [n]
  (m/state (+ n 5)))

(deftest first-law-state
  (let [mv1 (m/bind (do-result-state 10) state-f)
        mv2 (state-f 10)]
    (is (= (mv1 {}) (mv2 {})))))

(deftest second-law-state
  (let [mv1 (m/bind (do-result-state 10) do-result-state)
        mv2 (do-result-state 10)]
    (is (= (mv1 :state) (mv2 :state)))))

(deftest third-law-state
  (let [mv1 (m/bind (m/bind (m/state 4) state-f) state-g)
        mv2 (m/bind (m/state 4)
                    (fn [x]
                      (m/bind (state-f x) state-g)))]
    (is (= (mv1 :state) (mv2 :state)))))

(deftest test-update-state
  (is (= [:state :new-state]
         ((m/update-state (constantly :new-state)) :state))))

(deftest test-get-val
  (is (= [17 {:a 17}]
         ((m/get-val :a) {:a 17}))))

(deftest test-set-val
  (is (= [17 {:a 12}]
         ((m/set-val :a 12) {:a 17}))))

(deftest test-update-val
  (is (= [5 {:a 19}]
         ((m/update-val :a + 14) {:a 5}))))

(deftest test-get-in-val
  (let [state {:a {:b 1} :c {:d {:e 2}}}]
    (are [expected args] (is (= expected ((apply m/get-in-val args) state)))
         [1 state]      [[:a :b]]
         [:def state]   [[:z] :def]
         [nil state]    [[:a :b :c]]
         [2 state]      [[:c :d :e]]
         [{:b 1} state] [[:a]])))

(deftest test-assoc-in-val
  (is (= [nil {:a {:b {:c 9}}}]
         ((m/assoc-in-val [:a :b :c] 9) {}))))

(deftest test-update-in-val
  (are [expected in-state path args] (is (= expected
                                            ((apply m/update-in-val path args) in-state)))
       [2 {:a {:b 4}}]      {:a {:b 2}}  [:a :b]  [* 2]
       [2 {:a {:b 3}}]      {:a {:b 2}}  [:a :b]  [inc]
       [nil {:a {:b [1]}}]  {:a nil}     [:a :b]  [(fnil conj []) 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.Continuation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def do-result-cont (partial m/do-result (m/cont [nil])))

(deftest do-result-and-cont-factory-func-equiv
  (is (= @(do-result-cont [nil])
         @(m/cont [nil]))))

(defn cont-f [n]
  (m/cont (inc n)))

(defn cont-g [n]
  (m/cont (+ n 5)))

(deftest first-law-cont
  (let [mv1 (m/bind (do-result-cont 10) cont-f)
        mv2 (cont-f 10)]
    (is (= (mv1 identity) (mv2 identity)))))

(deftest second-law-cont
  (let [mv1 (m/bind (do-result-cont 10) m/cont)
        mv2 (do-result-cont 10)]
    (is (= (mv1 identity) (mv2 identity)))))

(deftest third-law-cont
  (let [mv1 (m/bind (m/bind (m/cont 4) cont-f) cont-g)
        mv2 (m/bind (m/cont 4)
                    (fn [x]
                      (m/bind (cont-f x) cont-g)))]
    (is (= (mv1 identity) (mv2 identity)))))

(deftest deref-cont
  (is (= 10 @(m/cont 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.call-cc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment "monads.core/call-cc is not yet implemented.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.Writer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-writer (m/writer #{}))

(def do-result-writer (partial m/do-result (test-writer [nil])))

(deftest do-result-and-writer-factory-func-equiv
  (is (= @(do-result-writer [nil])
         @(test-writer [nil]))))

(defn writer-f [n]
  (test-writer (inc n)))

(defn writer-g [n]
  (test-writer (+ n 5)))

(deftest first-law-writer
  (is (= @(m/bind (test-writer 10) writer-f)
         @(writer-f 10))))

(deftest second-law-writer
  (is (= @(m/bind (test-writer 10) test-writer)
         @(test-writer 10))))

(deftest third-law-writer
  (is (=@(m/bind (m/bind (test-writer 3) writer-f) writer-g)
        @(m/bind (test-writer 3)
                 (fn [x]
                   (m/bind (writer-f x) writer-g))))))

(deftest test-write
  (is (= [nil #{:written}]
         @(m/write test-writer :written))))

(deftest test-listen
  (is (= [[nil #{:written}] #{:written}]
         @(m/listen (m/write test-writer :written)))))

(deftest test-censor
  (is (= [nil #{:new-written}]
         @(m/censor (constantly #{:new-written})
                    (m/write test-writer :written)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Monad functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-do-basic
  (is (= (m/bind (range 5)
                 (fn [x]
                   (m/bind (range 3)
                           (fn [y]
                             (m/lazy-seq (+ x y))))))
         (m/do lazy-seq
               [x (range 5)
                y (range 3)]
               (+ x y)))))

(deftest test-do-when
  (is (= (m/zero (lazy-seq))
         (m/bind (range 5)
                 (fn [x]
                   (m/bind (range 3)
                           (fn [y]
                             (if (> x 1000)
                               (m/lazy-seq (+ x y))
                               (m/zero (lazy-seq)))))))
         (m/do lazy-seq
               [x (range 5)
                y (range 3)
                :when (> x 1000)]
               (+ x y)))))

(deftest test-do-let
  (is (= (m/bind (range 5)
                 (fn [x]
                   (let [z 100]
                     (m/bind (range 3)
                             (fn [y]
                               (m/lazy-seq (+ x y z)))))))
         (m/do lazy-seq
               [x (range 5)
                :let [z 100]
                y (range 3)]
               (+ x y z)))))

(deftest test-plus
  (is (= true true)))

(deftest test-plus*-laziness
  (is (= true true)))

(deftest test-plus*-return
  (is (= true true)))

(def comprehend (ns-resolve 'monads.core 'comprehend))

(deftest test-comprehend
  (is (= true true)))

(deftest test-seq
  (is (= [[3 :a] [3 :b] [5 :a] [5 :b]]
         (m/seq [[3 5] [:a :b]])))
  (is (= [[]]
         (m/seq vector []))))

(deftest test-lift
  (let [lifted-+ (m/lift +)]
    (is (= [6]
           (apply lifted-+ (map vector (range 4)))))
    (is (= [6 :state]
           ((apply lifted-+ (map m/state (range 4))) :state)))))

(deftest test-join
  (is (= true true)))

(deftest test-fmap
  (is (= true true)))

(deftest test-map
  (is (= true true)))

(deftest test-chain
  (let [t (fn [x] (vector (inc x) (* 2 x)))
        u (fn [x] (vector (dec x)))
        st (fn [x] (m/state (inc x)))
        su (fn [x] (m/state (* 2 x)))]
    (is (= (map (fn [x] (m/do vector [y (t x) z (u y)] z)) (range 4))
           (map (m/chain [t u]) (range 4))))
    (is (= (m/do vector [x (vec (range 4)) y (t x) z (u y)] z)
           ((m/chain [(comp vec range) t u]) 4)))
    (is (= ((m/do m/state [x (st 8) y (su x)] y) :state)
           (((m/chain [st su]) 8) :state)))))


;; The tests below have been disabled, and are in the process of being
;; reworked and re-enabled in light of monads.core/check-return-type
;; and other modifications to the protocol-monads library
(comment

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  monads.core.ListTransformer
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def set-list (m/list-t hash-set))
  (defn list-t-f [n]
    (set-list (inc n)))

  (defn list-t-g [n]
    (set-list (+ n 5)))

  (deftest first-law-list-t
    (is (= @(m/bind (set-list 10) list-t-f)
           @(list-t-f 10))))

  (deftest second-law-list-t
    (is (= @(m/bind (set-list 10) set-list)
           @(set-list 10))))

  (deftest third-law-list-t
    (is (= @(m/bind (m/bind (set-list 4) list-t-f) list-t-g)
           @(m/bind (set-list 4)
                    (fn [x]
                      (m/bind (list-t-f x) list-t-g))))))

  (deftest zero-law-list-t
    (is (= #{'()} @(m/zero (set-list nil))))
    (is (= @(m/bind (m/zero (set-list nil)) list-t-f)
           @(m/zero (set-list nil))))
    (is (= @(m/bind (set-list 4) (constantly (m/zero (set-list nil))))
           @(m/zero (set-list nil))))
    (is (= @(m/plus [(set-list 4) (m/zero (set-list nil))])
           @(set-list 4)))
    (is (= @(m/plus [(m/zero (set-list nil)) (set-list 4)])
           @(set-list 4))))

  (deftest do-list-t
    (is (= #{(list [10 15])}
           @(m/do set-list
                  [x (list-t-f 9)
                   y (list-t-g x)]
                  [x y]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  monads.core.VectorTransformer
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def set-vect (m/vector-t hash-set))
  (defn vector-t-f [n]
    (set-vect (inc n)))

  (defn vector-t-g [n]
    (set-vect (+ n 5)))

  (deftest first-law-vector-t
    (is (= @(m/bind (set-vect 10) vector-t-f)
           @(vector-t-f 10))))

  (deftest second-law-vector-t
    (is (= @(m/bind (set-vect 10) set-vect)
           @(set-vect 10))))

  (deftest third-law-vector-t
    (is (= @(m/bind (m/bind (set-vect 4) vector-t-f) vector-t-g)
           @(m/bind (set-vect 4)
                    (fn [x]
                      (m/bind (vector-t-f x) vector-t-g))))))

  (deftest zero-law-vector-t
    (is (= #{[]} @(m/zero (set-vect nil))))
    (is (= @(m/bind (m/zero (set-vect nil)) vector-t-f)
           @(m/zero (set-vect nil))))
    (is (= @(m/bind (set-vect 4) (constantly (m/zero (set-vect nil))))
           @(m/zero (set-vect nil))))
    (is (= @(m/plus [(set-vect 4) (m/zero (set-vect nil))])
           @(set-vect 4)))
    (is (= @(m/plus [(m/zero (set-vect nil)) (set-vect 4)])
           @(set-vect 4))))

  (deftest do-vector-t
    (is (= #{(vector [10 15])}
           @(m/do set-vect
                  [x (vector-t-f 9)
                   y (vector-t-g x)]
                  [x y]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  monads.core.LazySeqTransformer
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (comment "monads.core.LazySeqTransformer is not yet implemented.")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  monads.core.SetTransformer
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def vect-set (m/set-t vector))
  (defn set-t-f [n]
    (vect-set (inc n)))

  (defn set-t-g [n]
    (vect-set (+ n 5)))

  (deftest first-law-set-t
    (is (= @(m/bind (vect-set 10) set-t-f)
           @(set-t-f 10))))

  (deftest second-law-set-t
    (is (= @(m/bind (vect-set 10) vect-set)
           @(vect-set 10))))

  (deftest third-law-set-t
    (is (= @(m/bind (m/bind (vect-set 4) set-t-f) set-t-g)
           @(m/bind (vect-set 4)
                    (fn [x]
                      (m/bind (set-t-f x) set-t-g))))))

  (deftest zero-law-set-t
    (is (= [#{}] @(m/zero (vect-set nil))))
    (is (= @(m/bind (m/zero (vect-set nil)) set-t-f)
           @(m/zero (vect-set nil))))
    (is (= @(m/bind (vect-set 4) (constantly (m/zero (vect-set nil))))
           @(m/zero (vect-set nil))))
    (is (= @(m/plus [(vect-set 4) (m/zero (vect-set nil))])
           @(vect-set 4)))
    (is (= @(m/plus [(m/zero (vect-set nil)) (vect-set 4)])
           @(vect-set 4))))

  (deftest do-set-t
    (is (= [(hash-set [10 15])]
           @(m/do vect-set
                  [x (set-t-f 9)
                   y (set-t-g x)]
                  [x y]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  monads.core.MaybeTransformer
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def vect-maybe (m/maybe-t vector))
  (defn maybe-t-f [n]
    (vect-maybe (inc n)))

  (defn maybe-t-g [n]
    (vect-maybe (+ n 5)))

  (deftest first-law-maybe-t
    (is (= @(first @(m/bind (vect-maybe 10) maybe-t-f))
           @(first @(maybe-t-f 10)))))

  (deftest second-law-maybe-t
    (is (= @(first @(m/bind (vect-maybe 10) vect-maybe))
           @(first @(vect-maybe 10)))))

  (deftest third-law-maybe-t
    (is (= @(first @(m/bind (m/bind (vect-maybe 4) maybe-t-f) maybe-t-g))
           @(first @(m/bind (vect-maybe 4)
                            (fn [x]
                              (m/bind (maybe-t-f x) maybe-t-g)))))))

  (deftest zero-law-maybe-t
    (is (= m/maybe-zero-val (first @ (m/zero (vect-maybe nil)))))
    (is (= (first @(m/bind (m/zero (vect-maybe nil)) maybe-t-f))
           (first @(m/zero (vect-maybe nil)))))
    (is (= (first @(m/bind (vect-maybe 4) (constantly (m/zero (vect-maybe nil)))))
           (first @(m/zero (vect-maybe nil)))))
    (is (= @(first @(m/plus [(vect-maybe 4) (m/zero (vect-maybe nil))]))
           @(first @(vect-maybe 4))))
    (is (= @(first @(m/plus [(m/zero (vect-maybe nil)) (vect-maybe 4)]))
           @(first @(vect-maybe 4)))))

  (deftest do-maybe-t
    (is (= [10 15]
           @(first @(m/do vect-maybe
                          [x (maybe-t-f 9)
                           y (maybe-t-g x)]
                          [x y])))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  monads.core.StateTransformer
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def vect-state (m/state-t vector))
  (defn state-t-f [n]
    (vect-state (inc n)))

  (defn state-t-g [n]
    (vect-state (+ n 5)))

  (deftest first-law-state-t
    (let [mv1 (m/bind (vect-state 10) state-t-f)
          mv2 (state-t-f 10)]
      (is (= (mv1 {}) (mv2 {})))))

  (deftest second-law-state-t
    (let [mv1 (m/bind (vect-state 10) vect-state)
          mv2 (vect-state 10)]
      (is (= (mv1 :state-t) (mv2 :state-t)))))

  (deftest third-law-state-t
    (let [mv1 (m/bind (m/bind (vect-state 4) state-t-f) state-t-g)
          mv2 (m/bind (vect-state 4)
                      (fn [x]
                        (m/bind (state-t-f x) state-t-g)))]
      (is (= (mv1 :state-t) (mv2 :state-t)))))

  (deftest zero-law-state-t
    (is (= [] ((m/zero (vect-state nil)) :state)))
    (is (= ((m/bind (m/zero (vect-state nil)) state-t-f) :state)
           []))
    (is (= ((m/bind (vect-state 4) (constantly (m/zero (vect-state nil)))) :state)
           []))
    (is (= ((m/plus [(vect-state 5) (m/zero (vect-state nil))]) :state)
           ((vect-state 5) :state)))
    (is (= ((m/plus [(m/zero (vect-state nil)) (vect-state 4)]) :state)
           ((vect-state 4) :state))))

  (deftest do-state-t
    (is (= []
           ((m/do vect-state
                  [:when false]
                  :something)
            :state)))

    (is (= [[[10 15] :state]]
           ((m/do vect-state
                  [x (state-t-f 9)
                   y (state-t-g x)]
                  [x y])
            :state))))

  (def parse-m (m/state-t m/maybe))

  (deftest test-do
    (is (= [19 {:val 19}]
           @((m/do parse-m
                   [_ (m/set-val :val 19)]
                   19)
             {})))

    (let [tinc #(vector (inc %))]
      (is ( = [[1 2 3] [3 4 5]]
              (m/do vector
                    [a (vec (range 5))
                     :when (odd? a)
                     x (tinc a)
                     y (tinc x)]
                    [a x y])))))

  (deftest test-state-maybe-1
    (let [test-m (m/state-t m/maybe)]
      (is (= [:test :state]
             @((m/plus* [(test-m nil)
                         (m/do test-m
                               [:when false]
                               (throw (Exception. "Really should not be thrown")))
                         (test-m :test)
                         (m/do test-m
                               [_ (test-m 10)]
                               (throw (Exception. "Should not be thrown")))])
               :state)))))

  (deftest test-state-maybe-2
    (let [test-m (m/state-t m/maybe)]
      (is (= [:test :state]
             @((m/plus* [(test-m nil)
                         (test-m :test)
                         (m/do test-m
                               [_ (test-m 10)]
                               (throw (Exception. "Should not be thrown")))])
               :state)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  monads.core.WriterTransformer
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def vect-writer (m/writer-t hash-set []))
  (defn writer-t-f [n]
    (vect-writer (inc n)))

  (defn writer-t-g [n]
    (vect-writer (+ n 5)))

  (deftest first-law-writer-t
    (is (= @(first @(m/bind (vect-writer 10) writer-t-f))
           @(first @(writer-t-f 10)))))

  (deftest second-law-writer-t
    (is (= @(first @(m/bind (vect-writer 10) vect-writer))
           @(first @(vect-writer 10)))))

  (deftest third-law-writer-t
    (is (= @(first @(m/bind (m/bind (vect-writer 4) writer-t-f) writer-t-g))
           @(first @(m/bind (vect-writer 4)
                            (fn [x]
                              (m/bind (writer-t-f x) writer-t-g)))))))

  (deftest zero-law-writer-t
    (is (= #{} @(m/zero (vect-writer nil))))
    (is (= @(m/bind (m/zero (vect-writer nil)) writer-t-f)
           @(m/zero (vect-writer nil))))
    (is (= @(m/bind (vect-writer 4) (constantly (m/zero (vect-writer nil))))
           @(m/zero (vect-writer nil))))
    (is (= @(first @(m/plus [(vect-writer 4) (m/zero (vect-writer nil))]))
           @(first @(vect-writer 4))))
    (is (= @(first @(m/plus [(m/zero (vect-writer nil)) (vect-writer 4)]))
           @(first @(vect-writer 4)))))

  (deftest do-writer-t
    (is (= @(first @(vect-writer [10 15]))
           @(first @(m/do vect-writer
                          [x (writer-t-f 9)
                           y (writer-t-g x)]
                          [x y])))))

  (deftest test-hash-set-writer
    (let [test-m (m/writer-t hash-set [])
          writer-m (m/writer [])
          write-msg (fn [msg]
                      (WriterTransformer. hash-set
                                          (hash-set ((m/writer [msg]) nil))
                                          writer-m))
          listen-msgs (fn [mv]
                        (WriterTransformer. hash-set
                                            (->> @mv
                                                 (map #(m/listen %))
                                                 set)
                                            writer-m))
          censor-msgs (fn [f mv]
                        (WriterTransformer. hash-set
                                            (->> @mv
                                                 (map #(m/censor f %))
                                                 set)
                                            writer-m))]

      (is (= [nil [:msg1]] @(first @(write-msg :msg1))))

      (is (= [[nil [:msg3]] [:msg3]] @(first @(listen-msgs (write-msg :msg3)))))

      (is (= [[nil [nil [:msg3]] nil] [:msg1 :msg3 :msg2 :msg4]]
             (->> (m/seq [(write-msg :msg1)
                          (listen-msgs (write-msg :msg3))
                          (write-msg :msg2)])
                  (censor-msgs #(conj % :msg4))
                  deref
                  first
                  deref)))

      (is (= #{[5 [:msg3]] [nil [:msg1 :msg3]]}
             (->> (m/plus [(write-msg :msg1)
                           (m/zero (test-m nil))
                           (m/zero (write-msg :msg2))
                           (test-m 5)])
                  (censor-msgs #(conj % :msg3))
                  deref
                  (map deref)
                  set)))))

  (deftest test-state-writer-maybe
    (let [test-m (m/state-t (m/writer-t m/maybe []))
          writer-m (m/writer-t m/maybe [])
          write-msg (fn [msg]
                      (StateTransformer. writer-m
                                         nil
                                         ((m/state-t (m/writer-t m/maybe [msg])) nil)
                                         (constantly (test-m nil))
                                         nil))
          listen-msgs (fn [mv]
                        (StateTransformer. writer-m
                                           nil
                                           (fn [s]
                                             (let [[[_ s] msgs] @@@(mv s)]
                                               ((m/writer-t m/maybe msgs) [msgs s])))
                                           (fn [v]
                                             (test-m v))
                                           nil))
          censor-msgs (fn [f mv]
                        (StateTransformer. writer-m
                                           nil
                                           (fn [s]
                                             (let [[[v s] msgs] @@@(mv s)]
                                               ((m/writer-t m/maybe (f msgs)) [[v msgs] s])))
                                           (fn [v]
                                             (test-m v))
                                           nil))]
      (is (= [[nil :state] [:msg]]
             @@@((write-msg :msg) :state)))

      (is (= [[:result :state] [:msg]]
             @@@((m/bind (write-msg :msg)
                         (fn [x]
                           (is (nil? x))
                           (test-m :result)))
                 :state)))
      (is (= [[[nil nil] :state] [:msg1 :msg2]]
             @@@((m/seq [(write-msg :msg1)
                         (write-msg :msg2)])
                 :state)))
      (is (= [[nil :state] [:msg1]]
             @@@((m/plus [(write-msg :msg1)
                          (write-msg :msg2)])
                 :state)))
      (is (= [[nil :state] [:msg2]]
             @@@((m/plus [(m/zero (test-m nil))
                          (write-msg :msg2)])
                 :state)))

      (is (= [[[:msg3] :state] [:msg3]]
             @@@((listen-msgs (write-msg :msg3)) :state)))

      (is (= [[[[nil [:msg3] nil] [:msg1 :msg3 :msg2]] :state] [:msg1 :msg3 :msg2 :msg4]]
             @@@((->> (m/seq [(write-msg :msg1)
                              (listen-msgs (write-msg :msg3))
                              (write-msg :msg2)])
                      (censor-msgs #(conj % :msg4)))
                 :state)))

      (is (= [[[nil [:msg1]] :state] [:msg1 :msg3]]
             @@@((->> (m/plus [(m/zero (test-m nil))
                               (m/zero (write-msg :msg2))
                               (write-msg :msg1)])
                      (censor-msgs #(conj % :msg3)))
                 :state)))))
  )
