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
  (is (= (m/lazy-seq* 1 2 3)
         (m/lazy-seq [1 2 3])
         (lazy-seq [1 2 3]))))

(deftest monads-core-and-clojure-core-hash-set-factory-equiv
  (is (= (m/hash-set 1 2 3)
         (hash-set 1 2 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest monads-core-seq*-and-clojure-core-seq-equiv
  (is (= (m/seq* (list 1 2 3))
         (seq (list 1 2 3)))))

(deftest monads-core-map*-and-clojure-core-map-equiv
  (is (= (m/map* identity [1 2 3])
         (map identity [1 2 3]))))

(deftest lazy-concat-laziness
  (is (= clojure.lang.LazySeq
         (class (m/lazy-concat (m/lazy-seq* (/ 1 0)
                                            (/ 1 0))
                               (m/lazy-seq* (m/lazy-seq* (/ 1 0)
                                                         (/ 1 0)
                                                         (/ 1 0))))))))

(deftest lazy-concat-return
  (is (= (m/lazy-seq* (/ 1 1) (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5))
         (m/lazy-seq [(/ 1 1) (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)])
         (lazy-seq [(/ 1 1) (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)])
         (m/lazy-concat (m/lazy-seq* (/ 1 1)
                                     (/ 1 2))
                        (m/lazy-seq* (m/lazy-seq* (/ 1 3)
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

(deftest zero-laws-list
  (is (= (m/bind zero-val-list list-f)
         zero-val-list))
  (is (= (m/bind (do-result-list 4) (constantly zero-val-list))
         zero-val-list)))

(comment "Zero Laws for monads.core/plus and monads.core/plus* are
          defined in a section below the tests for those monad
          functions")

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

(deftest zero-laws-vector
  (is (= (m/bind zero-val-vector vector-f)
         zero-val-vector))
  (is (= (m/bind (do-result-vector 4) (constantly zero-val-vector))
         zero-val-vector)))

(comment "Zero Laws for monads.core/plus and monads.core/plus* are
          defined in a section below the tests for those monad
          functions")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  clojure.lang.LazySeq
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lazy-seq-f [n]
  (m/lazy-seq* (inc n)))

(defn lazy-seq-g [n]
  (m/lazy-seq* (+ n 5)))

(def do-result-lazy-seq (partial m/do-result (m/lazy-seq* [nil])))
(def zero-val-lazy-seq (m/zero (m/lazy-seq* [nil])))

(deftest do-result-and-lazy-seq-factory-func-equiv
  (is (= (do-result-lazy-seq [nil])
         (m/lazy-seq* [nil]))))

(deftest first-law-lazy-seq
  (is (= (m/bind (do-result-lazy-seq 10) lazy-seq-f)
         (lazy-seq-f 10))))

(deftest second-law-lazy-seq
  (is (= (m/bind (do-result-lazy-seq 10) do-result-lazy-seq)
         (do-result-lazy-seq 10))))

(deftest third-law-lazy-seq
  (is (= (m/bind (m/bind (m/lazy-seq* 4 9) lazy-seq-f) lazy-seq-g)
         (m/bind (m/lazy-seq* 4 9) (fn [x]
                                     (m/bind (lazy-seq-f x) lazy-seq-g))))))

(deftest zero-laws-lazy-seq
  (is (= (m/bind zero-val-lazy-seq lazy-seq-f)
         zero-val-lazy-seq))
  (is (= (m/bind (do-result-lazy-seq 4) (constantly zero-val-lazy-seq))
         zero-val-lazy-seq)))

(comment "Zero Laws for monads.core/plus and monads.core/plus* are
          defined in a section below the tests for those monad
          functions")

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

(deftest zero-laws-set
  (is (= (m/bind zero-val-set set-f)
         zero-val-set))
  (is (= (m/bind (do-result-set 4) (constantly zero-val-set))
         zero-val-set)))

(comment "Zero Laws for monads.core/plus and monads.core/plus* are
          defined in a section below the tests for those monad
          functions")

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
;; procol method do-result, as implemented for class Maybe, does
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

(deftest zero-laws-maybe
  (is (= (m/bind zero-val-maybe maybe-f)
         zero-val-maybe))
  (is (= (m/bind (do-result-maybe 4) (constantly zero-val-maybe))
         zero-val-maybe)))

(comment "Zero Laws for monads.core/plus and monads.core/plus* are
          defined in a section below the tests for those monad
          functions")

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

(deftest update-state
  (is (= [:state :new-state]
         ((m/update-state (constantly :new-state)) :state))))

(deftest get-val
  (is (= [17 {:a 17}]
         ((m/get-val :a) {:a 17}))))

(deftest set-val
  (is (= [17 {:a 12}]
         ((m/set-val :a 12) {:a 17}))))

(deftest update-val
  (is (= [5 {:a 19}]
         ((m/update-val :a + 14) {:a 5}))))

(deftest get-in-val
  (let [state {:a {:b 1} :c {:d {:e 2}}}]
    (are [expected args] (is (= expected ((apply m/get-in-val args) state)))
         [1 state]      [[:a :b]]
         [:def state]   [[:z] :def]
         [nil state]    [[:a :b :c]]
         [2 state]      [[:c :d :e]]
         [{:b 1} state] [[:a]])))

(deftest assoc-in-val
  (is (= [nil {:a {:b {:c 9}}}]
         ((m/assoc-in-val [:a :b :c] 9) {}))))

(deftest update-in-val
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

(deftest write
  (is (= [nil #{:written}]
         @(m/write test-writer :written))))

(deftest listen
  (is (= [[nil #{:written}] #{:written}]
         @(m/listen (m/write test-writer :written)))))

(deftest censor
  (is (= [nil #{:new-written}]
         @(m/censor (constantly #{:new-written})
                    (m/write test-writer :written)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Monadic functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  monads.core/do
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest do-basic
  (is (= (m/bind (range 5)
                 (fn [x]
                   (m/bind (range 3)
                           (fn [y]
                             (m/lazy-seq* (+ x y))))))
         (m/do lazy-seq
               [x (range 5)
                y (range 3)]
               (+ x y)))))

(deftest do-when
  (is (= (m/zero (lazy-seq))
         (m/bind (range 5)
                 (fn [x]
                   (m/bind (range 3)
                           (fn [y]
                             (if (> x 1000)
                               (m/lazy-seq* (+ x y))
                               (m/zero (lazy-seq)))))))
         (m/do lazy-seq
               [x (range 5)
                y (range 3)
                :when (> x 1000)]
               (+ x y)))))

(deftest do-let
  (is (= (m/bind (range 5)
                 (fn [x]
                   (let [z 100]
                     (m/bind (range 3)
                             (fn [y]
                               (m/lazy-seq* (+ x y z)))))))
         (m/do lazy-seq
               [x (range 5)
                :let [z 100]
                y (range 3)]
               (+ x y z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  monads.core/plus, monads.core/plus*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest plus-list
  (is (= (m/plus [(list 1) '() (list 2) (list 3) (list)])
         (list 1 2 3))))

(deftest plus*-list
  (is (= (m/plus* [(list 1) '() (list 2) (list 3) (list)])
         (list 1 2 3))))

(deftest plus-list-empty
  (is (= (m/plus [(list) (list 1) '() (list 2) (list 3) (list)])
         (list 1 2 3))))

(deftest plus*-list-empty
  (is (= (m/plus* [(list) (list 1) '() (list 2) (list 3) (list)])
         (list 1 2 3))))

(deftest plus-vector
  (is (= (m/plus [[1] [] [2] [3] []])
         [1 2 3])))

(deftest plus*-vector
  (is (= (m/plus* [[1] [] [2] [3] []])
         [1 2 3])))

(deftest plus-lazy-seq
  (is (= (m/plus [(m/lazy-seq* 1) (lazy-seq) (m/lazy-seq* 2) (m/lazy-seq* 3) (m/lazy-seq*)])
         (m/lazy-seq* 1 2 3))))

(deftest plus*-lazy-seq
  (is (= (m/plus* [(m/lazy-seq* 1) (lazy-seq) (m/lazy-seq* 2) (m/lazy-seq* 3) (m/lazy-seq*)])
         (m/lazy-seq* 1 2 3))))

(deftest plus-hash-set
  (is (= (m/plus [#{1} #{} #{2 3} #{4 5 6} #{}])
         #{1 2 3 4 5 6})))

(deftest plus*-hash-set
  (is (= (m/plus* [#{1} #{} #{2 3} #{4 5 6} #{}])
         #{1 2 3 4 5 6})))

(deftest plus-maybe
  (is (= @(m/plus [m/maybe-zero-val (m/maybe nil) (m/maybe 1) (m/maybe 2)])
         @(m/maybe 1))))

(deftest plus-maybe-not-lazy
  (is (thrown-with-msg?
        Exception #"Should be thrown"
        @(m/plus [(m/maybe :test)
                  (m/do m/maybe
                        [_ (m/maybe 10)]
                        (throw (Exception. "Should be thrown")))]))))

(deftest plus*-maybe-zero-first
  (is (= @(m/plus* [(m/maybe nil)
                    (m/maybe :test)
                    (m/do m/maybe
                          [_ (m/maybe 10)
                           _ (m/maybe (/ 1 0))]
                          (throw (Exception. "Should not be thrown")))])
         @(m/maybe :test))))

(deftest plus*-maybe-not-zero-first
  (is (= @(m/plus* [(m/maybe :test)
                    (m/do m/maybe
                          [_ (m/maybe 10)
                           _ (m/maybe (/ 1 0))]
                          (throw (Exception. "Should not be thrown")))])
         @(m/maybe :test))))

(comment "Tests for monads.core/plus and monads.core/plus* for the
          various monad transformers are defined in the sections below
          for their respective transformer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  monads.core/comprehend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest comprehend-list
  (let [do-list
        (m/do list
              [x (list 1 2)
               y (list 3 4)
               z (list 5 6)]
              (apply list (map inc
                               [x y z])))
        comprehend-list
        (m/comprehend
         #(apply list ((partial map inc) %))
         [(list 1 2) (list 3 4) (list 5 6)])]
    (is (= do-list
           comprehend-list))
    (is (= clojure.lang.PersistentList
           (class do-list)
           (class comprehend-list)))
    (is (= clojure.lang.PersistentList
           (class (first do-list))
           (class (first comprehend-list))))))

(deftest comprehend-vector
  (let [comprehend-vector
        (m/comprehend
         #(vec ((partial map inc) %))
         [[1 2] [3 4] [5 6]])]
    (is (= clojure.lang.PersistentVector
           (class comprehend-vector)))
    (is (= clojure.lang.PersistentVector
           (class (first comprehend-vector))))))

(deftest comprehend-state
  (let [do-state
        (m/do m/state
              [x (m/state 1)
               y (m/state 2)
               z (m/state 3)]
              (vec (map inc
                        [x y z])))
        comprehend-state
        (m/comprehend
         #(vec ((partial map inc) %))
         [(m/state 1) (m/state 2) (m/state 3)])]
    (is (= (do-state
            :state)
           (comprehend-state
            :state)))
    (is (= monads.core.State
           (class do-state)
           (class comprehend-state)))
    (is (= clojure.lang.PersistentVector
           (class (first (do-state :state)))
           (class (first (comprehend-state :state)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  monads.core/seq
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest seq-lazy-seq
  (let [seq-lazy-seq (m/seq [(m/lazy-seq* 3 5) (m/lazy-seq* :a :b)])]
    (is (= (m/lazy-seq* (list 3 :a) (list 3 :b) (list 5 :a) (list 5 :b))
           seq-lazy-seq))
    (is (= clojure.lang.LazySeq
           (class seq-lazy-seq)))
    (is (= clojure.lang.PersistentList
           (class (first seq-lazy-seq))))))

(deftest seq-hash-set-empty
  (let [seq-hash-set-empty (m/seq hash-set [])]
    (is (= #{(list)}
           seq-hash-set-empty))
    (is (= clojure.lang.PersistentHashSet
           (class seq-hash-set-empty)))
    (is (= clojure.lang.PersistentList$EmptyList
           (class (first seq-hash-set-empty))))))

(deftest seq-throws-on-empty-without-factory
  (is (thrown-with-msg?
        AssertionError #"At least one monadic value is required.*"
        (m/seq []))))

(comment

  "The following will throw runtime excpetions since
   clojure.core/lazy-seq, monads.core/lazy-seq and
   monads.core/lazy-seq* are macros."

  (m/seq lazy-seq [])
  (m/seq m/lazy-seq [])
  (m/seq m/lazy-seq* [])

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  monads.core/lift
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest monadic-lift
  (let [lifted-+ (m/lift +)]
    (is (= [6]
           (apply lifted-+ (map vector (range 4)))))
    (is (= [6 :state]
           ((apply lifted-+ (map m/state (range 4))) :state))))
  (let [lifted-list (m/lift list)]
    (is (= [(list 0 1 2 3)]
           (apply lifted-list (map vector (range 4)))))
    (is (= [(list 0 1 2 3) :state]
           ((apply lifted-list (map m/state (range 4))) :state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  monads.core/join
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest monadic-join
  (is (= (list 1)
         (m/join (list (list 1)))))
  (is (= [1]
         (m/join [[1]])))
  (is (= (m/lazy-seq* 1)
         (m/join (m/lazy-seq* (m/lazy-seq* 1)))))
  (is (= (hash-set 1)
         (m/join (hash-set (hash-set 1)))))
  (is (= @(m/maybe 1)
         @(m/join (m/maybe (m/maybe 1)))))
  (is (= ((m/state 1) :state)
         ((m/join (m/state (m/state 1))) :state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  monads.core/fmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest monadic-fmap
  (is (= (list 6)
         (m/fmap inc (list 5))))
  (is (= @(m/maybe 6)
         @(m/fmap inc (m/maybe 5))))
  (is (= @(m/maybe nil)
         @(m/fmap inc m/maybe-zero-val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  monads.core/map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest monadic-map
  (let [map-vec (m/map #(vector % (inc %)) (list 1 3 5))]
    (is (= (m/seq (map #(vector % (inc %)) (list 1 3 5)))
           map-vec))
    (is (= clojure.lang.PersistentVector
           (class map-vec)))
    (is (= clojure.lang.PersistentList
           (class (first map-vec))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  monads.core/chain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest monadic-chain
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Monad Zero Laws for monads.core/plus and monads.core/plus*
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest zero-laws-for-plus-list
  (is (= (m/plus [(list 5 6) zero-val-list])
         (list 5 6)))
  (is (= (m/plus [zero-val-list (list 5 6)])
         (list 5 6)))
  (is (= (m/plus* [(list 5 6) zero-val-list])
         (list 5 6)))
  (is (= (m/plus* [zero-val-list (list 5 6)])
         (list 5 6))))

(deftest zero-laws-for-plus-vector
  (is (= (m/plus [(vector 5 6) zero-val-vector])
         (vector 5 6)))
  (is (= (m/plus [zero-val-vector (vector 5 6)])
         (vector 5 6)))
  (is (= (m/plus* [(vector 5 6) zero-val-vector])
         (vector 5 6)))
  (is (= (m/plus* [zero-val-vector (vector 5 6)])
         (vector 5 6))))

(deftest zero-laws-for-plus-lazy-seq
  (is (= (m/plus [(m/lazy-seq* 5 6) zero-val-lazy-seq])
         (m/lazy-seq* 5 6)))
  (is (= (m/plus [zero-val-lazy-seq (m/lazy-seq* 5 6)])
         (m/lazy-seq* 5 6)))
  (is (= (m/plus* [(m/lazy-seq* 5 6) zero-val-lazy-seq])
         (m/lazy-seq* 5 6)))
  (is (= (m/plus* [zero-val-lazy-seq (m/lazy-seq* 5 6)])
         (m/lazy-seq* 5 6))))

(deftest zero-laws-for-plus-set
  (is (= (m/plus [(hash-set 5 6) zero-val-set])
         (hash-set 5 6)))
  (is (= (m/plus [zero-val-set (hash-set 5 6)])
         (hash-set 5 6)))
  (is (= (m/plus* [(hash-set 5 6) zero-val-set])
         (hash-set 5 6)))
  (is (= (m/plus* [zero-val-set (hash-set 5 6)])
         (hash-set 5 6))))

(deftest zero-laws-for-plus-maybe
  (is (= @(m/plus [(m/maybe 6) zero-val-maybe])
         @(m/maybe 6)))
  (is (= @(m/plus [zero-val-maybe (m/maybe 6)])
         @(m/maybe 6)))
  (is (= @(m/plus* [(m/maybe 6) zero-val-maybe])
         @(m/maybe 6)))
  (is (= @(m/plus* [zero-val-maybe (m/maybe 6)])
         @(m/maybe 6))))

(comment "Zero Laws tests for the various monad transformers are defined
          in the sections below for their respective transformer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.ListTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def set-list (m/list-t hash-set))

(def do-result-set-list (partial m/do-result (set-list [nil])))
(def zero-val-set-list (m/zero (set-list [nil])))

(defn list-t-f [& ns]
  (apply set-list (map inc ns)))

(defn list-t-g [& ns]
  (apply set-list (map #(+ % 5) ns)))

(deftest first-law-list-t
  (is (= @(m/bind (do-result-set-list 10) list-t-f)
         @(list-t-f 10))))

(deftest first-law-list-t-factory
  (is (= @(m/bind (set-list 10 11 12) list-t-f)
         @(list-t-f 10 11 12))))

(deftest second-law-list-t
  (is (= @(m/bind (do-result-set-list 10) do-result-set-list)
         @(do-result-set-list 10))))

(deftest second-law-list-t-factory
  (is (= @(m/bind (set-list 10 11 12) set-list)
         @(set-list 10 11 12))))

(deftest third-law-list-t
  (is (= @(m/bind (m/bind (set-list 4 5 6) list-t-f) list-t-g)
         @(m/bind (set-list 4 5 6)
                  (fn [x]
                    (m/bind (list-t-f x) list-t-g))))))

(deftest plus-list-t
  (let [plus-list-t @(m/plus [(set-list 1 2) (set-list) (set-list 3 4)])]
    (is (= #{(list 1 2 3 4)}
           plus-list-t))
    (is (= clojure.lang.PersistentHashSet
           (class plus-list-t)))
    (is (= clojure.lang.PersistentList
           (class (first plus-list-t))))))

(deftest zero-laws-list-t
  (is (= #{(list)} @zero-val-set-list))
  (is (= @(m/bind zero-val-set-list list-t-f)
         @zero-val-set-list))
  (is (= @(m/bind (set-list 4 5 6) (constantly zero-val-set-list))
         @zero-val-set-list))
  (is (= @(m/plus [(set-list 4 5 6) zero-val-set-list])
         @(set-list 4 5 6)))
  (is (= @(m/plus [zero-val-set-list (set-list 4 5 6)])
         @(set-list 4 5 6))))

(deftest do-list-t
  (let [do-list-t
        @(m/do set-list
               [x (list-t-f 9 8 7)
                y (list-t-g x)]
               [x y])]
    (is (= #{(list [10 15] [9 14] [8 13])}
           do-list-t))
    (is (= clojure.lang.PersistentHashSet
           (class do-list-t)))
    (is (= clojure.lang.PersistentList
           (class (first do-list-t))))
    (is (= clojure.lang.PersistentVector
           (class (first (first do-list-t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.VectorTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def set-vec (m/vector-t hash-set))

(def do-result-set-vec (partial m/do-result (set-vec [nil])))
(def zero-val-set-vec (m/zero (set-vec [nil])))

(defn vector-t-f [& ns]
  (apply set-vec (map inc ns)))

(defn vector-t-g [& ns]
  (apply set-vec (map #(+ % 5) ns)))

(deftest first-law-vector-t
  (is (= @(m/bind (do-result-set-vec 10) vector-t-f)
         @(vector-t-f 10))))

(deftest first-law-vector-factory
  (is (= @(m/bind (set-vec 10 11 12) vector-t-f)
         @(vector-t-f 10 11 12))))

(deftest second-law-vector-t
  (is (= @(m/bind (do-result-set-vec 10) do-result-set-vec)
         @(set-vec 10))))

(deftest second-law-vector-t-factory
  (is (= @(m/bind (set-vec 10 11 12) set-vec)
         @(set-vec 10 11 12))))

(deftest third-law-vector-t
  (is (= @(m/bind (m/bind (set-vec 4 5 6) vector-t-f) vector-t-g)
         @(m/bind (set-vec 4 5 6)
                  (fn [x]
                    (m/bind (vector-t-f x) vector-t-g))))))

(deftest plus-vector-t
  (let [plus-vector-t @(m/plus [(set-vec 1 2) (set-vec) (set-vec 3 4)])]
    (is (= #{[1 2 3 4]}
           plus-vector-t))
    (is (= clojure.lang.PersistentHashSet
           (class plus-vector-t)))
    (is (= clojure.lang.PersistentVector
           (class (first plus-vector-t))))))

(deftest zero-laws-vector-t
  (is (= #{[]} @zero-val-set-vec))
  (is (= @(m/bind zero-val-set-vec vector-t-f)
         @zero-val-set-vec))
  (is (= @(m/bind (set-vec 4 5 6) (constantly zero-val-set-vec))
         @zero-val-set-vec))
  (is (= @(m/plus [(set-vec 4 5 6) zero-val-set-vec])
         @(set-vec 4 5 6)))
  (is (= @(m/plus [zero-val-set-vec (set-vec 4 5 6)])
         @(set-vec 4 5 6))))

(deftest do-vector-t
  (let [do-vector-t
        @(m/do set-vec
               [x (vector-t-f 9 8 7)
                y (vector-t-g x)]
               (list x y))]
    (is (= #{[(list 10 15) (list 9 14) (list 8 13)]}
           do-vector-t))
    (is (= clojure.lang.PersistentHashSet
           (class do-vector-t)))
    (is (= clojure.lang.PersistentVector
           (class (first do-vector-t))))
    (is (= clojure.lang.PersistentList
           (class (first (first do-vector-t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.LazySeqTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def set-lazy-seq (m/lazy-seq-t hash-set))

(def do-result-set-lazy-seq (partial m/do-result (set-lazy-seq [nil])))
(def zero-val-set-lazy-seq (m/zero (set-lazy-seq [nil])))

(defn lazy-seq-t-f [& ns]
  (apply set-lazy-seq (map inc ns)))

(defn lazy-seq-t-g [& ns]
  (apply set-lazy-seq (map #(+ % 5) ns)))

(deftest first-law-lazy-seq-t
  (is (= @(m/bind (do-result-set-lazy-seq 10) lazy-seq-t-f)
         @(lazy-seq-t-f 10))))

(deftest first-law-lazy-seq-t-factory
  (is (= @(m/bind (set-lazy-seq 10 11 12) lazy-seq-t-f)
         @(lazy-seq-t-f 10 11 12))))

(deftest second-law-lazy-seq-t
  (is (= @(m/bind (do-result-set-lazy-seq 10) do-result-set-lazy-seq)
         @(do-result-set-lazy-seq 10))))

(deftest second-law-lazy-seq-t-factory
  (is (= @(m/bind (set-lazy-seq 10 11 12) set-lazy-seq)
         @(set-lazy-seq 10 11 12))))

(deftest third-law-lazy-seq-t
  (is (= @(m/bind (m/bind (set-lazy-seq 4 5 6) lazy-seq-t-f) lazy-seq-t-g)
         @(m/bind (set-lazy-seq 4 5 6)
                  (fn [x]
                    (m/bind (lazy-seq-t-f x) lazy-seq-t-g))))))

(deftest plus-lazy-seq-t
  (let [plus-lazy-seq-t @(m/plus [(set-lazy-seq 1 2) (set-lazy-seq) (set-lazy-seq 3 4)])]
    (is (= #{(m/lazy-seq* 1 2 3 4)}
           plus-lazy-seq-t))
    (is (= clojure.lang.PersistentHashSet
           (class plus-lazy-seq-t)))
    (is (= clojure.lang.LazySeq
           (class (first plus-lazy-seq-t))))))

(deftest zero-laws-lazy-seq-t
  (is (= #{(lazy-seq)} @zero-val-set-lazy-seq))
  (is (= @(m/bind zero-val-set-lazy-seq lazy-seq-t-f)
         @zero-val-set-lazy-seq))
  (is (= @(m/bind (set-lazy-seq 4 5 6) (constantly zero-val-set-lazy-seq))
         @zero-val-set-lazy-seq))
  (is (= @(m/plus [(set-lazy-seq 4 5 6) zero-val-set-lazy-seq])
         @(set-lazy-seq 4 5 6)))
  (is (= @(m/plus [zero-val-set-lazy-seq (set-lazy-seq 4 5 6)])
         @(set-lazy-seq 4 5 6))))

(deftest do-lazy-seq-t
  (let [do-lazy-seq-t
        @(m/do set-lazy-seq
               [x (lazy-seq-t-f 9 8 7)
                y (lazy-seq-t-g x)]
               [x y])]
    (is (= #{(list [10 15] [9 14] [8 13])}
           do-lazy-seq-t))
    (is (= clojure.lang.PersistentHashSet
           (class do-lazy-seq-t)))
    (is (= clojure.lang.LazySeq
           (class (first do-lazy-seq-t))))
    (is (= clojure.lang.PersistentVector
           (class (first (first do-lazy-seq-t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.SetTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def vec-set (m/set-t vector))

(def do-result-vec-set (partial m/do-result (vec-set [nil])))
(def zero-val-vec-set (m/zero (vec-set [nil])))

(defn set-t-f [& ns]
  (apply vec-set (map inc ns)))

(defn set-t-g [& ns]
  (apply vec-set (map #(+ % 5) ns)))

(deftest first-law-set-t
  (is (= @(m/bind (do-result-vec-set 10) set-t-f)
         @(set-t-f 10))))

(deftest first-law-vector-factory
  (is (= @(m/bind (vec-set 10 11 12) set-t-f)
         @(set-t-f 10 11 12))))

(deftest second-law-set-t
  (is (= @(m/bind (do-result-vec-set 10) do-result-vec-set)
         @(vec-set 10))))

(deftest second-law-set-t-factory
  (is (= @(m/bind (vec-set 10 11 12) vec-set)
         @(vec-set 10 11 12))))

(deftest third-law-set-t
  (is (= @(m/bind (m/bind (vec-set 4 5 6) set-t-f) set-t-g)
         @(m/bind (vec-set 4 5 6)
                  (fn [x]
                    (m/bind (set-t-f x) set-t-g))))))

(deftest plus-set-t
  (let [plus-set-t @(m/plus [(vec-set 1 2) (vec-set 1 2) (vec-set) (vec-set 2 3 4) (vec-set 2 3 4)])]
    (is (= [#{1 2 3 4}]
           plus-set-t))
    (is (= clojure.lang.PersistentVector
           (class plus-set-t)))
    (is (= clojure.lang.PersistentHashSet
           (class (first plus-set-t))))))

(deftest zero-laws-set-t
  (is (= [#{}] @zero-val-vec-set))
  (is (= @(m/bind zero-val-vec-set set-t-f)
         @zero-val-vec-set))
  (is (= @(m/bind (vec-set 4 5 6) (constantly zero-val-vec-set))
         @zero-val-vec-set))
  (is (= @(m/plus [(vec-set 4 5 6) zero-val-vec-set])
         @(vec-set 4 5 6)))
  (is (= @(m/plus [zero-val-vec-set (vec-set 4 5 6)])
         @(vec-set 4 5 6))))

(deftest do-set-t
  (let [do-set-t
        @(m/do vec-set
               [x (set-t-f 9 8 7)
                y (set-t-g x)]
               (list x y))]
    (is (= [#{(list 10 15) (list 9 14) (list 8 13)}]
           do-set-t))
    (is (= clojure.lang.PersistentVector
           (class do-set-t)))
    (is (= clojure.lang.PersistentHashSet
           (class (first do-set-t))))
    (is (= clojure.lang.PersistentList
           (class (first (first do-set-t)))))))

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

(deftest zero-laws-maybe-t
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
;;  The tests below have been disabled, and are in the process of being
;;  reworked and re-enabled in light of monads.core/check-return-type
;;  and other modifications to the protocol-monads library.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

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

  (deftest zero-laws-state-t
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

  (deftest zero-laws-writer-t
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
