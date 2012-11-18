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
  (let [seq* (ns-resolve 'monads.core 'seq*)]
    (is (= (seq* (list 1 2 3))
           (seq (list 1 2 3))))))

(deftest monads-core-map*-and-clojure-core-map-equiv
  (let [map* (ns-resolve 'monads.core 'map*)]
    (is (= (map* identity [1 2 3])
           (map identity [1 2 3])))))

(deftest lazy-concat-laziness
  (let [lazy-concat (ns-resolve 'monads.core 'lazy-concat)]
    (is (= clojure.lang.LazySeq
           (class (lazy-concat (m/lazy-seq* (/ 1 0)
                                            (/ 1 0))
                               (m/lazy-seq* (m/lazy-seq* (/ 1 0)
                                                         (/ 1 0)
                                                         (/ 1 0)))))))
    (is (= (m/lazy-seq* (/ 1 1) (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5))
           (m/lazy-seq [(/ 1 1) (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)])
           (lazy-seq [(/ 1 1) (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)])
           (lazy-concat (m/lazy-seq* (/ 1 1)
                                     (/ 1 2))
                        (m/lazy-seq* (m/lazy-seq* (/ 1 3)
                                                  (/ 1 4)
                                                  (/ 1 5))))))))

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

(deftest maybe-equality
  (is (= (m/maybe 1) (m/maybe 1)))
  (is (= @(m/maybe 1) @(m/maybe 1)))
  (is (= (m/maybe {:a 1}) (m/maybe {:a 1})))
  (is (not= (m/maybe 1) (m/maybe :1)))
  (is (not= @(m/maybe 1) @(m/maybe :1)))
  (is (not= (m/maybe 1) (m/maybe 2)))
  (is (not= (m/maybe {:a 1}) (m/maybe {:a 2}))))

(def do-result-maybe (partial m/do-result (m/maybe [nil])))
(def zero-val-maybe (m/zero (m/maybe [nil])))

(deftest do-result-and-maybe-factory-func-equiv
  (is (= (do-result-maybe [nil])
         (m/maybe [nil])))
  (is (= @(do-result-maybe [nil])
         @(m/maybe [nil]))))

(deftest do-result-and-maybe-factory-func-not-equiv-for-*Nothing*
  (is (not= (do-result-maybe nil)
            (m/maybe nil)))
  (binding [m/*Nothing* {:some ['value]}]
    (is (not= (do-result-maybe {:some ['value]})
              (m/maybe {:some ['value]})))))

(deftest zero-val-maybe-from-maybe-factory-func
  (is (= m/Nothing
         zero-val-maybe
         (m/maybe nil)))
  (binding [m/*Nothing* {:some ['value]}]
    (is (= m/Nothing
           zero-val-maybe
           (m/maybe {:some ['value]})))))

(deftest zero-val-maybe-deref-dynamic
  (is (= nil
         m/*Nothing*
         @m/Nothing
         @(m/maybe nil)
         @(do-result-maybe nil)))
  (binding [m/*Nothing* {:some ['value]}]
    (is (= {:some ['value]}
           m/*Nothing*
           @m/Nothing
           @(m/maybe {:some ['value]})
           @(do-result-maybe {:some ['value]})))))

(defn maybe-f [n]
  (m/maybe (inc n)))

(defn maybe-g [n]
  (m/maybe (+ n 5)))

(deftest first-law-maybe
  (is (= (m/bind (do-result-maybe 10) maybe-f)
         (maybe-f 10))))

(deftest second-law-maybe
  (is (= (m/bind (do-result-maybe 10) do-result-maybe)
         (do-result-maybe 10))))

(deftest third-law-maybe
  (is (= (m/bind (m/bind (m/maybe 5) maybe-f) maybe-g)
         (m/bind (m/maybe 5) (fn [x]
                               (m/bind (maybe-f x) maybe-g))))))

;; Special cases -- ensure we're handling them correctly. The m/maybe
;; factory function is not used to generate the monadic value passed
;; as the first argument to m/bind, since m/maybe implements
;; "convenience logic" which short-circuits the value of
;; monads.core/*Nothing* (default: nil) to maybe-zero-val. The
;; protocol method do-result, as implemented for class Maybe, does not
;; short-circuit.
(deftest first-law-maybe-*Nothing*
  (is (= (m/bind (do-result-maybe nil) (comp m/maybe not))
         ((comp m/maybe not) nil))))

;; For the same reasons given in the previous comment, m/maybe is not
;; used as the monadic function for this test, nor to generate the
;; monadic value passed as the first argument to m/bind.
(deftest second-law-maybe-*Nothing*
  (is (= (m/bind (do-result-maybe nil) do-result-maybe)
         (do-result-maybe nil))))

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

(deftest state-equality
  "Equality testing for monads.core.State instances is limited owing to
   the fact that those instances may contain function values which
   may not be reliably tested for equality, i.e. if they are anonymous
   functions."
  (is (= (m/state 1) (m/state 1)))
  (is (= (m/state {:a 1}) (m/state {:a 1})))
  (is (not= (m/state 1) (m/state :1)))
  (is (not= (m/state 1) (m/state 2)))
  (is (not= (m/state {:a 1}) (m/state {:a 2})))
  (binding [m/*throw-on-mismatch* false
            m/*warn-on-mismatch* false]
    (let [f #(m/state (inc %))]
      (is (= (m/bind (m/state 1) f)
             (m/bind (m/state 1) f)))))
  (binding [m/*throw-on-mismatch* false
            m/*warn-on-mismatch* false]
    (is (not= (m/bind (m/state 1) #(m/state (inc %)))
              (m/bind (m/state 1) #(m/state (inc %)))))))

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

(deftest update-state-equality
  "The following assertions demonstrate the limitations of testing
   equality for monads.core.State instances. See the docstring for
   test 'state-equality' defined above."
  (is (= (m/update-state identity)
         (m/update-state identity)))
  (is (not= (m/update-state identity)
            (m/update-state inc)))
  (is (not= (m/update-state (fn [s] s))
            (m/update-state (fn [s] s)))))

(deftest get-state-val
  (is (= [17 {:a 17}]
         ((m/get-state-val :a) {:a 17}))))

(deftest set-state-val
  (is (= [17 {:a 12}]
         ((m/set-state-val :a 12) {:a 17}))))

(deftest update-state-val
  (is (= [5 {:a 19}]
         ((m/update-state-val :a + 14) {:a 5}))))

(deftest get-in-state-val
  (let [state {:a {:b 1} :c {:d {:e 2}}}]
    (are [expected args] (is (= expected ((apply m/get-in-state-val args) state)))
         [1 state]      [[:a :b]]
         [:def state]   [[:z] :def]
         [nil state]    [[:a :b :c]]
         [2 state]      [[:c :d :e]]
         [{:b 1} state] [[:a]])))

(deftest assoc-in-state-val
  (is (= [nil {:a {:b {:c 9}}}]
         ((m/assoc-in-state-val [:a :b :c] 9) {}))))

(deftest update-in-state-val
  (are [expected in-state path args] (is (= expected
                                            ((apply m/update-in-state-val path args) in-state)))
       [2 {:a {:b 4}}]      {:a {:b 2}}  [:a :b]  [* 2]
       [2 {:a {:b 3}}]      {:a {:b 2}}  [:a :b]  [inc]
       [nil {:a {:b [1]}}]  {:a nil}     [:a :b]  [(fnil conj []) 1]))

(defn state-f-2+-ary-factory [n]
  (m/state (m/update-state (fn [s] (conj s `["increment" ~n])))
           (fn [_] (m/update-state (fn [s] (conj s ["update-f!"]))))
           (fn [_] (m/state (inc n)))
           (fn [v] (m/state (inc v)))))

(defn state-g-2+-ary-factory [n]
  (m/state (m/update-state (fn [s] (conj s `["plus-five" ~n])))
           (fn [_] (m/update-state (fn [s] (conj s ["update-g!"]))))
           (fn [_] (m/state (+ n 5)))
           (fn [v] (m/state (+ v 5)))))

(deftest first-law-state-2+-ary
  (let [mv1 (m/bind (do-result-state 10) state-f-2+-ary-factory)
        mv2 (state-f-2+-ary-factory 10)]
    (is (= (mv1 []) (mv2 [])))))

(deftest third-law-state-2+-ary
  (let [mv1 (m/bind (m/bind (m/state 4) state-f-2+-ary-factory) state-g-2+-ary-factory)
        mv2 (m/bind (m/state 4)
                    (fn [x]
                      (m/bind (state-f-2+-ary-factory x) state-g-2+-ary-factory)))]
    (is (= (mv1 []) (mv2 [])))))

(deftest state-equality-2+-ary-factory
  "The following assertions demonstrate the limitations of testing
   equality for monads.core.State instances. See the docstring for
   test 'state-equality' defined above."
  (is (not= (state-f-2+-ary-factory 1) (state-f-2+-ary-factory 1)))
  (is (not= (state-f-2+-ary-factory {:a 1})
            (state-f-2+-ary-factory {:a 1}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.Continuation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest cont-equality
  (is (= (m/cont 1) (m/cont 1)))
  (is (= (m/cont {:a 1}) (m/cont {:a 1})))
  (is (not= (m/cont 1) (m/cont :1)))
  (is (not= (m/cont 1) (m/cont 2)))
  (is (not= (m/cont {:a 1}) (m/cont {:a 2}))))

(deftest cont-equality
  "Equality testing for monads.core.Continuation instances is limited
   owing to the fact that those instances may contain function values
   which may not be reliably tested for equality, i.e. if they are
   anonymous functions."
  (is (= (m/cont 1) (m/cont 1)))
  (is (= (m/cont {:a 1}) (m/cont {:a 1})))
  (is (not= (m/cont 1) (m/cont :1)))
  (is (not= (m/cont 1) (m/cont 2)))
  (is (not= (m/cont {:a 1}) (m/cont {:a 2})))
  (binding [m/*throw-on-mismatch* false
            m/*warn-on-mismatch* false]
    (let [f #(m/cont (inc %))]
      (is (= (m/bind (m/cont 1) f)
             (m/bind (m/cont 1) f)))))
  (binding [m/*throw-on-mismatch* false
            m/*warn-on-mismatch* false]
    (is (not= (m/bind (m/cont 1) #(m/cont (inc %)))
              (m/bind (m/cont 1) #(m/cont (inc %)))))))

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
  (let [mv1 (m/bind (do-result-cont 10) do-result-cont)
        mv2 (do-result-cont 10)]
    (is (= (mv1 identity) (mv2 identity)))))

(deftest third-law-cont
  (let [mv1 (m/bind (m/bind (m/cont 4) cont-f) cont-g)
        mv2 (m/bind (m/cont 4)
                    (fn [x]
                      (m/bind (cont-f x) cont-g)))]
    (is (= (mv1 identity) (mv2 identity)))))

(deftest deref-cont
  (is (= @(m/cont 10)
         ((m/cont 10) identity)
         10)))

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

(def writer+set (m/writer #{}))

(deftest writer-equality
  (is (= (writer+set 1) (writer+set 1)))
  (is (= (writer+set {:a 1}) (writer+set {:a 1})))
  (is (not= (writer+set 1) (writer+set :1)))
  (is (not= (writer+set 1) (writer+set 2)))
  (is (not= (writer+set {:a 1}) (writer+set {:a 2}))))

(def do-result-writer+set (partial m/do-result (writer+set [nil])))

(deftest do-result-and-writer-factory-func-equiv
  (is (= (do-result-writer+set [nil])
         (writer+set [nil]))))

(defn writer-f [n]
  (writer+set (inc n)))

(defn writer-g [n]
  (writer+set (+ n 5)))

(deftest first-law-writer
  (is (= (m/bind (do-result-writer+set 10) writer-f)
         (writer-f 10))))

(deftest second-law-writer
  (is (= (m/bind (do-result-writer+set 10) do-result-writer+set)
         (do-result-writer+set 10))))

(deftest third-law-writer
  (is (= (m/bind (m/bind (writer+set 3) writer-f) writer-g)
         (m/bind (writer+set 3)
                 (fn [x]
                   (m/bind (writer-f x) writer-g))))))

(deftest write-writer
  (is (= [nil #{:written}]
         @(m/write-writer writer+set :written))))

(deftest listen-writer
  (is (= [[nil #{:written}] #{:written}]
         @(m/listen-writer (m/write-writer writer+set :written)))))

(deftest censor-writer
  (is (= [nil #{:new-written}]
         @(m/censor-writer (constantly #{:new-written})
                           (m/write-writer writer+set :written)))))

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
               (+ x y))))
  (let [tinc #(vector (inc %))]
    (is ( = [[1 2 3] [3 4 5]]
            (m/do vector
                  [a (vec (range 5))
                   :when (odd? a)
                   x (tinc a)
                   y (tinc x)]
                  [a x y])))))

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
  (is (= @(m/plus [m/Nothing (m/maybe nil) (m/maybe 1) (m/maybe 2)])
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

(let [comprehend (ns-resolve 'monads.core 'comprehend)]
  (deftest comprehend-list
    (let [do-list
          (m/do list
                [x (list 1 2)
                 y (list 3 4)
                 z (list 5 6)]
                (apply list (map inc
                                 [x y z])))
          comprehend-list (comprehend
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
    (let [comprehend-vector (comprehend
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
          comprehend-state (comprehend
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
             (class (first (comprehend-state :state))))))))

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
         @(m/fmap inc m/Nothing))))

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

(deftest chain-state
  (let [state-up (m/update-state #(conj % "step"))]
    (is (= (let [step-f (fn [n] (m/state
                                 state-up
                                 (fn [_] (m/state (inc n)))))]
             (((m/chain [step-f step-f]) 1)
              []))
           ((m/do m/state
                  [x (m/state 1)
                   y (m/state (inc x))
                   _ state-up
                   z (m/state (inc y))
                   _ state-up]
                  z)
            [])))))

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

(deftest list-t-factory-excises-maybe-zero-val-when-monad-is-maybe
  (let [maybe-list (m/list-t m/maybe)]
    (is (= (maybe-list nil 1 nil 2 nil nil 3 4 nil)
           (maybe-list 1 2 3 4)))
    (is (= (maybe-list nil)
           (maybe-list)))))

(def set-list (m/list-t hash-set))

(deftest list-t-equality
  (is (= (set-list 1 2 3) (set-list 1 2 3)))
  (is (= @(set-list 1 2 3) @(set-list 1 2 3)))
  (is (= (set-list {:a 1} {:b 2}) (set-list {:a 1} {:b 2})))
  (is (not= (set-list 1 2 3) (set-list :1 2 3)))
  (is (not= @(set-list 1 2 3) @(set-list :1 2 3)))
  (is (not= (set-list 1) (set-list 2)))
  (is (not= (set-list {:a 1} {:b 2}) (set-list {:a 2} {:b 2}))))

(def do-result-set-list (partial m/do-result (set-list [nil])))
(def zero-val-set-list (m/zero (set-list [nil])))

(deftest do-result-and-list-t-factory-func-equiv
  (is (= (do-result-set-list [nil])
         (set-list [nil])))
  (is (= @(do-result-set-list [nil])
         @(set-list [nil]))))

(defn list-t-f [& ns]
  (apply set-list (map inc ns)))

(defn list-t-g [& ns]
  (apply set-list (map #(+ % 5) ns)))

(deftest first-law-list-t
  (is (= (m/bind (do-result-set-list 10) list-t-f)
         (list-t-f 10))))

(deftest first-law-list-t-factory
  (is (= (m/bind (set-list 10 11 12) list-t-f)
         (list-t-f 10 11 12))))

(deftest second-law-list-t
  (is (= (m/bind (do-result-set-list 10) do-result-set-list)
         (do-result-set-list 10))))

(deftest second-law-list-t-factory
  (is (= (m/bind (set-list 10 11 12) set-list)
         (set-list 10 11 12))))

(deftest third-law-list-t
  (is (= (m/bind (m/bind (set-list 4 5 6) list-t-f) list-t-g)
         (m/bind (set-list 4 5 6)
                 (fn [x]
                   (m/bind (list-t-f x) list-t-g))))))

(deftest plus-list-t
  (let [plus-list-t (m/plus [(set-list 1 2) (set-list) (set-list 3 4)])]
    (is (= #{(list 1 2 3 4)}
           @plus-list-t))
    (is (= monads.core.ListTransformer
           (class plus-list-t)))
    (is (= clojure.lang.PersistentHashSet
           (class @plus-list-t)))
    (is (= clojure.lang.PersistentList
           (class (first @plus-list-t))))))

(deftest zero-laws-list-t
  (is (= #{(list)} @zero-val-set-list))
  (is (= (m/bind zero-val-set-list list-t-f)
         zero-val-set-list))
  (is (= (m/bind (set-list 4 5 6) (constantly zero-val-set-list))
         zero-val-set-list))
  (is (= (m/plus [(set-list 4 5 6) zero-val-set-list])
         (set-list 4 5 6)))
  (is (= (m/plus [zero-val-set-list (set-list 4 5 6)])
         (set-list 4 5 6))))

(deftest do-list-t
  (let [do-list-t
        (m/do set-list
              [x (list-t-f 9 8 7)
               y (list-t-g x)]
              [x y])]
    (is (= #{(list [10 15] [9 14] [8 13])}
           @do-list-t))
    (is (= monads.core.ListTransformer
           (class do-list-t)))
    (is (= clojure.lang.PersistentHashSet
           (class @do-list-t)))
    (is (= clojure.lang.PersistentList
           (class (first @do-list-t))))
    (is (= clojure.lang.PersistentVector
           (class (first (first @do-list-t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.VectorTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest vector-t-factory-excises-maybe-zero-val-when-monad-is-maybe
  (let [maybe-vector (m/vector-t m/maybe)]
    (is (= (maybe-vector nil 1 nil 2 nil nil 3 4 nil)
           (maybe-vector 1 2 3 4)))
    (is (= (maybe-vector nil)
           (maybe-vector)))))

(def set-vec (m/vector-t hash-set))

(deftest vector-t-equality
  (is (= (set-vec 1 2 3) (set-vec 1 2 3)))
  (is (= @(set-vec 1 2 3) @(set-vec 1 2 3)))
  (is (= (set-vec {:a 1} {:b 2}) (set-vec {:a 1} {:b 2})))
  (is (not= (set-vec 1 2 3) (set-vec :1 2 3)))
  (is (not= @(set-vec 1 2 3) @(set-vec :1 2 3)))
  (is (not= (set-vec 1) (set-vec 2)))
  (is (not= (set-vec {:a 1} {:b 2}) (set-vec {:a 2} {:b 2}))))

(def do-result-set-vec (partial m/do-result (set-vec [nil])))
(def zero-val-set-vec (m/zero (set-vec [nil])))

(deftest do-result-and-vector-t-factory-func-equiv
  (is (= (do-result-set-vec [nil])
         (set-vec [nil])))
  (is (= @(do-result-set-vec [nil])
         @(set-vec [nil]))))

(defn vector-t-f [& ns]
  (apply set-vec (map inc ns)))

(defn vector-t-g [& ns]
  (apply set-vec (map #(+ % 5) ns)))

(deftest first-law-vector-t
  (is (= (m/bind (do-result-set-vec 10) vector-t-f)
         (vector-t-f 10))))

(deftest first-law-vector-factory
  (is (= (m/bind (set-vec 10 11 12) vector-t-f)
         (vector-t-f 10 11 12))))

(deftest second-law-vector-t
  (is (= (m/bind (do-result-set-vec 10) do-result-set-vec)
         (do-result-set-vec 10))))

(deftest second-law-vector-t-factory
  (is (= (m/bind (set-vec 10 11 12) set-vec)
         (set-vec 10 11 12))))

(deftest third-law-vector-t
  (is (= (m/bind (m/bind (set-vec 4 5 6) vector-t-f) vector-t-g)
         (m/bind (set-vec 4 5 6)
                 (fn [x]
                   (m/bind (vector-t-f x) vector-t-g))))))

(deftest plus-vector-t
  (let [plus-vector-t (m/plus [(set-vec 1 2) (set-vec) (set-vec 3 4)])]
    (is (= #{[1 2 3 4]}
           @plus-vector-t))
    (is (= monads.core.VectorTransformer
           (class plus-vector-t)))
    (is (= clojure.lang.PersistentHashSet
           (class @plus-vector-t)))
    (is (= clojure.lang.PersistentVector
           (class (first @plus-vector-t))))))

(deftest zero-laws-vector-t
  (is (= #{[]} @zero-val-set-vec))
  (is (= (m/bind zero-val-set-vec vector-t-f)
         zero-val-set-vec))
  (is (= (m/bind (set-vec 4 5 6) (constantly zero-val-set-vec))
         zero-val-set-vec))
  (is (= (m/plus [(set-vec 4 5 6) zero-val-set-vec])
         (set-vec 4 5 6)))
  (is (= (m/plus [zero-val-set-vec (set-vec 4 5 6)])
         (set-vec 4 5 6))))

(deftest do-vector-t
  (let [do-vector-t
        (m/do set-vec
              [x (vector-t-f 9 8 7)
               y (vector-t-g x)]
              (list x y))]
    (is (= #{[(list 10 15) (list 9 14) (list 8 13)]}
           @do-vector-t))
    (is (= monads.core.VectorTransformer
           (class do-vector-t)))
    (is (= clojure.lang.PersistentHashSet
           (class @do-vector-t)))
    (is (= clojure.lang.PersistentVector
           (class (first @do-vector-t))))
    (is (= clojure.lang.PersistentList
           (class (first (first @do-vector-t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.LazySeqTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest lazy-seq-t-factory-excises-maybe-zero-val-when-monad-is-maybe
  (let [maybe-lazy-seq (m/lazy-seq-t m/maybe)]
    (is (= (maybe-lazy-seq nil 1 nil 2 nil nil 3 4 nil)
           (maybe-lazy-seq 1 2 3 4)))
    (is (= (maybe-lazy-seq nil)
           (maybe-lazy-seq)))))

(def set-lazy-seq (m/lazy-seq-t hash-set))

(deftest lazy-seq-t-equality
  (is (= (set-lazy-seq 1 2 3) (set-lazy-seq 1 2 3)))
  (is (= @(set-lazy-seq 1 2 3) @(set-lazy-seq 1 2 3)))
  (is (= (set-lazy-seq {:a 1} {:b 2}) (set-lazy-seq {:a 1} {:b 2})))
  (is (not= (set-lazy-seq 1 2 3) (set-lazy-seq :1 2 3)))
  (is (not= @(set-lazy-seq 1 2 3) @(set-lazy-seq :1 2 3)))
  (is (not= (set-lazy-seq 1) (set-lazy-seq 2)))
  (is (not= (set-lazy-seq {:a 1} {:b 2}) (set-lazy-seq {:a 2} {:b 2}))))

(def do-result-set-lazy-seq (partial m/do-result (set-lazy-seq [nil])))
(def zero-val-set-lazy-seq (m/zero (set-lazy-seq [nil])))

(deftest do-result-and-lazy-seq-t-factory-func-equiv
  (is (= (do-result-set-lazy-seq [nil])
         (set-lazy-seq [nil])))
  (is (= @(do-result-set-lazy-seq [nil])
         @(set-lazy-seq [nil]))))

(defn lazy-seq-t-f [& ns]
  (apply set-lazy-seq (map inc ns)))

(defn lazy-seq-t-g [& ns]
  (apply set-lazy-seq (map #(+ % 5) ns)))

(deftest first-law-lazy-seq-t
  (is (= (m/bind (do-result-set-lazy-seq 10) lazy-seq-t-f)
         (lazy-seq-t-f 10))))

(deftest first-law-lazy-seq-t-factory
  (is (= (m/bind (set-lazy-seq 10 11 12) lazy-seq-t-f)
         (lazy-seq-t-f 10 11 12))))

(deftest second-law-lazy-seq-t
  (is (= (m/bind (do-result-set-lazy-seq 10) do-result-set-lazy-seq)
         (do-result-set-lazy-seq 10))))

(deftest second-law-lazy-seq-t-factory
  (is (= (m/bind (set-lazy-seq 10 11 12) set-lazy-seq)
         (set-lazy-seq 10 11 12))))

(deftest third-law-lazy-seq-t
  (is (= (m/bind (m/bind (set-lazy-seq 4 5 6) lazy-seq-t-f) lazy-seq-t-g)
         (m/bind (set-lazy-seq 4 5 6)
                 (fn [x]
                   (m/bind (lazy-seq-t-f x) lazy-seq-t-g))))))

(deftest plus-lazy-seq-t
  (let [plus-lazy-seq-t (m/plus [(set-lazy-seq 1 2) (set-lazy-seq) (set-lazy-seq 3 4)])]
    (is (= #{(m/lazy-seq* 1 2 3 4)}
           @plus-lazy-seq-t))
    (is (= monads.core.LazySeqTransformer
           (class plus-lazy-seq-t)))
    (is (= clojure.lang.PersistentHashSet
           (class @plus-lazy-seq-t)))
    (is (= clojure.lang.LazySeq
           (class (first @plus-lazy-seq-t))))))

(deftest zero-laws-lazy-seq-t
  (is (= #{(lazy-seq)} @zero-val-set-lazy-seq))
  (is (= (m/bind zero-val-set-lazy-seq lazy-seq-t-f)
         zero-val-set-lazy-seq))
  (is (= (m/bind (set-lazy-seq 4 5 6) (constantly zero-val-set-lazy-seq))
         zero-val-set-lazy-seq))
  (is (= (m/plus [(set-lazy-seq 4 5 6) zero-val-set-lazy-seq])
         (set-lazy-seq 4 5 6)))
  (is (= (m/plus [zero-val-set-lazy-seq (set-lazy-seq 4 5 6)])
         (set-lazy-seq 4 5 6))))

(deftest do-lazy-seq-t
  (let [do-lazy-seq-t
        (m/do set-lazy-seq
              [x (lazy-seq-t-f 9 8 7)
               y (lazy-seq-t-g x)]
              [x y])]
    (is (= #{(list [10 15] [9 14] [8 13])}
           @do-lazy-seq-t))
    (is (= monads.core.LazySeqTransformer
           (class do-lazy-seq-t)))
    (is (= clojure.lang.PersistentHashSet
           (class @do-lazy-seq-t)))
    (is (= clojure.lang.LazySeq
           (class (first @do-lazy-seq-t))))
    (is (= clojure.lang.PersistentVector
           (class (first (first @do-lazy-seq-t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.SetTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest set-t-factory-excises-maybe-zero-val-when-monad-is-maybe
  (let [maybe-set (m/set-t m/maybe)]
    (is (= (maybe-set nil 1 nil 2 nil nil 3 4 nil)
           (maybe-set 1 2 3 4)))
    (is (= (maybe-set nil)
           (maybe-set)))))

(def vec-set (m/set-t vector))

(deftest set-t-equality
  (is (= (vec-set 1 2 3) (vec-set 1 2 3)))
  (is (= @(vec-set 1 2 3) @(vec-set 1 2 3)))
  (is (= (vec-set {:a 1} {:b 2}) (vec-set {:a 1} {:b 2})))
  (is (not= (vec-set 1 2 3) (vec-set :1 2 3)))
  (is (not= @(vec-set 1 2 3) @(vec-set :1 2 3)))
  (is (not= (vec-set 1) (vec-set 2)))
  (is (not= (vec-set {:a 1} {:b 2}) (vec-set {:a 2} {:b 2}))))

(def do-result-vec-set (partial m/do-result (vec-set [nil])))
(def zero-val-vec-set (m/zero (vec-set [nil])))

(deftest do-result-and-set-t-factory-func-equiv
  (is (= (do-result-vec-set [nil])
         (vec-set [nil])))
  (is (= @(do-result-vec-set [nil])
         @(vec-set [nil]))))

(defn set-t-f [& ns]
  (apply vec-set (map inc ns)))

(defn set-t-g [& ns]
  (apply vec-set (map #(+ % 5) ns)))

(deftest first-law-set-t
  (is (= (m/bind (do-result-vec-set 10) set-t-f)
         (set-t-f 10))))

(deftest first-law-vector-factory
  (is (= (m/bind (vec-set 10 11 12) set-t-f)
         (set-t-f 10 11 12))))

(deftest second-law-set-t
  (is (= (m/bind (do-result-vec-set 10) do-result-vec-set)
         (do-result-vec-set 10))))

(deftest second-law-set-t-factory
  (is (= (m/bind (vec-set 10 11 12) vec-set)
         (vec-set 10 11 12))))

(deftest third-law-set-t
  (is (= (m/bind (m/bind (vec-set 4 5 6) set-t-f) set-t-g)
         (m/bind (vec-set 4 5 6)
                 (fn [x]
                   (m/bind (set-t-f x) set-t-g))))))

(deftest plus-set-t
  (let [plus-set-t (m/plus [(vec-set 1 2) (vec-set 1 2) (vec-set) (vec-set 2 3 4) (vec-set 2 3 4)])]
    (is (= [#{1 2 3 4}]
           @plus-set-t))
    (is (= monads.core.SetTransformer
           (class plus-set-t)))
    (is (= clojure.lang.PersistentVector
           (class @plus-set-t)))
    (is (= clojure.lang.PersistentHashSet
           (class (first @plus-set-t))))))

(deftest zero-laws-set-t
  (is (= [#{}] @zero-val-vec-set))
  (is (= (m/bind zero-val-vec-set set-t-f)
         zero-val-vec-set))
  (is (= (m/bind (vec-set 4 5 6) (constantly zero-val-vec-set))
         zero-val-vec-set))
  (is (= (m/plus [(vec-set 4 5 6) zero-val-vec-set])
         (vec-set 4 5 6)))
  (is (= (m/plus [zero-val-vec-set (vec-set 4 5 6)])
         (vec-set 4 5 6))))

(deftest do-set-t
  (let [do-set-t
        (m/do vec-set
              [x (set-t-f 9 8 7)
               y (set-t-g x)]
              (list x y))]
    (is (= [#{(list 10 15) (list 9 14) (list 8 13)}]
           @do-set-t))
    (is (= monads.core.SetTransformer
           (class do-set-t)))
    (is (= clojure.lang.PersistentVector
           (class @do-set-t)))
    (is (= clojure.lang.PersistentHashSet
           (class (first @do-set-t))))
    (is (= clojure.lang.PersistentList
           (class (first (first @do-set-t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.MaybeTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def vec-maybe (m/maybe-t vector))

(deftest maybe-t-factory-excises-maybe-zero-val
  (is (= (vec-maybe nil 1 nil 2 nil nil 3 4 nil)
         (vec-maybe 1 2 3 4)))
  (is (= (vec-maybe nil)
         (vec-maybe))))

(deftest maybe-t-equality
  (is (= (vec-maybe 1 2 3) (vec-maybe 1 2 3)))
  (is (= (vec-maybe {:a 1} {:b 2}) (vec-maybe {:a 1} {:b 2})))
  (is (not= (vec-maybe 1 2 3) (vec-maybe :1 2 3)))
  (is (not= (vec-maybe 1) (vec-maybe 2)))
  (is (not= (vec-maybe {:a 1} {:b 2}) (vec-maybe {:a 2} {:b 2}))))

(def do-result-vec-maybe (partial m/do-result (vec-maybe [nil])))
(def zero-val-vec-maybe (m/zero (vec-maybe [nil])))

(deftest do-result-and-maybe-t-factory-func-equiv
  (is (= (do-result-vec-maybe [nil])
         (vec-maybe [nil])))
  (is (= @(do-result-vec-maybe [nil])
         @(vec-maybe [nil]))))

(deftest do-result-and-maybe-t-factory-func-not-equiv-for-*Nothing*
  (is (not= (do-result-vec-maybe nil)
            (vec-maybe nil)))
  (is (not= @(do-result-vec-maybe nil)
            @(vec-maybe nil)))
  (binding [m/*Nothing* {:some ['value]}]
    (is (not= (do-result-vec-maybe {:some ['value]})
              (vec-maybe {:some ['value]})))
    (is (not= @(do-result-vec-maybe {:some ['value]})
              @(vec-maybe {:some ['value]})))))

(deftest zero-val-from-maybe-t-factory-func
  (is (= [m/Nothing]
         @zero-val-vec-maybe
         @(vec-maybe nil)
         @(vec-maybe nil nil nil)
         @(vec-maybe)))
  (binding [m/*Nothing* {:some ['value]}]
    (is (= [m/Nothing]
           @zero-val-vec-maybe
           @(vec-maybe {:some ['value]})
           @(vec-maybe {:some ['value]} {:some ['value]} {:some ['value]})
           @(vec-maybe)))))

(defn maybe-t-f [& ns]
  (apply vec-maybe (map #(when % (inc %)) ns)))

(defn maybe-t-g [& ns]
  (apply vec-maybe (map #(when % (+ % 5)) ns)))

(deftest first-law-maybe-t
  (is (= (m/bind (do-result-vec-maybe 10) maybe-t-f)
         (maybe-t-f 10))))

(deftest first-law-maybe-t-factory
  (is (= (m/bind (vec-maybe 10 nil 11) maybe-t-f)
         (maybe-t-f 10 nil 11))))

(deftest second-law-maybe-t
  (is (= (m/bind (do-result-vec-maybe 10) do-result-vec-maybe)
         (do-result-vec-maybe 10))))

(deftest second-law-maybe-t-factory
  (is (= (m/bind (vec-maybe 10 nil 11) vec-maybe)
         (vec-maybe 10 nil 11))))

(deftest third-law-maybe-t
  (is (= (m/bind (m/bind (vec-maybe 4 nil 5) maybe-t-f) maybe-t-g)
         (m/bind (vec-maybe 4 nil 5)
                 (fn [x]
                   (m/bind (maybe-t-f x) maybe-t-g))))))

;; Special cases -- ensure we're handling them correctly. The
;; vec-maybe factory function (returned by m/maybe-t) is not used to
;; generate the monadic value passed as the first argument to m/bind,
;; since m/maybe-t implements "convenience logic" which short-circuits
;; the value of monads.core/*Nothing* (default: nil) to
;; maybe-zero-val. The protocol method do-result, as implemented for
;; class MaybeTransformer, does not short-circuit.
(deftest first-law-maybe-t-*Nothing*
  (is (= (m/bind (do-result-vec-maybe nil) (comp vec-maybe not))
         ((comp vec-maybe not) nil))))

;; For the same reasons given in the previous comment, vec-maybe is
;; not used as the monadic function for this test, nor to generate the
;; monadic value passed as the first argument to m/bind.
(deftest second-law-maybe-t-*Nothing*
  (is (= (m/bind (do-result-vec-maybe nil) do-result-vec-maybe)
         (do-result-vec-maybe nil))))

(deftest plus-maybe-t
  (let [plus-maybe-t (m/plus [(vec-maybe 1 2) (vec-maybe) (vec-maybe 3 4)])]
    (is (= (vec-maybe 1 2)
           plus-maybe-t))
    (is (= monads.core.MaybeTransformer
           (class plus-maybe-t)))
    (is (= clojure.lang.PersistentVector
           (class @plus-maybe-t)))
    (is (= monads.core.Maybe
           (class (first @plus-maybe-t))))))

(deftest zero-laws-maybe-t
  (is (= [m/Nothing] @zero-val-vec-maybe))
  (is (= (m/bind zero-val-vec-maybe list-t-f)
         zero-val-vec-maybe))
  (is (= (m/bind (vec-maybe 4 5 nil) (constantly zero-val-vec-maybe))
         zero-val-vec-maybe))
  (is (= (m/plus [(vec-maybe 4 5 nil) zero-val-vec-maybe])
         (vec-maybe 4 5 nil)))
  (is (= (m/plus [zero-val-vec-maybe (vec-maybe 4 nil 5)])
         (vec-maybe 4 nil 5))))

(deftest do-maybe-t
  (is (= zero-val-vec-maybe
         (m/do vec-maybe
               [x (vec-maybe nil)
                y (maybe-t-g x)]
               [x y])))
  (let [do-maybe-t
        (m/do vec-maybe
              [x (maybe-t-f 9 nil 7)
               y (maybe-t-g x)]
              [x y])]
    (is (= [(m/maybe [10 15]) (m/maybe [8 13])]
           @do-maybe-t))
    (is (= monads.core.MaybeTransformer
           (class do-maybe-t)))
    (is (= clojure.lang.PersistentVector
           (class @do-maybe-t)))
    (is (= monads.core.Maybe
           (class (first @do-maybe-t))))
    (is (= clojure.lang.PersistentVector
           (class @(first @do-maybe-t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.StateTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest state-t-factory-short-circuits-to-maybe-zero-val-when-monad-is-maybe
  (let [maybe-state (m/state-t m/maybe)]
    (is (= ((maybe-state nil) :state)
           m/Nothing))))

(def vec-state (m/state-t vector))

(deftest state-t-equality
  "Equality testing for monads.core.StateTransformer instances is
   limited owing to the fact that those instances may contain function
   values which may not be reliably tested for equality, i.e. if they
   are anonymous functions."
  (is (= (vec-state 1) (vec-state 1)))
  (is (= (vec-state {:a 1}) (vec-state {:a 1})))
  (is (not= (vec-state 1) (vec-state :1)))
  (is (not= (vec-state 1) (vec-state 2)))
  (is (not= (vec-state {:a 1}) (vec-state {:a 2})))
  (binding [m/*throw-on-mismatch* false
            m/*warn-on-mismatch* false]
    (let [f #(vec-state (inc %))]
      (is (= (m/bind (vec-state 1) f)
             (m/bind (vec-state 1) f)))))
  (binding [m/*throw-on-mismatch* false
            m/*warn-on-mismatch* false]
    (is (not= (m/bind (vec-state 1) #(vec-state (inc %)))
              (m/bind (vec-state 1) #(vec-state (inc %)))))))

(def do-result-vec-state (partial m/do-result (vec-state [nil])))
(def zero-val-vec-state (m/zero (vec-state [nil])))

(deftest do-result-and-state-t-factory-func-equiv
  (is (= ((do-result-vec-state [nil]) :state)
         ((vec-state [nil]) :state))))

(deftest zero-val-from-state-t-factory-func
  (is (= []
         (zero-val-vec-state :state)
         ((vec-state []) :state))))

(defn state-t-f [n]
  (vec-state (inc n)))

(defn state-t-g [n]
  (vec-state (+ n 5)))

(deftest first-law-state-t
  (let [mv1 (m/bind (do-result-vec-state 10) state-t-f)
        mv2 (state-t-f 10)]
    (is (= (mv1 {}) (mv2 {})))))

(deftest second-law-state-t
  (let [mv1 (m/bind (do-result-vec-state 10) do-result-vec-state)
        mv2 (do-result-vec-state 10)]
    (is (= (mv1 :state-t) (mv2 :state-t)))))

(deftest third-law-state-t
  (let [mv1 (m/bind (m/bind (vec-state 4) state-t-f) state-t-g)
        mv2 (m/bind (vec-state 4)
                    (fn [x]
                      (m/bind (state-t-f x) state-t-g)))]
    (is (= (mv1 :state-t) (mv2 :state-t)))))

(def update-vec-state (m/update-state-t vec-state))

(deftest update-state-t
  (is (= [[:state :new-state]]
         ((update-vec-state (constantly :new-state)) :state))))

(deftest update-state-t-equality
  "The following assertions demonstrate the limitations of testing
   equality for monads.core.StateTransformer instances. See the
   docstring for test 'state-t-equality' defined above."
  (is (= (update-vec-state identity)
         (update-vec-state identity)))
  (is (not= (update-vec-state identity)
            (update-vec-state inc)))
  (is (not= (update-vec-state (fn [s] s))
            (update-vec-state (fn [s] s)))))

(deftest get-state-t-val
  (let [get-vec-state-val (m/get-state-t-val vec-state)]
    (is (= [[17 {:a 17}]]
           ((get-vec-state-val :a) {:a 17})))))

(deftest set-state-t-val
  (let [set-vec-state-val (m/set-state-t-val vec-state)]
    (is (= [[17 {:a 12}]]
           ((set-vec-state-val :a 12) {:a 17})))))

(deftest update-state-t-val
  (let [update-vec-state-val (m/update-state-t-val vec-state)]
    (is (= [[5 {:a 19}]]
           ((update-vec-state-val :a + 14) {:a 5})))))

(deftest get-in-state-t-val
  (let [get-in-vec-state-val (m/get-in-state-t-val vec-state)
        state {:a {:b 1} :c {:d {:e 2}}}]
    (are [expected args] (is (= expected ((apply get-in-vec-state-val args) state)))
         [[1 state]]      [[:a :b]]
         [[:def state]]   [[:z] :def]
         [[nil state]]    [[:a :b :c]]
         [[2 state]]      [[:c :d :e]]
         [[{:b 1} state]] [[:a]])))

(deftest assoc-in-state-t-val
  (let [assoc-in-vec-state-val (m/assoc-in-state-t-val vec-state)]
    (is (= [[nil {:a {:b {:c 9}}}]]
           ((assoc-in-vec-state-val [:a :b :c] 9) {})))))

(deftest update-in-state-t-val
  (let [update-in-vec-state-val (m/update-in-state-t-val vec-state)]
    (are [expected in-state path args] (is (= expected
                                              ((apply update-in-vec-state-val path args) in-state)))
         [[2 {:a {:b 4}}]]      {:a {:b 2}}  [:a :b]  [* 2]
         [[2 {:a {:b 3}}]]      {:a {:b 2}}  [:a :b]  [inc]
         [[nil {:a {:b [1]}}]]  {:a nil}     [:a :b]  [(fnil conj []) 1])))

(defn state-t-f-2+-ary-factory [n]
  (vec-state (update-vec-state (fn [s] (conj s `["increment" ~n])))
             (fn [_] (update-vec-state (fn [s] (conj s ["update-f!"]))))
             (fn [_] (vec-state (inc n)))
             (fn [v] (vec-state (inc v)))))

(defn state-t-g-2+-ary-factory [n]
  (vec-state (update-vec-state (fn [s] (conj s `["plus-five" ~n])))
             (fn [_] (update-vec-state (fn [s] (conj s ["update-g!"]))))
             (fn [_] (vec-state (+ n 5)))
             (fn [v] (vec-state (+ v 5)))))

(deftest first-law-state-2+-ary
  (let [mv1 (m/bind (do-result-state 10) state-t-f-2+-ary-factory)
        mv2 (state-t-f-2+-ary-factory 10)]
    (is (= (mv1 []) (mv2 [])))))

(deftest third-law-state-2+-ary
  (let [mv1 (m/bind (m/bind (vec-state 4) state-t-f-2+-ary-factory) state-t-g-2+-ary-factory)
        mv2 (m/bind (vec-state 4)
                    (fn [x]
                      (m/bind (state-t-f-2+-ary-factory x) state-t-g-2+-ary-factory)))]
    (is (= (mv1 []) (mv2 [])))))

(deftest state-t-equality-2+-ary-factory
  "The following assertions demonstrate the limitations of testing
   equality for monads.core.StateTransformer instances. See the
   docstring for test 'state-t-equality' defined above."
  (is (not= (state-t-f-2+-ary-factory 1)
            (state-t-f-2+-ary-factory 1)))
  (is (not= (state-t-f-2+-ary-factory {:a 1})
            (state-t-f-2+-ary-factory {:a 1}))))

(deftest plus-state-t
  (let [maybe-state (m/state-t m/maybe)]
    (is (= [:test :state]
           @((m/plus* [(maybe-state nil)
                       (m/do maybe-state
                             [:when false]
                             (throw (Exception. "Really should not be thrown")))
                       (maybe-state :test)
                       (m/do maybe-state
                             [_ (maybe-state 10)]
                             (throw (Exception. "Should not be thrown")))])
             :state)))
    (is (= [:test :state]
           @((m/plus* [(maybe-state nil)
                       (maybe-state :test)
                       (m/do maybe-state
                             [_ (maybe-state 10)]
                             (throw (Exception. "Should not be thrown")))])
             :state)))))

(deftest zero-laws-state-t
  (is (= [] (zero-val-vec-state :state)))
  (is (= ((m/bind zero-val-vec-state state-t-f) :state)
         []))
  (is (= ((m/bind (vec-state 4) (constantly zero-val-vec-state)) :state)
         []))
  (is (= ((m/plus [(vec-state 5) zero-val-vec-state]) :state)
         ((vec-state 5) :state)))
  (is (= ((m/plus [zero-val-vec-state (vec-state 4)]) :state)
         ((vec-state 4) :state))))

(deftest do-state-t
  (let [do-state-t (m/do vec-state
                         [x (state-t-f 9)
                          y (state-t-g x)]
                         (list x y))
        do-state-t-ret (do-state-t :state)]
    (is (= [[(list 10 15) :state]]
           do-state-t-ret))
    (is (= monads.core.StateTransformer
           (class do-state-t)))
    (is (= clojure.lang.PersistentVector
           (class do-state-t-ret)))
    (is (= clojure.lang.PersistentList
           (class (first (first do-state-t-ret)))))
    (is (= []
           ((m/do vec-state
                  [:when false]
                  :something)
            :state)))
    (let [maybe-state (m/state-t m/maybe)
          set-maybe-state-val (m/set-state-t-val maybe-state)]
      (is (= [19 {:val 19}]
             @((m/do maybe-state
                     [_ (set-maybe-state-val :val 19)]
                     19)
               {}))))))

(deftest chain-state-t
  (let [state-up (update-vec-state #(conj % "step"))]
    (is (= (let [step-f (fn [n] (vec-state
                                 state-up
                                 (fn [_] (vec-state (inc n)))))]
             (((m/chain [step-f step-f]) 1)
              []))
           ((m/do vec-state
                  [x (vec-state 1)
                   y (vec-state (inc x))
                   _ state-up
                   z (vec-state (inc y))
                   _ state-up]
                  z)
            [])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  monads.core.WriterTransformer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest writer-t-factory-short-circuits-to-maybe-zero-val-when-monad-is-maybe
  (let [maybe-writer+string (m/writer-t m/maybe "")]
    (is (= @(maybe-writer+string nil)
           m/Nothing))))

(def set-writer+vec (m/writer-t hash-set []))

(deftest writer-t-equality
  (is (= (set-writer+vec 1) (set-writer+vec 1 )))
  (is (= @(set-writer+vec 1) @(set-writer+vec 1 )))
  (is (= (set-writer+vec {:a 1}) (set-writer+vec {:a 1})))
  (is (not= (set-writer+vec 1) (set-writer+vec :1)))
  (is (not= @(set-writer+vec 1) @(set-writer+vec :1)))
  (is (not= (set-writer+vec 1) (set-writer+vec 2)))
  (is (not= (set-writer+vec {:a 1}) (set-writer+vec {:a 2}))))

(def do-result-set-writer+vec (partial m/do-result (set-writer+vec [nil])))
(def zero-val-set-writer+vec (m/zero (set-writer+vec [nil])))

(deftest do-result-and-writer-t-factory-func-equiv
  (is (= (do-result-set-writer+vec [nil])
         (set-writer+vec [nil])))
  (is (= @(do-result-set-writer+vec [nil])
         @(set-writer+vec [nil]))))

(defn writer-t-f [n]
  (set-writer+vec (inc n)))

(defn writer-t-g [n]
  (set-writer+vec (+ n 5)))

(deftest first-law-writer-t
  (is (= (m/bind (do-result-set-writer+vec 10) writer-t-f)
         (writer-t-f 10))))

(deftest second-law-writer-t
  (is (= (m/bind (do-result-set-writer+vec 10) do-result-set-writer+vec)
         (do-result-set-writer+vec 10))))

(deftest third-law-writer-t
  (is (= (m/bind (m/bind (set-writer+vec 4) writer-t-f) writer-t-g)
         (m/bind (set-writer+vec 4)
                 (fn [x]
                   (m/bind (writer-t-f x) writer-t-g))))))

(deftest plus-writer-t
  (let [plus-writer-t (m/plus [(set-writer+vec 1) (set-writer+vec 2)])
        writer+vec (m/writer [])]
    (is (= #{(writer+vec 1) (writer+vec 2)}
           @plus-writer-t))
    (is (= monads.core.WriterTransformer
           (class plus-writer-t)))
    (is (= clojure.lang.PersistentHashSet
           (class @plus-writer-t)))
    (is (= monads.core.Writer
           (class (first @plus-writer-t))))
    (is (= clojure.lang.PersistentVector
           (class @(first @plus-writer-t))))))

(deftest zero-laws-writer-t
  (is (= #{} @zero-val-set-writer+vec))
  (is (= (m/bind zero-val-set-writer+vec writer-t-f)
         zero-val-set-writer+vec))
  (is (= (m/bind (set-writer+vec 4) (constantly zero-val-set-writer+vec))
         zero-val-set-writer+vec))
  (is (= (m/plus [(set-writer+vec 4) zero-val-set-writer+vec])
         (set-writer+vec 4)))
  (is (= (m/plus [zero-val-set-writer+vec (set-writer+vec 4)])
         (set-writer+vec 4))))

(deftest do-writer-t
  (is (= (set-writer+vec [10 15])
         (m/do set-writer+vec
               [x (writer-t-f 9)
                y (writer-t-g x)]
               [x y]))))

(let [writer+vec (m/writer [])]
  (deftest write-writer-t
    (is (= #{(m/write-writer writer+vec :written)}
           @(m/write-writer set-writer+vec :written))))

  (deftest listen-writer-t
    (is (= #{(m/listen-writer (m/write-writer writer+vec :written))}
           @(m/listen-writer (m/write-writer set-writer+vec :written)))))

  (deftest censor-writer-t
    (is (= #_[nil #{:new-written}]
           #{(m/censor-writer (constantly #{:new-written})
                              (m/write-writer writer+vec :written))}
           @(m/censor-writer (constantly #{:new-written})
                             (m/write-writer set-writer+vec :written))))))

(deftest write-listen-censor-writer-t
  (let [write-msg (fn [msg] (m/write-writer set-writer+vec msg))
        listen-msgs (fn [mv] (m/listen-writer mv))
        censor-msgs (fn [f mv] (m/censor-writer f mv))]
    (is (= [nil [:msg1]]
           @(first @(write-msg :msg1))))
    (is (= [[nil [:msg3]] [:msg3]]
           @(first @(listen-msgs (write-msg :msg3)))))
    (is (= [(list nil [nil [:msg3]] nil) [:msg1 :msg3 :msg2 :msg4]]
           (->> (m/seq [(write-msg :msg1)
                        (listen-msgs (write-msg :msg3))
                        (write-msg :msg2)])
                (censor-msgs #(conj % :msg4))
                deref
                first
                deref)))
    (is (= #{[5 [:msg3]] [nil [:msg1 :msg3]]}
           (m/fmap (fn [writer-mv] @writer-mv)
                   (->> (m/plus [(write-msg :msg1)
                                 (m/zero (set-writer+vec nil))
                                 (m/zero (write-msg :msg2))
                                 (set-writer+vec 5)])
                        (censor-msgs #(conj % :msg3))
                        deref))))))

(deftest maybe-writer+vec-state
  (let [maybe-writer+vec-state (m/state-t (m/writer-t m/maybe []))
        write-msg (fn [msg] (maybe-writer+vec-state
                             ((m/state-t (m/writer-t m/maybe [msg])) nil)
                             (constantly (maybe-writer+vec-state nil))))
        listen-msgs (fn [mv] (maybe-writer+vec-state
                              (fn [s]
                                (let [[[_ s] msgs] @@@(mv s)]
                                  ((m/writer-t m/maybe msgs) [msgs s])))
                              (fn [v]
                                (maybe-writer+vec-state v))))
        censor-msgs (fn [f mv] (maybe-writer+vec-state
                                (fn [s]
                                  (let [[[v s] msgs] @@@(mv s)]
                                    ((m/writer-t m/maybe (f msgs)) [[v msgs] s])))
                                (fn [v]
                                  (maybe-writer+vec-state v))))]
    (is (= [[nil :state] [:msg]]
           @@@((write-msg :msg) :state)))
    (is (= [[:result :state] [:msg]]
           @@@((m/bind (write-msg :msg)
                       (fn [x]
                         (is (nil? x))
                         (maybe-writer+vec-state :result)))
               :state)))
    (is (= [[(list nil nil) :state] [:msg1 :msg2]]
           @@@((m/seq [(write-msg :msg1)
                       (write-msg :msg2)])
               :state)))
    (is (= [[nil :state] [:msg1]]
           @@@((m/plus [(write-msg :msg1)
                        (write-msg :msg2)])
               :state)))
    (is (= [[nil :state] [:msg2]]
           @@@((m/plus [(m/zero (maybe-writer+vec-state nil))
                        (write-msg :msg2)])
               :state)))
    (is (= [[[:msg3] :state] [:msg3]]
           @@@((listen-msgs (write-msg :msg3)) :state)))

    (is (= [[[(list nil [:msg3] nil) [:msg1 :msg3 :msg2]] :state] [:msg1 :msg3 :msg2 :msg4]]
           @@@((->> (m/seq [(write-msg :msg1)
                            (listen-msgs (write-msg :msg3))
                            (write-msg :msg2)])
                    (censor-msgs #(conj % :msg4)))
               :state)))
    (is (= [[[nil [:msg1]] :state] [:msg1 :msg3]]
           @@@((->> (m/plus [(m/zero (maybe-writer+vec-state nil))
                             (m/zero (write-msg :msg2))
                             (write-msg :msg1)])
                    (censor-msgs #(conj % :msg3)))
               :state)))))
