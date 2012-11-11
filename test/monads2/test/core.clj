(ns monads2.test.core
  (:use [clojure.test])
  (:require [monads2.core :as m]))

;;;;;;;;;;;;;;;;;;;;

(defn list-f [n]
  (list (inc n)))

(defn list-g [n]
  (list (+ n 5)))

(deftest first-law-list
  (is (= (m/bind (list 10) list-f)
         (list-f 10))))

(deftest second-law-list
         (is (= (m/bind '(10) list)
                '(10))))

(deftest third-law-list
         (is (= (m/bind (m/bind [4 9] list-f) list-g)
                (m/bind [4 9] (fn [x]
                              (m/bind (list-f x) list-g))))))

(deftest zero-law-list
         (is (= (m/bind '() list-f)
                '()))
         (is (= (m/bind '(4) (constantly '()))
                '()))
         (is (= (m/plus [(list 5 6) '()])
                (list 5 6)))
         (is (= (m/plus ['() (list 5 6)])
                (list 5 6))))

;;;;;;;;;;;;;;;;;;;;

(defn vector-f [n]
  (vector (inc n)))

(defn vector-g [n]
  (vector (+ n 5)))

(deftest first-law-vector
  (is (= (m/bind [10] vector-f)
         (vector-f 10))))

(deftest second-law-vector
         (is (= (m/bind [10] vector)
                [10])))

(deftest third-law-vector
         (is (= (m/bind (m/bind [4 9] vector-f) vector-g)
                (m/bind [4 9] (fn [x]
                              (m/bind (vector-f x) vector-g))))))

(deftest zero-law-vector
         (is (= (m/bind [] vector-f)
                []))
         (is (= (m/bind '(4) (constantly []))
                []))
         (is (= (m/plus [(vector 5 6) []])
                (vector 5 6)))
         (is (= (m/plus [[] (vector 5 6)])
                (vector 5 6))))

;;;;;;;;;;;;;;;;;;;;

(defn set-f [n]
  (hash-set (inc n)))

(defn set-g [n]
  (hash-set (+ n 5)))

(deftest first-law-set
  (is (= (m/bind #{10} set-f)
         (set-f 10))))

(deftest second-law-set
         (is (= (m/bind #{10} hash-set)
                #{10})))

(deftest third-law-set
         (is (= (m/bind (m/bind #{4 9} set-f) set-g)
                (m/bind #{4 9} (fn [x]
                               (m/bind (set-f x) set-g))))))

(deftest zero-law-set
         (is (= (m/bind #{} set-f)
                #{}))
         (is (= (m/bind #{4} (constantly #{}))
                #{}))
         (is (= (m/plus [(hash-set 5 6) #{}])
                (hash-set 5 6)))
         (is (= (m/plus [#{} (hash-set 5 6)])
                (hash-set 5 6))))

;;;;;;;;;;;;;;;;;;;;

(defn state-f [n]
  (m/state (inc n)))

(defn state-g [n]
  (m/state (+ n 5)))

(deftest first-law-state
  (let [mv1 (m/bind (m/state 10) state-f)
        mv2 (state-f 10)]
    (is (= (mv1 {}) (mv2 {})))))

(deftest second-law-state
         (let [mv1 (m/bind (m/state 10) m/state)
               mv2 (m/state 10)]
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

(deftest test-update-val
         (is (= [5 {:a 19}]
                ((m/update-val :a + 14) {:a 5}))))

;;;;;;;;;;;;;;;;;;;;

(defn cont-f [n]
  (m/cont (inc n)))

(defn cont-g [n]
  (m/cont (+ n 5)))

(deftest first-law-cont
  (let [mv1 (m/bind (m/cont 10) cont-f)
        mv2 (cont-f 10)]
    (is (= (mv1 identity) (mv2 identity)))))

(deftest second-law-cont
         (let [mv1 (m/bind (m/cont 10) m/cont)
               mv2 (m/cont 10)]
           (is (= (mv1 identity) (mv2 identity)))))

(deftest third-law-cont
         (let [mv1 (m/bind (m/bind (m/cont 4) cont-f) cont-g)
               mv2 (m/bind (m/cont 4)
                           (fn [x]
                             (m/bind (cont-f x) cont-g)))]
           (is (= (mv1 identity) (mv2 identity)))))

;;;;;;;;;;;;;;;;;;;;

(deftest if-then-else-test
  (let [monad-fn (m/do [
                   a (m/get-state)
                   b (m/update-state (partial + 1))
                   :if (not (= a b))
                   :then [
                     result (m/state "blah")
                   ]
                   :else [
                     result (m/state "bleh")
                   ]]
                   result)]
    (is (= ["blah" 2] (monad-fn 1)))))

(deftest if-then-else-nested-test
  (let [monad-fn (m/do [
                   a (m/get-state)
                   b (m/update-state #(.toUpperCase %))
                   :if (= a b)
                   :then [
                     c (m/state "... what's up?")
                     :if (= 5 (.length c))
                     :then [
                       result (m/state a)
                     ]
                     :else [
                       result (m/state (concat a c))
                     ]]
                   :else [
                     d (m/state "... you didn't!")
                     :if (= 5 (.length c))
                     :then [
                       result (m/state (concat b d))
                     ]
                     :else [
                       result (m/state b)
                     ]]]
                    result)]
    (is (= ["RINGO" "RINGO"] (monad-fn "ringo")))))

#_(prn :do ((m/do
            [x (m/state 29)
             y (m/state 12)
             :let [z (inc x)]]
            [x y z])
            :state))
