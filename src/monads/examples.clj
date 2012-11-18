;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Monad Application Examples
;;
;;  Ported from Konrad Hinsen's `algo.monads` library for Clojure:
;;  http://j.mp/algo-monads-examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns #^{:doc "Examples for using protocol-monads"}
  monads.examples
  (:require [monads.core :as m]
            [clojure.algo.monads :as am]))

(alter-var-root (var m/*check-types*) (constantly true))

(defmacro defex
  [name & body]
  `(defn ~name [] ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Each example is defined as a function which takes zero arguments.
;;  To run the first example, in a Clojure REPL you could do
;;  `monads.examples> (ex1)`, then make changes, recompile and run it
;;  again. Have fun, be fearless and don't be afraid to ask lots of
;;  questions in #clojure on irc.freenode.net.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defex ex1
  (m/do list
        [x (range 5)
         y (range 3)]
        (+ x y)))


(defex ex2
  (let [res (m/do vector
                  [x (range 5)
                   y (range 3)]
                  (+ x y))]
    (println (class res))
    res))


(defex ex3
  (m/do lazy-seq
        [x (range 5)
         y (range 3)]
        (+ x y)))


(defex ex4a
  (m/do hash-set
        [x (into #{} (range 5))
         y (into #{} (range 3))]
        (+ x y)))


(defex ex4b
  "Will throw an exception since monads.core/*check-types* was bound to
   true earlier in this program (its default value is false)."
  (m/do hash-set
        [x (into [] (range 5))
         y (into #{} (range 3))]
        (+ x y)))


(defex ex4c
  "Will not throw an exception since monads.core/*check-types* is
   dynamically bound to false."
  (binding [m/*check-types* false]
    (m/do hash-set
          [x (into [] (range 5))
           y (into #{} (range 3))]
          (+ x y))))


(defex ex5
  (m/do lazy-seq
        [x (range 5)
         y (range (+ 1 x))
         :when (= (+ x y) 2)]
        (list x y)))


(defex ex6
  (m/do lazy-seq
        [x (range 5)
         y (range (+ 1 x))
         :let [sum (+ x y)
               diff (- x y)]
         :when (= sum 2)]
        (list diff)))


(defex ex7
  (let [pairs (fn [xs]
                ((m/lift #(list %1 %2)) xs xs))]
    (pairs (range 5))))


(defex ex8
  (let [pairs (fn [xs]
                (m/seq [xs xs]))]
    (pairs (range 5))))


(defex ex9
  (let [ntuples (fn [n xs]
                  (m/seq (repeat n xs)))]
    [(symbol "(ntuples 2) =>") (ntuples 2 (range 5))
     (symbol "(ntuples 3) =>") (ntuples 3 (range 5))]))


(defex ex10
  (m/do list
        [x ((m/lift (partial * 2)) (range 5))
         y (range 2)]
        [x y]))


(defex ex11
  (m/do list
        [x ((m/lift +) (range 5) (range 3))
         y (m/plus [(range 2) '(10 11)])]
        [x y]))


(def m+ (m/lift +))
(def m- (m/lift -))
(def m* (m/lift *))

(defn safe-div
  [x y]
  (m/do m/maybe
        [a x
         b y
         :when (not (zero? b))]
        (/ a b)))

(defn some-function
  [x y]
  (let [one (m/maybe 1)]
    (safe-div one (m+ (safe-div one (m/maybe x))
                      (safe-div one (m/maybe y))))))


(defex ex12
  [(symbol "@(some-function 2 3) =>") @(some-function 2 3)
   (symbol "@(some-function 2 0) =>") @(some-function 2 0)
   (symbol "(some-function 2 -2) =>") (some-function 2 -2)])


(defex ex13
  (m/plus [(some-function 2 0)
           (some-function 2 -2)
           (some-function 2 3)]))





























