;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monad application examples
;;
;; ported from Konrad Hinsen's examples for his `algo.monads` library
;; http://j.mp/algo-monads-examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns #^{:doc "Examples for using protocol-monads"}
  monads.examples
  (:require [monads.core :as m]
            [clojure.algo.monads :as am]))

(defmacro defexample
  [name & body]
  `(defn ~name [] ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Each example is defined as a function which takes zero arguments.
;; This is to facilitate experimentation in a Clojure REPL. To run the
;; first example, you could do `monads.examples> (ex1)`, then make
;; changes, recompile and run it again. Have fun, be fearless and don't
;; be afraid to ask lots of questions in #clojure on irc.freenode.net
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defexample ex1
  (m/do list
        [x (range 5)
         y (range 3)]
        (+ x y)))

(defexample ex2
  (m/do vector
        [x (vec (range 5))
         y (vec (range 3))]
        (+ x y)))

(defexample ex3
  (m/do hash-set
        [x (into #{} (range 5))
         y (into #{} (range 3))]
        (+ x y)))

(defexample ex4
  (m/do list
        [x (range 5)
         y (range (+ 1 x))
         :when (= (+ x y) 2)]
        (list x y)))

(defexample ex5
  (m/do list
        [x (range 5)
         y (range (+ 1 x))
         :let [sum (+ x y)
               diff (- x y)]
         :when (= sum 2)]
        (list x y sum diff)))

(defexample ex6
  (let [pairs (fn [xs]
                (m/lift #(list %1 %2)))]
    (pairs (range 5))))


(am/with-monad am/sequence-m
  (let [pairs (fn [xs]
                ((am/m-lift 2 #(list %1 %2)) xs xs))]
    (pairs (range 5))))

