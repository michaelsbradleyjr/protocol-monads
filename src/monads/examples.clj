(ns monads.examples
  (:require [monads.core :as m]
            [clojure.algo.monads :as am]))

(m/do list
      [x (range 5)
       y (range 3)]
      (+ x y))

(m/do vector
      [x (vec (range 5))
       y (vec (range 3))]
      (+ x y))

(m/do hash-set
      [x (into #{} (range 5))
       y (into #{} (range 3))]
      (+ x y))

(m/do list
      [x (range 5)
       y (range (+ 1 x))
       :when (= (+ x y) 2)]
      (list x y))

(m/do list
      [x (range 5)
       y (range (+ 1 x))
       :let [sum (+ x y)
             diff (- x y)]
       :when (= sum 2)]
      (list x y sum diff))
