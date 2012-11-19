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
  (:require [monads.core :as m]))

(defmacro defex
  [name & body]
  `(defn ~name [] ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Each example is defined as a function which takes zero arguments.
;;  To run the first example, in a Clojure REPL you could do
;;  `monads.examples=> (ex1)`, then make changes, recompile and run it
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
  (let [res
        (m/do vector
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


(defex ex4
  (m/do hash-set
        [x (into #{} (range 5))
         y (into #{} (range 3))]
        (+ x y)))


(defex ex5
  "Will throw an exception since monads.core/*check-types* is
  dynamically bound to true."
  (binding [m/*check-types* true]
    (m/do hash-set
          [x (into [] (range 5))
           y (into #{} (range 3))]
          (+ x y))))


(defex ex6
  "Will not throw an exception since monads.core/*check-types* is bound
  to false by default."
  (m/do hash-set
        [x (into [] (range 5))
         y (into #{} (range 3))]
        (+ x y)))


(defex ex7
  (m/do lazy-seq
        [x (range 5)
         y (range (+ 1 x))
         :when (= (+ x y) 2)]
        (list x y)))


(defex ex8
  (m/do lazy-seq
        [x (range 5)
         y (range (+ 1 x))
         :let [sum (+ x y)
               diff (- x y)]
         :when (= sum 2)]
        (list diff)))


(defex ex9
  (let [pairs (fn [xs]
                ((m/lift #(list %1 %2)) xs xs))]
    (pairs (range 5))))


(defex ex10
  (let [pairs (fn [xs]
                (m/seq [xs xs]))]
    (pairs (range 5))))


(defex ex11
  (let [ntuples (fn [n xs]
                  (m/seq (repeat n xs)))]
    [(symbol "(ntuples 2) =>") (ntuples 2 (range 5))
     (symbol "(ntuples 3) =>") (ntuples 3 (range 5))]))


(defex ex12
  (m/do list
        [x ((m/lift (partial * 2)) (range 5))
         y (range 2)]
        [x y]))


(defex ex13
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


(defex ex14
  [(symbol "@(some-function 2 3) =>") @(some-function 2 3)
   (symbol "@(some-function 2 0) =>") @(some-function 2 0)
   (symbol "(some-function 2 -2) =>") (some-function 2 -2)])


(defex ex15
  (m/plus [(some-function 2 0)
           (some-function 2 -2)
           (some-function 2 3)]))


(def rng
  (let [m 259200
        next (m/update-state (fn [seed] (rem (+ 54773 (* 7141 seed)) m)))
        value (fn [seed] (m/state (/ (float seed) (float m))))]
    (m/state next value)))

(defn value-seq [f seed]
  (lazy-seq
    (let [[value next] (f seed)]
      (cons value (value-seq f next)))))

(defn sum [xs]  (apply + xs))
(defn mean [xs]  (/ (sum xs) (count xs)))
(defn variance [xs]
  (let [m (mean xs)
        sq #(* % %)]
    (mean (for [x xs] (sq (- x m))))))


(defex ex16
  [(symbol "mean =>") (mean (take 1000 (value-seq rng 1)))
   (symbol "variance =>") (variance (take 1000 (value-seq rng 1)))])


(defex ex17
  (let [gaussian
        (m/do m/state
              [x1 rng
               x2 rng
               x3 rng
               x4 rng
               x5 rng
               x6 rng
               x7 rng
               x8 rng
               x9 rng
               x10 rng
               x11 rng
               x12 rng]
              (- (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) 6.))]
    [(symbol "mean =>") (mean (take 1000 (value-seq gaussian 1)))
     (symbol "variance =>") (variance (take 1000 (value-seq gaussian 1)))]))


(defex ex18
  (let [gaussian
        (m/do m/state
              [sum12 (reduce (m/lift +) (repeat 12 rng))]
              (- sum12 6.))]
    [(symbol "mean =>") (mean (take 1000 (value-seq gaussian 1)))
     (symbol "variance =>") (variance (take 1000 (value-seq gaussian 1)))]))


(defex ex19
  (let [gaussian
        (m/do m/state
              [sum12 (m/reduce + (repeat 12 rng))]
              (- sum12 6.))]
    [(symbol "mean =>") (mean (take 1000 (value-seq gaussian 1)))
     (symbol "variance =>") (variance (take 1000 (value-seq gaussian 1)))]))


(defex ex20
  (let [gaussian
        ((m/lift #(- % 6.))
         (m/reduce + (repeat 12 rng)))]
    [(symbol "mean =>") (mean (take 1000 (value-seq gaussian 1)))
     (symbol "variance =>") (variance (take 1000 (value-seq gaussian 1)))]))


(defex ex21
  (let [rng2 (m/seq [rng rng])]
    (rng2 1)))


(defex ex22
  (let [identical-random-seqs
        (m/do m/state
              [seed (m/get-state)
               x1   rng
               x2   rng
               _    (m/set-state seed)
               y1   rng
               y2   rng]
              (list [x1 x2] [y1 y2]))]
    (identical-random-seqs 1)))


(defex ex23
  (let [writer+str (m/writer "")
        write (fn [msg] (m/write-writer writer+str msg))]
    @(m/do writer+str
           [x (writer+str 1)
            _ (write "first step\n")
            y (writer+str 2)
            _ (write "second step\n")]
           (+ x y))))


(defex ex24
  (let [writer+vec (m/writer [])
        write (fn [msg] (m/write-writer writer+vec msg))
        fib-trace (fn fib-trace [n]
                    (if (< n 2)
                      (writer+vec n)
                      (m/do writer+vec
                            [n1 (writer+vec (dec n))
                             n2 (writer+vec (dec n1))
                             f1 (fib-trace n1)
                             _  (write [n1 f1])
                             f2 (fib-trace n2)
                             _  (write [n2 f2])]
                            (+ f1 f2))))]
    @(fib-trace 5)))


(defex ex25
  (let [list-maybe (m/maybe-t list)
        odds (apply list-maybe
                    (for [n (range 10)] (when (odd? n) n)))
        evens (apply list-maybe
                     (for [n (range 10)] (when (even? n) n)))]
    (m/fmap #(deref %)
            @(m/do list-maybe
                   [x odds
                    y evens]
                   [(inc x) (dec y)]))))


(defex ex26
  (let [list-maybe (m/maybe-t list)
        list*-maybe (partial apply list-maybe)]
    (m/fmap #(deref %)
            @(m/do list-maybe
                   [x (list*-maybe (range 10))
                    y (list*-maybe (range 10))
                    :when (and (odd? x)
                               (even? y))]
                   [(inc x) (dec y)]))))


(defex ex27
  (let [pairs-maybe
        (fn [xs]
          (let [xsm (apply (m/maybe-t list) xs)]
            (m/seq [xsm xsm])))]
    (m/fmap #(deref %)
            @(pairs-maybe (for [n (range 5)] (when (odd? n) n))))))


(defex ex28
  @(m/do m/cont
         [x (m/cont 1)
          y (m/cont 2)]
         (+ x y)))


(comment

  "The remaining examples involving call-cc have not been ported since
  this library's call-cc is not yet implemented."

  )
