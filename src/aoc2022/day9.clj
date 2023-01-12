(ns aoc2022.day9
  (:require [aoc2022.utils :refer [read-input-as-string-vector]]
            [clojure.string :as str]))

(defn expand-instruction
  [ins]
  (let [ins1 (str/split ins #" ")
        direction (first ins1)
        times (read-string (second ins1))]
    (repeat times direction)))

(defn expand-input
  [file]
  (->> file
       read-input-as-string-vector
       (mapv expand-instruction)
       (reduce concat)))

;; UGHH...
(defn tail-update
  [[xh yh] [xt yt]]
  (cond
    (and (= xh xt) (= (- yh yt) 2)) [xt (inc yt)] ;; head je S od tail
    (and (= (- xh xt) 1) (= (- yh yt) 2)) [(inc xt) (inc yt)] ;; head je SV od tail
    (and (= (- xt xh) 1) (= (- yh yt) 2)) [(dec xt) (inc yt)] ;; head je SZ od tail

    (and (= xh xt) (= (- yt yh) 2)) [xt (dec yt)] ;; head je J od tail
    (and (= (- xt xh) 1) (= (- yt yh) 2)) [(dec xt) (dec yt)] ;; head je JV od tail
    (and (= (- xh xt) 1) (= (- yt yh) 2)) [(inc xt) (dec yt)] ;; head je JZ od tail

    (and (= (- xh xt) 2) (= yt yh)) [(inc xt) yt] ;; head je V od tail
    (and (= (- xh xt) 2) (= (- yt yh) 1)) [(inc xt) (dec yt)] ;; head je VS od tail
    (and (= (- xh xt) 2) (= (- yh yt) 1)) [(inc xt) (inc yt)] ;; head je VJ od tail

    (and (= (- xt xh) 2) (= yt yh)) [(dec xt) yt] ;; head je Z od tail
    (and (= (- xt xh) 2) (= (- yt yh) 1)) [(dec xt) (dec yt)] ;; head je ZJ od tail
    (and (= (- xt xh) 2) (= (- yh yt) 1)) [(dec xt) (inc yt)] ;; head je ZS od tail

    :else [xt yt] 
    ))

(defn head-update
  [[xh yh] ins]
  (cond
    (= ins "R") [(inc xh) yh]
    (= ins "L") [(dec xh) yh]
    (= ins "U") [xh (inc yh)]
    (= ins "D") [xh (dec yh)]))

(defn tail-trace
  [instructions h-pos t-pos t-trace]
  (cond
    (not= instructions []) (let [ins (first instructions)
                                 new-h-pos (head-update h-pos ins)
                                 new-t-pos (tail-update new-h-pos t-pos)]
                             (recur  (rest instructions) new-h-pos new-t-pos (conj t-trace new-t-pos)))
    :else t-trace))

(defn solution1
  [file]
  (->> file
       expand-input
       (#(tail-trace % [0 0] [0 0] [[0 0]]))
       set
       count))

;; UGHHHH -_-...
;; It's latte and I need late.
(defn tail-trace2
  [instructions h t1 t2 t3 t4 t5 t6 t7 t8 t9 t-trace]
  (cond
    (not= instructions []) (let [ins (first instructions)
                                 new-h (head-update h ins)
                                 new-t1 (tail-update new-h t1)
                                 new-t2 (tail-update new-t1 t2)
                                 new-t3 (tail-update new-t2 t3)
                                 new-t4 (tail-update new-t3 t4)
                                 new-t5 (tail-update new-t4 t5)
                                 new-t6 (tail-update new-t5 t6)
                                 new-t7 (tail-update new-t6 t7)
                                 new-t8 (tail-update new-t7 t8)
                                 new-t9 (tail-update new-t8 t9)]

                             (recur  (rest instructions)
                                     new-h
                                     new-t1
                                     new-t2
                                     new-t3
                                     new-t4
                                     new-t5
                                     new-t6
                                     new-t7
                                     new-t8
                                     new-t9
                                     (conj t-trace new-t9)))
    :else t-trace))


(defn solution2
  [file]
  (->> file
       expand-input
       (#(tail-trace2 % [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [[0 0]]))
       set
       count))


(defn -main
  []
  (printf "Solution1: %s\n", (solution1 "day9-input.txt"))
  (printf "Solution2: %s\n", (solution2 "day9-test.txt")))


(comment
  (expand-instruction "R 4")
  (expand-input "day9-test.txt")
  (tail-update [4 2] [2 2]) ;; [3 2]
  (tail-update [2 2] [2 4]) ;; [2 3]
  (tail-update [3 4] [2 2]) ;; [3 3]
  (tail-update [4 3] [2 2]) ;; [3 3]
  (tail-trace (expand-input "day9-test.txt") [0 0] [0 0] [[0 0]])
  (solution1 "day9-test.txt")
  (tail-trace2 (expand-input "day9-test2.txt") 
                           [0 0] 
                           [0 0]
                           [0 0]
                           [0 0]
                           [0 0]
                           [0 0]
                           [0 0]
                           [0 0]
                           [0 0]
                           [0 0]
                           [[0 0]])
  ,)
