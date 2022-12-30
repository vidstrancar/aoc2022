(ns aoc2022.day2
  (:require [aoc2022.utils :refer [read-input-as-string-vector]]
            [clojure.string :as str]))

(defn parse-strategy
  [data]
  (mapv #(str/split % #" ") data))

(defn normalize
  [shapes]
  (let [shape1 (first shapes)
        shape2 (second shapes)]
    (vector (- (int (first shape1)) (int \A))
            (- (int (first shape2)) (int \X)))))

(defn roundScore
  [shapes]
  (let [shape1 (first shapes)
        shape2 (second shapes)
        shapeScore (+ shape2 1)]
    (cond
      (= (mod (inc shape1) 3) shape2) (+ shapeScore 6)
      (= shape1 shape2) (+ shapeScore 3)
      :else shapeScore)))

(defn solution1
  [file]
  (->> file
       read-input-as-string-vector
       parse-strategy
       (mapv normalize)
       (mapv roundScore)
       (reduce +)))

(defn get-shape2
  [input]
  (let [shape1 (first input)
        outcome (second input)]
    (cond
      (= outcome 0) (mod (dec shape1) 3)
      (= outcome 1) shape1
      :else (mod (inc shape1) 3))))

(defn solution2
  [file]
  (->> file
       read-input-as-string-vector
       parse-strategy
       (mapv normalize)
       (mapv #(vector (first %) (get-shape2 %)))
       (mapv roundScore)
       (reduce +)))

(defn -main
  []
  (printf "Solution 1: %s\n", (solution1 "day2-input.txt"))
  (printf "Solution 2: %s\n", (solution2 "day2-input.txt")))
