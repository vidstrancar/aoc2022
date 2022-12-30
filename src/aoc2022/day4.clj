(ns aoc2022.day4
  (:require [aoc2022.utils :refer [read-input-as-string-vector]]
            [clojure.string :as str]))

(defn full-overlap?
  [[a b c d]]
  (cond
    (and (<= a c) (>= b d)) true
    (and (<= c a) (>= d b)) true
    :else false))

(defn solution1
  [file]
  (->> file
       read-input-as-string-vector
       (mapv #(str/split % #"-|,"))
       (mapv #(mapv read-string %))
       (filter full-overlap?)
       (count)))

(defn partial-overlap?
  [[a b c d]]
  (cond
    (and (< b c) (< b d)) false
    (and (< d a) (< d b)) false
    :else true))

(defn solution2
  [file]
  (->> file
       read-input-as-string-vector
       (mapv #(str/split % #"-|,"))
       (mapv #(mapv read-string %))
       (filter partial-overlap?)
       (count)))

(defn -main
  []
  (printf "Solution 1: %s\n", (solution1 "day4-input.txt"))
  (printf "Solution 2: %s\n", (solution2 "day4-input.txt")))
