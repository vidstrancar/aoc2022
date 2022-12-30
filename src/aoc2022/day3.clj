(ns aoc2022.day3
  (:require [aoc2022.utils :refer [read-input-as-string-vector]]
            [clojure.string :as str]
            [clojure.set :refer [intersection]]))

(defn halve-str
  [s]
  (let [new-len (/ (count s) 2)
        s-vec (apply vector (seq s))]
    [(subvec s-vec 0 new-len)
     (subvec s-vec new-len)]))

(defn find-faulty
  [[left right]]
  (first (into [](intersection (set left) (set right)))))

(defn get-priority
  [c]
  (if (Character/isUpperCase c) (+ (- (int c) 65) 27) (inc (- (int c) 97))))

(defn solution1
  [file]
  (->> file
       read-input-as-string-vector
       (mapv halve-str)
       (mapv find-faulty)
       (mapv get-priority)
       (reduce +)))

(defn solution2
  [file]
  (->> file
       read-input-as-string-vector
       (partition 3)
       (map get-group-badge)
       (map get-priority)
       (reduce +)))

(defn get-group-badge
  [[b1 b2 b3]]
  (first (into [](intersection (set b1) (set b2) (set b3)))))

(defn -main
  []
  (printf "Solution 1: %s\n", (solution1 "day3-input.txt"))
  (printf "Solution 2: %s\n", (solution2 "day3-input.txt")))
