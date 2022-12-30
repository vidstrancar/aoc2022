(ns aoc2022.day6
  (:require [aoc2022.utils :refer [read-input-as-string]]
            [clojure.string :as str]))

(defn no-duplicates?
  [q]
  (if (= (count (set (mapv second q)))
         (count q))
    true
    false))

(defn solution
  [file n]
  (->> file
       read-input-as-string
       (filterv #(Character/isLetter %))
       (map-indexed vector)
       (partition n 1)
       (filterv no-duplicates?)
       first
       last
       first
       inc)) 

(defn -main
  []
  (printf "Solution 1: %s\n", (solution "day6-input.txt" 4))
  (printf "Solution 2: %s\n", (solution "day6-input.txt" 14)))
