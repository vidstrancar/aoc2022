(ns aoc22-1.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input-as-string
  [fname]
  (slurp (io/resource fname)))

(defn parse-calories
  [data]
  (->> (str/split data #"\n\n")
       (map str/split-lines)
       (map #(remove str/blank? %))
       (map (fn [cals] (map #(Integer/parseInt %) cals)))))

(def data (read-input-as-string "day1-input.txt"))

(defn -main
  []
  (let [sorted-cals (->> data
                         parse-calories
                         (map #(reduce + %))
                         (sort >))
        solution1 (first sorted-cals)
        solution2 (reduce + (take 3 sorted-cals))]
    (printf "Solution 1: %s\n", solution1)
    (printf "Solution 2: %s\n", solution2)))
