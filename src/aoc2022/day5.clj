(ns aoc2022.day5
  (:require [aoc2022.utils :refer [read-input-as-string-vector]]
            [clojure.string :as str]))

(defn transpose
  [matrix]
  (apply mapv vector matrix))

(defn parse-stacks
  [file]
  (->> file
       read-input-as-string-vector
       (partition-by #(= "" %))
       first
       drop-last
       (mapv #(take-nth 4 (rest %)))
       transpose
       (map reverse)
       (map #(remove (fn [c] (= c \space)) %))
       (mapv #(apply vector %))))

(defn parse-instructions
  [file]
  (->> file
       read-input-as-string-vector
       (partition-by #(= "" %))
       (#(nth % 2))
       (mapv (fn [i]
               (let [[_ & matches] (re-matches  #"move (\d+) from (\d+) to (\d+)" i)]
                 matches)))
       (mapv #(mapv read-string %))))

(defn update-stacks
  [stack [amount from to]]
  (let [i1 (dec from)
        i2 (dec to)
        new-i1 (into [] (drop-last amount (nth stack i1)))
        crates (into [] (take-last amount (nth stack i1)))
        new-i2 (into [] (concat (nth stack i2) (reverse crates)))]
    (->> stack
         (#(assoc % i1 new-i1))
         (#(assoc % i2 new-i2)))))

(defn solution1
  [file]
  (let [stack (parse-stacks file)
        instructions (parse-instructions file)]
    (->>
     (reduce update-stacks stack instructions)
     (mapv last)
     (apply str))))

(defn update-stacks-9001
  [stack [amount from to]]
  (let [i1 (dec from)
        i2 (dec to)
        new-i1 (into [] (drop-last amount (nth stack i1)))
        crates (into [] (take-last amount (nth stack i1)))
        new-i2 (into [] (concat (nth stack i2) crates))]
    (->> stack
         (#(assoc % i1 new-i1))
         (#(assoc % i2 new-i2)))))

(defn solution2
  [file]
  (let [stack (parse-stacks file)
        instructions (parse-instructions file)]
    (->>
     (reduce update-stacks-9001 stack instructions)
     (mapv last)
     (apply str))))

(defn -main
  []
  (printf "Solution 1: %s\n", (solution1 "day5-input.txt"))
  (printf "Solution 2: %s\n", (solution2 "day5-input.txt")))
