(ns aoc2022.day8
  (:require [aoc2022.utils :refer [read-input-as-string-vector transpose]]
            [clojure.string :as str]))

(defn read-numeric-grid
  [file]
  (->> file
       read-input-as-string-vector
       (mapv #(mapv read-string (str/split % #"")))))

(def G (read-numeric-grid "day8-input.txt"))
(def M (count G))
(def Gt (transpose G))
(def N (count Gt))

(defn sub-row
  [n begin end]
  (->> G
       (#(nth % (dec n)))
       (#(subvec % (dec begin) end))))

(defn sub-col
  [n begin end]
  (->> Gt
       (#(nth % (dec n)))
       (#(subvec % (dec begin) end))))

(defn visible?
  [i j]
  (let [el (nth (nth G (dec i)) (dec j))
        left (sub-row i 1 (dec j))
        right (sub-row i (inc j) N)
        up (sub-col j 1 (dec i))
        down (sub-col j (inc i) M)]
    (if (some #(every? (fn [x] (< x el)) %)
              [left right up down])
      true
      false)))

(defn solution1
  []
  (let [visibility (for [i (range M)
                         j (range N)]
                     (visible? (inc i) (inc j)))]
    (->> visibility
         (filter #(= % true))
         count)))
