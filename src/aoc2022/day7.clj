(ns aoc2022.day7
  (:require [aoc2022.utils :refer [read-input-as-string-vector]]
            [clojure.string :as str]))

(def input (map #(str/split % #" ")(read-input-as-string-vector "day7-test2.txt")))

;; overcomplicating by building a tree first. not actually used, but parses good tree, so left there.
(defn join-last-two
  [v]
  (let [n (dec (count v))
        last (nth v n)
        pre-last (nth v (dec n))]
    (into [] (drop-last (assoc v (dec n) (conj pre-last last))))))

(defn append-to-last
  [v val]
  (let [n (dec (count v))
        last (nth v n)]
    (assoc v n (conj last val))))

(defn parse-input
  [input levels]
  (let [[c1 c2 c3 :as instruction] (first input)]
    (cond
      (= instruction nil) (last (take (count levels) (iterate join-last-two levels)))
      (and (= c2 "cd") (not= c3 "..")) (parse-input (rest input)
                                                     (conj levels [c3]))
      (and (= c2 "cd") (= c3 "..")) (parse-input (rest input)
                                                  (join-last-two levels))
      (every? #(Character/isDigit %) c1) (parse-input (rest input)
                                                       (append-to-last levels [c2 c1]))
      :else (parse-input (rest input) levels))))

;; actual solution starts here
(defn custom-update
  [d vs f]
  (reduce (fn [dd el] (update dd el f))
          d
          vs))

(defn full-name
  [path fname]
  (str/join "/" (conj path fname)))

(defn gen-full-names
  [stack]
  (reduce #(conj %1 (full-name [(last %1)] %2)) [(first stack)] (rest stack)))

(defn dir-sizes
  [input stack dsizes]
  (let [[c1 c2 c3 :as instruction] (first input)]
    (cond
      (= instruction nil) dsizes
      (and (= c2 "cd") (not= c3 "..")) (dir-sizes (rest input)
                                                  (conj stack c3)
                                                  (conj dsizes {(full-name stack c3) 0}))
      (and (= c2 "cd") (= c3 "..")) (dir-sizes (rest input)
                                               (into [] (drop-last stack))
                                               dsizes)
      (every? #(Character/isDigit %) c1) (dir-sizes (rest input)
                                                    stack
                                                    (custom-update dsizes
                                                                   (gen-full-names stack)
                                                                   #(+ (read-string c1) %)))
      :else (dir-sizes (rest input) stack dsizes))))

(defn solution1
  [file]
  (->> file
       read-input-as-string-vector
       (map #(str/split % #" "))
       (#(dir-sizes % [] {}))
       (map second)
       (filter #(<= % 100000))
       (apply +)))

(defn solution2
  [file]
  (let [dsizes (->> file
                    read-input-as-string-vector
                    (map #(str/split % #" "))
                    (#(dir-sizes % [] {})))
        space-taken (get dsizes "/")
        free-space (- 70000000 space-taken)
        space-needed (- 30000000 free-space)]
    (->> dsizes
         (map second)
         (filter #(>= % space-needed))
         (reduce min))))


(defn -main
  []
  (printf "Solution 1: %s\n", (solution1 "day7-input.txt"))
  (printf "Solution 2: %s\n", (solution2 "day7-input.txt")))
