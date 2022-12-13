(ns aoc2022.day13
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [clojure.java.io :as io]))

(def sample
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn parse
  [s]
  (->> s
       str/split-lines
       (remove empty?)
       (map edn/read-string)
       (partition 2)
       (zipmap (map inc (range)))))

(deftest parse-test
  (is (= [[1 1 3 1 1] [1 1 5 1 1]]
         (get (parse sample) 1))))

(defn compare
  [[x & xs] [y & ys]]
  (cond
    (every? int? [x y])
    (cond (< x y) -1
          (> x y) 1
          :else (recur xs ys))

    (and (nil? x) (some? y)) -1

    (and (some? x) (nil? y)) 1

    (every? coll? [x y])
    (cond
      (every? empty? [x y]) (recur xs ys)
      (empty? x) -1
      (empty? y) 1
      :else (or (compare x y) (recur xs ys)))

    (and (int? x) (coll? y))
    (or (compare [x] y) (recur xs ys))

    (and (coll? x) (int? y))
    (or (compare x [y]) (recur xs ys))))

(deftest compare-test
  (is (= -1 (apply compare (get (parse sample) 1))))
  (is (= -1 (apply compare (get (parse sample) 2))))
  (is (=  1 (apply compare (get (parse sample) 3))))
  (is (= -1 (apply compare (get (parse sample) 4))))
  (is (=  1 (apply compare (get (parse sample) 5))))
  (is (= -1 (apply compare (get (parse sample) 6))))
  (is (=  1 (apply compare (get (parse sample) 7))))
  (is (=  1 (apply compare (get (parse sample) 8)))))

(defn solve-part1
  [x]
  (->> x
       parse
       (keep (fn [[k v]] (when (= -1 (apply compare v)) k)))
       (reduce +)))

(deftest solve-part1-test
  (is (= 13 (solve-part1 sample))))

(def input
  (slurp (io/resource "day13.txt")))

(comment
  (solve-part1 input)

  )

