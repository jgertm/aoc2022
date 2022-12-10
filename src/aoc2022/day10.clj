(ns aoc2022.day10
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]))



(def sample
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(defn parse
  [s]
  (->> s
       (re-seq #"(\w+)(?: (-?\d+))?")
       (map next)
       (map (fn [[insn n]] (filterv some? [(keyword insn) (some-> n parse-long)])))))

(defn interpret
  [insns]
  (reductions
   (fn [state insn]
     (case (first insn)
       :noop (update state :cycles inc)
       :addx (-> state
                 (update :cycles + 2)
                 (update :x + (second insn)))))
   {:x 1 :cycles 0}
   insns))

(defn measure
  [n states]
  (->> states
       (take-while #(-> % :cycles (< n)))
       last
       :x
       (* n)))

(deftest measure-test
  (is (= 420 (measure 20 (interpret (parse sample)))))
  (is (= 1140 (measure 60 (interpret (parse sample)))))
  (is (= 1800 (measure 100 (interpret (parse sample)))))
  (is (= 2940 (measure 140 (interpret (parse sample)))))
  (is (= 2880 (measure 180 (interpret (parse sample)))))
  (is (= 3960 (measure 220 (interpret (parse sample))))))

(defn solve-part1
  [x]
  (->> [20 60 100 140 180 220]
       (map #(measure % (->> x parse interpret)))
       (reduce +)))

(deftest solve-part1-test
  (is (= 13140 (solve-part1 sample))))

(def input
  (slurp (io/resource "day10.txt")))

(comment
  (solve-part1 input)

  )

(interpret (parse sample))
