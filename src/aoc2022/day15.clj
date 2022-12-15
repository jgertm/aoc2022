(ns aoc2022.day15
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.set :as set]
   [clojure.java.io :as io]))

(def sample
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn parse
  [s]
  (->> s
       (re-seq #"(?:x|y)=(-?\d+)")
       (map #(-> % second parse-long))
       (partition 2)
       (map (partial zipmap [:x :y]))
       (partition 2)
       (map (partial zipmap [:sensor :beacon]))))

(deftest parse-test
  (is (= {:sensor {:x 2 :y 18} :beacon {:x -2 :y 15}}
         (first (parse sample)))))

(defn distance
  [a b]
  (->> (merge-with - a b)
       vals
       (map abs)
       (reduce +)))

(defn width
  [{:keys [sensor beacon]} row]
  (let [{:keys [y]} sensor
        range (distance sensor beacon)]
    (+ 1 (* 2 (- range (abs (- row y)))))))

(defn positions
  [{:keys [sensor] :as pair} row]
  (when (pos? (width pair row))
    (let [{:keys [x]} sensor
          reach (quot (width pair row) 2)]
      (map (fn [x] {:x x :y row})
           (range (- x reach) (+ x reach 1))))))

(defn solve-part1
  [row x]
  (let [parsed (parse x)
        covered
        (->> parsed
             (sort-by (comp :y :sensor))
             (filter (fn [{:keys [sensor beacon]}]
                         (>= (distance sensor beacon)
                             (abs (- row (:y sensor))))))
             (mapcat (fn [pair] (positions pair row)))
             (into #{}))
        beacons (map :beacon parsed)]
    (count (set/difference covered beacons))))

(deftest solve-part1-test
  (is (= 26 (solve-part1 10 sample))))

(def input
  (slurp (io/resource "day15.txt")))

(comment
  (solve-part1 2000000 input)

  )

;;                1    1    2    2
;;      0    5    0    5    0    5
;; -2 ..........#.................
;; -1 .........###................
;;  0 ....S...#####...............
;;  1 .......#######........S.....
;;  2 ......#########S............
;;  3 .....###########SB..........
;;  4 ....#############...........
;;  5 ...###############..........
;;  6 ..#################.........
;;  7 .#########S#######S#........
;;  8 ..o################.........
;;  9 .ooo##############..........
;; 10 ooooB############........... 13 wide
;; 11oooSooo##########............
;; 12 ......#########.............
;; 13 .......#######..............
;; 14 ........#####.S.......S.....
;; 15 B........###................
;; 16 ..........#SB...............
;; 17 ................S..........B
;; 18 ....S.......................
;; 19 ............................
;; 20 ............S......S........
;; 21 ............................
;; 22 .......................B....
