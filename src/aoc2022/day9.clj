(ns aoc2022.day9
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]))

;; --- Day 9: Rope Bridge ---

;; This rope bridge creaks as you walk along it. You aren't sure how old it is, or whether it can even support your weight.

;; It seems to support the Elves just fine, though. The bridge spans a gorge which was carved out by the massive river far below you.

;; You step carefully; as you do, the ropes stretch and twist. You decide to distract yourself by modeling rope physics; maybe you can even figure out where not to step.

;; Consider a rope with a knot at each end; these knots mark the head and the tail of the rope. If the head moves far enough away from the tail, the tail is pulled toward the head.

;; Due to nebulous reasoning involving Planck lengths, you should be able to model the positions of the knots on a two-dimensional grid. Then, by following a hypothetical series of motions (your puzzle input) for the head, you can determine how the tail will move.

;; Due to the aforementioned Planck lengths, the rope must be quite short; in fact, the head (H) and tail (T) must always be touching (diagonally adjacent and even overlapping both count as touching):

;; ....
;; .TH.
;; ....

;; ....
;; .H..
;; ..T.
;; ....

;; ...
;; .H. (H covers T)
;; ...

;; If the head is ever two steps directly up, down, left, or right from the tail, the tail must also move one step in that direction so it remains close enough:

;; .....    .....    .....
;; .TH.. -> .T.H. -> ..TH.
;; .....    .....    .....

;; ...    ...    ...
;; .T.    .T.    ...
;; .H. -> ... -> .T.
;; ...    .H.    .H.
;; ...    ...    ...

;; Otherwise, if the head and tail aren't touching and aren't in the same row or column, the tail always moves one step diagonally to keep up:

;; .....    .....    .....
;; .....    ..H..    ..H..
;; ..H.. -> ..... -> ..T..
;; .T...    .T...    .....
;; .....    .....    .....

;; .....    .....    .....
;; .....    .....    .....
;; ..H.. -> ...H. -> ..TH.
;; .T...    .T...    .....
;; .....    .....    .....

;; You just need to work out where the tail goes as the head follows a series of motions. Assume the head and the tail both start at the same position, overlapping.

;; For example:

;; R 4
;; U 4
;; L 3
;; D 1
;; R 4
;; D 1
;; L 5
;; R 2

;; This series of motions moves the head right four steps, then up four steps, then left three steps, then down one step, and so on. After each step, you'll need to update the position of the tail if the step means the head is no longer adjacent to the tail. Visually, these motions occur as follows (s marks the starting position as a reference point):

;; == Initial State ==

;; ......
;; ......
;; ......
;; ......
;; H.....  (H covers T, s)

;; == R 4 ==

;; ......
;; ......
;; ......
;; ......
;; TH....  (T covers s)

;; ......
;; ......
;; ......
;; ......
;; sTH...

;; ......
;; ......
;; ......
;; ......
;; s.TH..

;; ......
;; ......
;; ......
;; ......
;; s..TH.

;; == U 4 ==

;; ......
;; ......
;; ......
;; ....H.
;; s..T..

;; ......
;; ......
;; ....H.
;; ....T.
;; s.....

;; ......
;; ....H.
;; ....T.
;; ......
;; s.....

;; ....H.
;; ....T.
;; ......
;; ......
;; s.....

;; == L 3 ==

;; ...H..
;; ....T.
;; ......
;; ......
;; s.....

;; ..HT..
;; ......
;; ......
;; ......
;; s.....

;; .HT...
;; ......
;; ......
;; ......
;; s.....

;; == D 1 ==

;; ..T...
;; .H....
;; ......
;; ......
;; s.....

;; == R 4 ==

;; ..T...
;; ..H...
;; ......
;; ......
;; s.....

;; ..T...
;; ...H..
;; ......
;; ......
;; s.....

;; ......
;; ...TH.
;; ......
;; ......
;; s.....

;; ......
;; ....TH
;; ......
;; ......
;; s.....

;; == D 1 ==

;; ......
;; ....T.
;; .....H
;; ......
;; s.....

;; == L 5 ==

;; ......
;; ....T.
;; ....H.
;; ......
;; s.....

;; ......
;; ....T.
;; ...H..
;; ......
;; s.....

;; ......
;; ......
;; ..HT..
;; ......
;; s.....

;; ......
;; ......
;; .HT...
;; ......
;; s.....

;; ......
;; ......
;; HT....
;; ......
;; s.....

;; == R 2 ==

;; ......
;; ......
;; .H....  (H covers T)
;; ......
;; s.....

;; ......
;; ......
;; .TH...
;; ......
;; s.....

;; After simulating the rope, you can count up all of the positions the tail visited at least once. In this diagram, s again marks the starting position (which the tail also visited) and # marks other positions the tail visited:

;; ..##..
;; ...##.
;; .####.
;; ....#.
;; s###..

;; So, there are 13 positions the tail visited at least once.

;; Simulate your complete hypothetical series of motions. How many positions does the tail of the rope visit at least once?

(def sample
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn parse
  [x]
  (->> x
       (re-seq #"(\w) (\d)")
       (map (fn [[_ d n]] [(keyword d) (parse-long n)]))))

(defn move
  [point dir]
  (case dir
    :L (update point :x dec)
    :R (update point :x inc)
    :U (update point :y inc)
    :D (update point :y dec)))

(defn trail
  [head tail]
  (let [{:keys [x y] :as diff} (merge-with - head tail)]
    (letfn [(diagonal? [diff]
              (->> diff
                   vals
                   (map abs)
                   (reduce +)
                   (<= 3)))]
      (cond
        (and (diagonal? diff) (pos? x) (pos? y)) (-> tail (move :R) (move :U))
        (and (diagonal? diff) (pos? x) (neg? y)) (-> tail (move :R) (move :D))
        (and (diagonal? diff) (neg? x) (pos? y)) (-> tail (move :L) (move :U))
        (and (diagonal? diff) (neg? x) (neg? y)) (-> tail (move :L) (move :D))
        (= 2 x) (move tail :R)
        (= -2 x) (move tail :L)
        (= 2 y) (move tail :U)
        (= -2 y) (move tail :D)
        :else tail))))

(deftest trail-test
  (is (= {:x 1 :y 1}
         (trail {:x 2 :y 1} {:x 1 :y 1})))
  (is (= {:x 2 :y 1}
         (trail {:x 1 :y 2} {:x 2 :y 1})))
  (is (= {:x 1 :y 1}
         (trail {:x 1 :y 1} {:x 1 :y 1})))
  (is (= {:x 2 :y 1}
         (trail {:x 3 :y 1} {:x 1 :y 1})))
  (is (= {:x 1 :y 2}
         (trail {:x 1 :y 1} {:x 1 :y 3})))
  (is (= {:x 2 :y 2}
         (trail {:x 2 :y 3} {:x 1 :y 1})))
  (is (= {:x 2 :y 2}
         (trail {:x 3 :y 2} {:x 1 :y 1}))))

(defn step
  [{:keys [head tail] :as state} dir]
  (let [head (move head dir)
        tail (trail head tail)]
    (-> state
        (assoc :head head)
        (assoc :tail tail))))

(defn solve-part1
  [x]
  (->> x
       parse
       (mapcat (fn [[d n]] (repeat n d)))
       (reduce
        (fn [state dir]
          (let [{:keys [head tail] :as state} (step state dir)]
            (-> state
                (update :heads conj head)
                (update :tails conj tail))))
        {:head {:x 0 :y 0}
         :tail {:x 0 :y 0}
         :tails #{}})
       :tails
       count))

(deftest solve-part1-test
  (is (= 13 (solve-part1 sample))))

(def input
  (slurp (io/resource "day9.txt")))

(comment
  (solve-part1 input)

  )
