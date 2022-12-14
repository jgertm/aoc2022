(ns aoc2022.day14
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def sample
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse
  [s]
  (->> s
       str/split-lines
       (map (fn [line]
              (->> line
                  (re-seq #"(\d+),(\d+)")
                  (map
                   (fn [match]
                     (mapv (fn [val] (parse-long val)) (next match)))))))))

(defn layout
  [x]
  (let [rocks
        (into #{}
              (for [shape x
                    segment (partition 2 1 shape)
                    :let [[[x1 y1] [x2 y2]] segment]
                    x (range (min x1 x2) (inc (max x1 x2)))
                    y (range (min y1 y2) (inc (max y1 y2)))]
                [x y]))]
    {:rocks rocks}))

(def origin [500 0])

(defn draw
  [rock sand grain]
  (let [[min-x max-x] (->> rock
                           (map first)
                           (reduce
                            (fn [[l h] n] [(min l n) (max h n)])
                            [Long/MAX_VALUE Long/MIN_VALUE]))
        row (inc (- max-x min-x))
        max-y (reduce max (map second rock))]
    (letfn [(layer [coords sym buffer]
              (->> coords
                   (filter some?)
                   (reduce
                    (fn [acc [x y]] (assoc-in acc [y (- x min-x)] sym))
                    buffer)))]
      (->> (vec (repeat (inc max-y) (vec (repeat row "."))))
           (layer rock "#")
           (layer sand "o")
           (layer #{grain} "+")
           (map (partial apply str))
           (str/join "\n")))))

(defn simulate
  [{:keys [rocks]}]
  (letfn [(down [[x y]] [x (inc y)])
          (diag-left [[x y]] [(dec x) (inc y)])
          (diag-right [[x y]] [(inc x) (inc y)])
          (fall [sand grain]
            (->> [down diag-left diag-right identity]
                 (map #(% grain))
                 (some #(and (not (contains? (set/union rocks sand) %)) %))))]
    (let [highest-y (reduce max (map second rocks))]
     (loop [sand #{}
            grain origin]
       ;; (println "\n\n")
       ;; (println (count sand))
       ;; (println (draw rocks sand grain))
       (if (not= (fall sand grain) grain)
         (let [[_x y :as next] (fall sand grain)]
           (if (>= y highest-y)
             sand
             (recur sand next)))
         (recur (conj sand grain) origin))))))

(defn solve-part1
  [x]
  (->> x
       parse
       layout
       simulate
       count))

(deftest solve-part1-test
  (is (= 24 (solve-part1 sample))))

(def input
  (slurp (io/resource "day14.txt")))

(comment
  (solve-part1 input)

  )
