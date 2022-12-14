(ns aoc2022.day11
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [clojure.string :as str]))

(def sample
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn parse
  [s]
  (letfn [(reify-op [[x op y]]
            (let [x (if (= x "old") (symbol x) (parse-long x))
                  y (if (= y "old") (symbol y) (parse-long y))]
              (eval (list 'fn 'monkey-op ['old] (list (symbol op) x y)))))
          (parse-monkey [[l1 l2 l3 l4 l5 l6]]
            (let [divisor (->> l4 (re-find #"divisible by (\d+)") last parse-long)]
              [(->> l1 (re-find #"\d+") parse-long)
               {:count     0
                :inbox     (->> l2 (re-seq #"\d+") (map parse-long) vec)
                :operation (->> l3 (re-find #"(old|[\d]+) ([*+]) (old|[\d]+)") next reify-op)
                :divisor   divisor
                :test      #(zero? (mod % divisor))
                true       (->> l5 (re-find #"throw to monkey (\d+)") last parse-long)
                false      (->> l6 (re-find #"throw to monkey (\d+)") last parse-long)}]))]
    (->> s
         str/split-lines
         (partition-by empty?)
         (remove (partial every? empty?))
         (map parse-monkey)
         (into {}))))

(def ^:dynamic *div* nil)

(defn turn
  [state id]
  (let [pod (->> state vals (map :divisor) (reduce *))
        {:keys [inbox operation test] :as monkey} (get state id)]
    (reduce
     (fn [state item]
       (let [item (-> item operation (/ *div*) long)]
         (-> state
             (update-in [id :count] inc)
             (update-in [(get monkey (test item)) :inbox] conj (mod item pod)))))
     (assoc-in state [id :inbox] [])
     inbox)))

(defn round
  [state]
  (reduce turn state (sort (keys state))))

(defn rounds
  [n state]
  (-> round
      (iterate state)
      (nth n)))

(defn monkey-business
  [n x]
  (->> x
       (rounds n)
       vals
       (map :count)
       sort
       (take-last 2)
       (reduce *)))

(defn solve-part1
  [x]
  (binding [*div* 3]
    (->> x
         parse
         (monkey-business 20))))

(deftest solve-part1-test
  (is (= 10605 (solve-part1 sample))))

(def input
  (slurp (io/resource "day11.txt")))

(comment
  (solve-part1 input)

  )

(defn solve-part2
  [x]
  (binding [*div* 1]
    (->> x
         parse
         (monkey-business 10000))))

(deftest solve-part2-test
  (is (= 2713310158 (solve-part2 sample))))

(comment
  (solve-part2 input)

  )
