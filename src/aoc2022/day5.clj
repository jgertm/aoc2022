(ns aoc2022.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

;; --- Day 5: Supply Stacks ---

;; The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need to be rearranged.

;; The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.

;; The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.

;; They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:

;;     [D]
;; [N] [C]
;; [Z] [M] [P]
;;  1   2   3

;; move 1 from 2 to 1
;; move 3 from 1 to 3
;; move 2 from 2 to 1
;; move 1 from 1 to 2

;; In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.

;; Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:

;; [D]
;; [N] [C]
;; [Z] [M] [P]
;;  1   2   3

;; In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:

;;         [Z]
;;         [N]
;;     [C] [D]
;;     [M] [P]
;;  1   2   3

;; Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:

;;         [Z]
;;         [N]
;; [M]     [D]
;; [C]     [P]
;;  1   2   3

;; Finally, one crate is moved from stack 1 to stack 2:

;;         [Z]
;;         [N]
;;         [D]
;; [C] [M] [P]
;;  1   2   3

;; The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.

;; After the rearrangement procedure completes, what crate ends up on top of each stack?

(def sample
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse
  [s]
  (let [[initial moves]
        (->> s
             str/split-lines
             (partition-by empty?)
             (remove (partial every? empty?)))
        buckets (->> initial
                     last
                     (re-seq #"\d+")
                     (map parse-long))
        layers (->> initial
                    butlast
                    (map #(->> %
                               (re-seq #"(?:   |\[(\w)\])\s?")
                               (map last)
                               (zipmap buckets))))]
    {:initial (reduce (partial merge-with (fn [b v] (if v (conj b v) b)))
                      (zipmap buckets (repeat []))
                      (reverse layers))
     :moves (->> moves
                 (map #(->> % (re-find #"move (\d+) from (\d+) to (\d+)") next (map parse-long)))
                 (map (partial zipmap [:count :from :to])))}))

(deftest parse-test
  (is (= {1 ["Z" "N"]
          2 ["M" "C" "D"]
          3 ["P"]}
         (:initial (parse sample))))
  (is (= {:count 1 :from 2 :to 1}
         (first (:moves (parse sample))))))

(defn move
  [stacks {:keys [count from to]}]
  (let [moved (take-last count (get stacks from))]
    (-> stacks
        (update from #(->> % (drop-last count) vec))
        (update to (fnil into []) (reverse moved)))))

(deftest move-test
  (let [{:keys [initial]} (parse sample)]
    (is (= {1 ["Z" "N" "D"]
            2 ["M" "C"]
            3 ["P"]}
           (move initial {:count 1 :from 2 :to 1})))))

(defn rearrange
  [{:keys [initial moves]}]
  (reduce move initial moves))

(deftest rearrange-test
  (is (= {1 ["C"]
          2 ["M"]
          3 ["P" "D" "N" "Z"]}
         (rearrange (parse sample)))))

(defn tops
  [stacks]
  (map #(-> % val peek) (sort stacks)))

(deftest tops-test
  (is (= ["M" "C" "Z"]
         (tops {1 ["M"]
                2 ["C"]
                3 ["P" "D" "N" "Z"]}))))

(defn solve-part1
  [x]
  (->> x
      parse
      rearrange
      tops
      (apply str)))

(deftest solve-part1-test
  (is (= "CMZ"
         (solve-part1 sample))))

(def input
  (slurp (io/resource "day5.txt")))

(comment
  (solve-part1 input)

  )
