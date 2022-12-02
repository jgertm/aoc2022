(ns aoc2022.day2
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;; --- Day 2: Rock Paper Scissors ---

;; The Elves begin to set up camp on the beach. To decide whose tent gets to be closest to the snack storage, a giant Rock Paper Scissors tournament is already in progress.

;; Rock Paper Scissors is a game between two players. Each game contains many rounds; in each round, the players each simultaneously choose one of Rock, Paper, or Scissors using a hand shape. Then, a winner for that round is selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. If both players choose the same shape, the round instead ends in a draw.

;; Appreciative of your help yesterday, one Elf gives you an encrypted strategy guide (your puzzle input) that they say will be sure to help you win. "The first column is what your opponent is going to play: A for Rock, B for Paper, and C for Scissors. The second column--" Suddenly, the Elf is called away to help with someone's tent.

;; The second column, you reason, must be what you should play in response: X for Rock, Y for Paper, and Z for Scissors. Winning every time would be suspicious, so the responses must have been carefully chosen.

;; The winner of the whole tournament is the player with the highest score. Your total score is the sum of your scores for each round. The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

;; Since you can't be sure if the Elf is trying to help you or trick you, you should calculate the score you would get if you were to follow the strategy guide.

;; For example, suppose you were given the following strategy guide:

;; A Y
;; B X
;; C Z

;; This strategy guide predicts and recommends the following:

;;     In the first round, your opponent will choose Rock (A), and you should choose Paper (Y). This ends in a win for you with a score of 8 (2 because you chose Paper + 6 because you won).
;;     In the second round, your opponent will choose Paper (B), and you should choose Rock (X). This ends in a loss for you with a score of 1 (1 + 0).
;;     The third round is a draw with both players choosing Scissors, giving you a score of 3 + 3 = 6.

;; In this example, if you were to follow the strategy guide, you would get a total score of 15 (8 + 1 + 6).

;; What would your total score be if everything goes exactly according to your strategy guide?

(def sample
  (str/join "\n"
            (map #(->> % (map name) (str/join " "))
                 [[:A :Y]
                  [:B :X]
                  [:C :Z]])))

(defn parse
  [s]
  (->> s
       str/split-lines
       (map #(->> % (re-seq #"\S") (map (fn [c] (case c
                                           ("A" "X") :rock
                                           ("B" "Y") :paper
                                           ("C" "Z") :scissors))) vec))))

(deftest parse-test
  (let [result (parse sample)]
    (is (= (first result) [:rock :paper]))))

(defn outcome
  [p]
  (match p
         [:rock :paper] :win
         [:rock :scissors] :loss
         [:paper :scissors] :win
         [:paper :rock] :loss
         [:scissors :rock] :win
         [:scissors :paper] :loss
         _ :draw))

(defn score
  [p]
  (+ (case (second p)
       :rock 1
       :paper 2
       :scissors 3)
   (case (outcome p)
     :loss 0
     :draw 3
     :win 6)))

(deftest score-test
  (is (= (score (first (parse sample))) 8))
  (is (= (score (second (parse sample))) 1))
  (is (= (score (last (parse sample))) 6)))

(defn total
  [g]
  (->> g
       (map score)
       (reduce +)))

(deftest total-test
  (is (= (total (parse sample)) 15)))

(defn solve-part1
  [x]
  (-> x parse total))

(def input
  (slurp (io/resource "day2.txt")))


(comment
  (solve-part1 input)

  )

;; --- Part Two ---

;; The Elf finishes helping with the tent and sneaks back over to you. "Anyway, the second column says how the round needs to end: X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win. Good luck!"

;; The total score is still calculated in the same way, but now you need to figure out what shape to choose so the round ends as indicated. The example above now goes like this:

;;     In the first round, your opponent will choose Rock (A), and you need the round to end in a draw (Y), so you also choose Rock. This gives you a score of 1 + 3 = 4.
;;     In the second round, your opponent will choose Paper (B), and you choose Rock so you lose (X) with a score of 1 + 0 = 1.
;;     In the third round, you will defeat your opponent's Scissors with Rock for a score of 1 + 6 = 7.

;; Now that you're correctly decrypting the ultra top secret strategy guide, you would get a total score of 12.

;; Following the Elf's instructions for the second column, what would your total score be if everything goes exactly according to your strategy guide?


(defn parse2
  [s]
  (->> s
       str/split-lines
       (map #(->> % (re-seq #"\S") (map (fn [c] (case c
                                                  "A" :rock
                                                  "B" :paper
                                                  "C" :scissors
                                                  "X" :loss
                                                  "Y" :draw
                                                  "Z" :win))) vec))))

(deftest parse2-test
  (let [result (parse2 sample)]
    (is (= (first result) [:rock :draw]))))

(defn move
  [p]
  (match p
         [:rock :win] :paper
         [:rock :loss] :scissors
         [:paper :win] :scissors
         [:paper :loss] :rock
         [:scissors :win] :rock
         [:scissors :loss] :paper
         [opponent-move :draw] opponent-move))

(defn score2
  [p]
  (let [my-move (move p)]
    (+ (case my-move
         :rock 1
         :paper 2
         :scissors 3)
       (case (second p)
         :loss 0
         :draw 3
         :win 6))))

(deftest score2-test
  (is (= (score2 (first (parse2 sample))) 4))
  (is (= (score2 (second (parse2 sample))) 1))
  (is (= (score2 (last (parse2 sample))) 7)))

(defn total2
  [g]
  (->> g
       (map score2)
       (reduce +)))

(deftest total2-test
  (is (= (total2 (parse2 sample)) 12)))

(defn solve-part2
  [x]
  (-> x parse2 total2))

(comment
  (solve-part2 input)

  )
