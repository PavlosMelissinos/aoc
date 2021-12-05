(ns aoc.2021.day04-test
  (:require [aoc.2021.day04 :as sut]
            [clojure.test :refer :all]
            [clojure.string :as str]))


(def sample-input
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")

(deftest test-parse-numbers
  (is (= [14 21 17 24 4 10 16 15 9 19 18 8 23 26 20 22 11 13 6 5 2 0 12 3 7]
         (sut/parse-numbers
          "14 21 17 24 4 10 16 15 9 19 18 8 23 26 20 22 11 13 6 5 2 0 12 3 7")))
  (is (= [3 15 0 2 22 9 18 13 17 5 19 8 7 25 23 20 11 10 24 4 14 21 16 12 6]
         (sut/parse-numbers
          " 3 15 0 2 22 9 18 13 17 5 19 8 7 25 23 20 11 10 24 4 14 21 16 12 6"))))

(deftest test-parse-input
  (is (= [[7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1]
          [22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19]
          [3 15 0 2 22 9 18 13 17 5 19 8 7 25 23 20 11 10 24 4 14 21 16 12 6]
          [14 21 17 24 4 10 16 15 9 19 18 8 23 26 20 22 11 13 6 5 2 0 12 3 7]]
         (sut/parse-input sample-input))))

(deftest test-bingo?
  (let [b [14 21 17 24 4 10 16 15 9 19 18 8 23 26 20 22 11 13 6 5 2 0 12 3 7]]
    (is (false? (sut/bingo? b))))

  (let [b [14 21 17 24 4 nil nil nil nil nil 18 8 23 26 20 22 11 13 6 5 2 0 12 3 7]]
    (is (true? (sut/bingo? b))))

  (let [b [14 21 nil 24 4 10 16 nil 9 19 18 8 nil 26 20 22 11 nil 6 5 2 0 nil 3 7]]
    (is (true? (sut/bingo? b))))

  (let [b [14 21 17 24 nil nil nil nil nil 19 18 8 23 26 20 22 11 13 6 5 2 0 12 3 7]]
    (is (false? (sut/bingo? b)))))

(deftest test-solve
  (is (= [4512 1924]
         (sut/solve (sut/parse-input sample-input)))))
