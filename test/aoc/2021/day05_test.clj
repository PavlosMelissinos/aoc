(ns aoc.2021.day05-test
  (:require [aoc.2021.day05 :as sut]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(def sample-input
  ["0,9 -> 5,9"
   "8,0 -> 0,8"
   "9,4 -> 3,4"
   "2,2 -> 2,1"
   "7,0 -> 7,4"
   "6,4 -> 2,0"
   "0,9 -> 2,9"
   "3,4 -> 1,4"
   "0,0 -> 8,8"
   "5,5 -> 8,2"])

(deftest test-combos
  (is (= [[3 3] [1 3]]
         (sut/combos [3 1] [3])))

  (is (= [[0 0] [0 3] [3 0] [3 3]]
         (sut/combos [0 3] [0 3])))

  (is (= [[0 0] [0 3] [1 0] [1 3] [2 0] [2 3]]
         (sut/combos [0 1 2] [0 3]))))

(deftest auto-range
  (is (= [1 2 3] (sut/auto-range 1 3)))
  (is (= [3 2 1] (sut/auto-range 3 1))))

(deftest test-parse-bound-coords
  (is (= [0 9 5 9]
         (sut/parse-bound-coords "0,9 -> 5,9")))

  (is (= [0 0 8 8]
         (sut/parse-bound-coords "0,0 -> 8,8")))

  (is (= [3 4 1 4]
         (sut/parse-bound-coords "3,4 -> 1,4"))))

(deftest test-parse-input
  (is (= [[0 9 5 9] [8 0 0 8] [9 4 3 4] [2 2 2 1] [7 0 7 4]
          [6 4 2 0] [0 9 2 9] [3 4 1 4] [0 0 8 8] [5 5 8 2]]
         (sut/parse-input sample-input))))

(deftest test-solve
  (is (= [5 12]
         (sut/solve (sut/parse-input sample-input)))))
