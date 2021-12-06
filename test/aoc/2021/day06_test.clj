(ns aoc.2021.day06-test
  (:require [aoc.2021.day06 :as sut]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(def sample-input ["3,4,3,1,2"])

(deftest test-parse-input
  (is (= [0 1 1 2 1 0 0 0 0]
         (sut/parse-input sample-input)))

  (is (= [0 1 1 3 2 2 1 0 0]
         (sut/parse-input ["3,4,3,1,2,3,4,5,5,6"])))

  (is (= [0 1 1 4 3 5 3 2 1]
         (sut/parse-input ["3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8"]))))

(deftest test-next-day
  (is (= [1 1 4 3 5 3 2 1 0]
         (sut/next-day [0 1 1 4 3 5 3 2 1])))

  (is (= [1 4 3 5 3 2 2 0 1]
         (sut/next-day [1 1 4 3 5 3 2 1 0])))
)

(deftest test-weekly-growth
  (is (= nil
         (sut/weekly-growth [3 4 3 1 2]))))

(deftest test-solve
  (is (= 5934
         (sut/solve (sut/parse-input sample-input) 80)))

  (is (= 26984457539
         (sut/solve (sut/parse-input sample-input) 256))))
