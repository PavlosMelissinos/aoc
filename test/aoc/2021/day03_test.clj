(ns aoc.2021.day03-test
  (:require [aoc.2021.day03 :as sut]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]))

#_(remove-ns 'aoc.2021.day03-test)
(def report
  [[0 0 1 0 0] [1 1 1 1 0] [1 0 1 1 0] [1 0 1 1 1] [1 0 1 0 1] [0 1 1 1 1] [0 0 1 1 1] [1 1 1 0 0] [1 0 0 0 0] [1 1 0 0 1] [0 0 0 1 0] [0 1 0 1 0]])

(deftest test-truthy?
  (is (true? (sut/truthy? "1")))
  (is (false? (sut/truthy? \0))))

(deftest test-parse-number
  (is (= [0 0 1 0 0] (sut/parse-number "00100")))
  (is (= [0 0 1 0 0 1 0 1 0 0 0 1] (sut/parse-number "001001010001")))
  (is (= [] (sut/parse-number ""))))

(deftest test-ones
  (is (= [7 5 8 7 5] (sut/ones report))))

(deftest test-bit-string-invert
  (let [s "10010"]
    (is (= "01101" (sut/bit-string-invert s)))
    (is (= s (sut/bit-string-invert (sut/bit-string-invert s))))))

(deftest test-bit-vector-invert
  (let [s [1 0 0 1 0]]
    (is (= [0 1 1 0 1] (sut/bit-vector-invert s)))
    (is (= s (sut/bit-vector-invert (sut/bit-vector-invert s))))))

(deftest test-gamma-rate
  (is (= (Long/toString 22 2) (sut/gamma-rate report))))

(deftest test-epsilon-rate
  (is (= "01001" ;; 9
         (sut/epsilon-rate report))))

(deftest test-most-common
  (is (= 1 (sut/most-common report 0)))
  (is (= 0 (sut/most-common report 1)))
  (is (= 1 (sut/most-common report 2)))
  (is (= 1 (sut/most-common report 3)))
  (is (= 0 (sut/most-common report 4)))

  (is (= 1 (sut/most-common ["01111" "01010"] 2))))

(deftest test-og-rating-candidates
  (let [report1 (map sut/parse-number ["11110" "10110" "10111" "10101" "11100" "10000" "11001"])
        report2 (map sut/parse-number ["10110" "10111" "10101" "10000"])
        report3 (map sut/parse-number ["10110" "10111" "10101"])
        report4 (map sut/parse-number ["10110" "10111"])
        report5 (map sut/parse-number ["10111"])]
    (is (= report1 (sut/og-rating-candidates report 0)))
    (is (= report2 (sut/og-rating-candidates report1 1)))
    (is (= report3 (sut/og-rating-candidates report2 2)))
    (is (= report4 (sut/og-rating-candidates report3 3)))
    (is (= report5 (sut/og-rating-candidates report4 4)))))

(deftest test-co2-rating-candidates
  (let [report1 [[0 0 1 0 0] [0 1 1 1 1] [0 0 1 1 1] [0 0 0 1 0] [0 1 0 1 0]]
        report2 [[0 1 1 1 1] [0 1 0 1 0]]
        report3 [[0 1 0 1 0]]]
    (is (= report1 (sut/co2-rating-candidates report 0)))
    (is (= report2 (sut/co2-rating-candidates report1 1)))
    (is (= report3 (sut/co2-rating-candidates report2 2))))

  #_(let [report ["01010"]]
    (is (= []
           (sut/co2-rating-candidates report 3)))))

(deftest test-og-rating
  (is (= "10111"
         (sut/og-rating report))))

(deftest test-co2-rating
  (is (= "01010"
         (sut/co2-rating report))))

(deftest solve
  (is (= [198 230]
         (sut/solve report))))
