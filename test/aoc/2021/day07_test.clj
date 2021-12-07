(ns aoc.2021.day07-test
  (:require [aoc.2021.day07 :as sut]
            [clojure.test :refer :all]))

(def sample-input ["16,1,2,0,4,2,7,1,2,14"])


(deftest fuel-cost
  (is (= 1 (sut/fuel-cost 3 4 true)))
  (is (= 3 (sut/fuel-cost 3 5 true)))
  (is (= 6 (sut/fuel-cost 6 3 true))))

(deftest optimal-score
  (is (= 37 (sut/optimal-score [16 1 2 0 4 2 7 1 2 14] false)))

  (is (= 168 (sut/optimal-score [16 1 2 0 4 2 7 1 2 14] true))))

(deftest test-solve
  (is (= [37 168]
         (sut/solve (sut/parse-input sample-input)))))
