(ns aoc.2021.day07-test
  (:require [aoc.2021.day07 :as sut]
            [clojure.test :refer :all]))

(def sample-input ["16,1,2,0,4,2,7,1,2,14"])
(def sample-coll [16 1 2 0 4 2 7 1 2 14])


(deftest test-fuel-cost
  (is (= 1 (sut/fuel-cost 3 4 true)))
  (is (= 3 (sut/fuel-cost 3 5 true)))
  (is (= 6 (sut/fuel-cost 6 3 true))))

(deftest test-score
  (is (= 37 (sut/score sample-coll 2 false)))

  (is (= 206 (sut/score sample-coll 2 true)))

  (is (= 183 (sut/score sample-coll 3 true)))

  (is (= 170 (sut/score sample-coll 4 true)))

  (is (= 168 (sut/score sample-coll 5 true))))

(deftest test-optimal-score
  (is (= 37 (sut/optimal-score sample-coll false)))

  (is (= 168 (sut/optimal-score sample-coll true))))

(deftest test-solve
  (is (= [37 168]
         (sut/solve (sut/parse-input sample-input)))))
