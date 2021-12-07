(ns aoc.2021.day07
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [aoc.utils :as utils]))

(defn parse-input [[in _]]
  (map parse-long (str/split in #",")))

(defn sum-of-range [n] (* 1/2 n (inc n)))

(defn distance [a b]
  (math/abs (- a b)))

(defn score [positions new-position increasing-fuel-burn?]
  (->> (map (partial distance new-position) positions)
       (map #(if increasing-fuel-burn? (sum-of-range %) %))
       (apply +)))

(defn optimal-score [positions increasing-fuel-burn?]
  (let [positions (vec (sort positions))]
    (->> (range (first positions) (last positions))
         (map #(vector % (score positions % increasing-fuel-burn?)))
         (apply min-key second)
         second)))

(defn solve [positions]
  [(optimal-score positions false)
   (optimal-score positions true)])

(comment
  (solve (parse-input (utils/input 7)))
  ;; => [337833 96678050]
  ,)
