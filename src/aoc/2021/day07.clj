(ns aoc.2021.day07
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [aoc.utils :as utils]))

(defn parse-input [[in _]]
  (map parse-long (str/split in #",")))

(defn sum-of-range [n] (* 1/2 n (inc n)))

(defn distance [^long a ^long b]
  (Math/abs (- a b)))

(defn median [coll]
  (nth (sort coll) (Math/floor (/ (count coll) 2))))

(defn score [positions new-position increasing-fuel-burn?]
  (->> (map (partial distance new-position) positions)
       (map #(if increasing-fuel-burn? (sum-of-range %) %))
       (apply +)))

(defn optimal-score [positions increasing-fuel-burn?]
  (let [positions (vec (sort positions))]
    (if-not increasing-fuel-burn?
      (score positions (median positions) increasing-fuel-burn?)
      (->> (range (first positions) (last positions))
           (map #(vector % (score positions % increasing-fuel-burn?)))
           (apply min-key second)
           second))))

(defn solve [positions]
  [(optimal-score positions false)
   (optimal-score positions true)])

(comment
  (solve (parse-input (utils/input 7)))
  ;; => [337833 96678050]
  ,)
