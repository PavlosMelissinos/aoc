(ns aoc.2021.day03
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [aoc.utils :as ut]))

(def truthy? (complement #{0 "0" \0 false nil}))

(defn parse-number [n]
  (map #(if (truthy? %) 1 0) n))

(defn input [n]
  (map parse-number (ut/input n)))

(defn bit-string-invert [s]
  (-> s
      (str/replace #"0" "2")
      (str/replace #"1" "0")
      (str/replace #"2" "1")))

(defn bit-vector-invert [coll]
  (map {0 1, 1 0} coll))

(defn bitwise-invert [n]
  (let [power-of-two (Long/highestOneBit n)]
    (if power-of-two n)))

(defn ones [report]
  (apply map + report))

(defn gamma-rate [report]
  (let [critical-point (/ (count report) 2)]
    (->> (ones report)
         (map #(if (> % critical-point) 1 0))
         str/join)))

(defn epsilon-rate [report]
  (bit-string-invert (gamma-rate report)))

(defn most-common [report pos]
  (let [freqs (->> (mapv #(str (nth % pos)) report)
                   frequencies)
        more-zeros? (pos? (- (get freqs "0" 0)
                             (get freqs "1" 0)))]
    (if more-zeros? 0 1)))

(defn rating-filter-fn [report pos]
  (let [mode (most-common report pos)]
    (fn [coll] (= mode (nth coll pos)))))

(defn og-rating-candidates [report pos]
  (filter (rating-filter-fn report pos) report))

(defn co2-rating-candidates [report pos]
  (remove (rating-filter-fn report pos) report))

(defn rating
  "oxygen generator rating"
  [report rating-fn]
  (->> [report 0]
       (iterate (fn [[r pos]]
                  (vector (rating-fn r pos) (inc pos))))
       (drop-while #(> (count (first %)) 1))
       ffirst
       first))

(defn og-rating
  "oxygen generator rating"
  [report]
  (str/join (rating report og-rating-candidates)))

(defn co2-rating [report]
  (str/join (rating report co2-rating-candidates)))

(defn power-consumption [report]
  (* (Integer/parseInt (gamma-rate report) 2)
     (Integer/parseInt (epsilon-rate report) 2)))

(defn life-support-rating [report]
  (* (Integer/parseInt (og-rating report) 2)
     (Integer/parseInt (co2-rating report) 2)))

(defn diagnosis [report]
  {:gamma-rate (Integer/parseInt (gamma-rate report) 2)
   :epsilon-rate (Integer/parseInt (epsilon-rate report) 2)
 ;;  :og-rating (Integer/parseInt (og-rating report) 2)
 ;;  :co2-rating (Integer/parseInt (co2-rating report) 2)
   }
)

(defn solve [report]
  [(power-consumption report)
   (life-support-rating report)])

(comment
  (solve (input 3))
  ;; => [3912944 4996233]

  ,)
