(ns aoc.2021.day08
  (:require [clojure.string :as str]
            [aoc.utils :as utils]
            [clojure.set :as set]))

(defn parse-line [s]
  (->> (str/split s #" \| ")
       (map #(str/split % #"\s+"))
       (zipmap [:input :output])))

(def parse-input (partial map parse-line))

(defmulti look-up (fn [sym d] sym))

(defmethod look-up 0 [_ digits]
  (let [seven (set (look-up 7 digits))
        nine  (look-up 9 digits)]
    (->> (filter #(set/superset? (set %) seven) digits)
         (filter #(= 6 (count %)))
         (remove #(= % nine))
         first)))

(defmethod look-up 1 [_ digits] (first (filter #(= 2 (count %)) digits)))

(defmethod look-up 2 [_ digits]
  (let [five (look-up 5 digits)
        three (look-up 3 digits)]
    (->> (filter #(= 5 (count %)) digits)
         (remove #(or (= % five) (= % three)))
         first)))

(defmethod look-up 3 [_ digits]
  (let [seven (set (look-up 7 digits))]
    (->> (filter #(set/superset? (set %) seven) digits)
         (filter #(= 5 (count %)))
         first)))

(defmethod look-up 4 [_ digits] (first (filter #(= 4 (count %)) digits)))

(defmethod look-up 5 [_ digits]
  (let [seven (set (look-up 7 digits))
        nine  (set (look-up 9 digits))]
    (->> (filter #(= 5 (count %)) digits)
         (filter #(= (set/union seven (set %)) nine))
         first)))

(defmethod look-up 6 [_ digits]
  (let [zero (look-up 0 digits)
        nine (look-up 9 digits)]
    (->> (filter #(= 6 (count %)) digits)
         (remove #(or (= % zero) (= % nine)))
         first)))

(defmethod look-up 7 [_ digits] (first (filter #(= 3 (count %)) digits)))

(defmethod look-up 8 [_ digits] (first (filter #(= 7 (count %)) digits)))

(defmethod look-up 9 [_ digits]
  (let [four-and-seven (set/union (set (look-up 4 digits))
                                  (set (look-up 7 digits)))]
    (->> (filter #(set/superset? (set %) four-and-seven) digits)
         (filter #(= 6 (count %)))
         first)))

(defn decoder [digits]
  (zipmap (map #(look-up % digits) (range 10)) (range 10)))

(defn sort-digits [digit-coll]
  (->> (map set digit-coll) (map str/join)))

(defn output-number [{:keys [input output]}]
  (let [digits (distinct (sort-digits (concat input output)))]
    (->> (map #(get (decoder digits) %) (sort-digits output))
         (apply str)
         parse-long)))

(def simple-digit-counts #{2 4 3 7})

(defn simple-number-count [inp]
  (->> (mapcat :output inp)
       (map count)
       (filter simple-digit-counts)
       count))

(defn output-sum [inp] (apply + (map output-number inp)))

(defn solve [inp]
  [(simple-number-count inp)
   (output-sum inp)])

(comment
  (solve (parse-input (utils/input 8)))
  ;; => [397 1027422]
  )
