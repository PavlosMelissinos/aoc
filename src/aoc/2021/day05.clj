(ns aoc.2021.day05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.math :as math]
            [aoc.utils :as utils]))

(defn parse-vent-line [s]
  (let [[x1 y1 x2 y2]
        (-> (str/replace s #"," " ")
            (str/replace #" -> " " ")
            (str/split #" ")
            (as-> $ (map parse-long $)))]
    [x1 y1 x2 y2]))

(defn parse-input [in] (map parse-vent-line in))

(defn auto-range [a b]
  (if (> a b)
    (range a (dec b) -1)
    (range a (inc b)))
;;  (apply range (sort a b))
)

(defn combos [xs ys]
  (for [x xs, y ys] [x y]))

(defn freqs [xys]
  (frequencies (map (fn [[x y]] (+ (* 1000 x) y)) xys)))

(defn cell-range [[x1 y1 x2 y2] with-diagonals]
  (let [xs (auto-range x1 x2)
        ys (auto-range y1 y2)]
    (cond
      (or (= y1 y2) (= x1 x2)) (freqs (combos xs ys))
      with-diagonals           (freqs (map vector xs ys)))))

(defn common-points [in with-diagonals]
  (->> (map #(cell-range % (or with-diagonals false)) in)
       (apply merge-with +)
       (filter #(> (val %) 1))
       count))

(defn solve [input]
  [(common-points input false)
   (common-points input true)])

(comment
  (solve (parse-input (utils/input 5)))
  ;; => 5576 18144

  ,)
