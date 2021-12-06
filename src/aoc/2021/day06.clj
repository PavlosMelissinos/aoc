(ns aoc.2021.day06
  (:require [clojure.string :as str]
            [aoc.utils :as utils]))

(defn parse-input [[in _]]
  (let [freqs (->> (str/split in #",") (map parse-long) frequencies)]
    (mapv #(get freqs % 0) (range 9))))

(defn next-day [[h & t]]
  (assoc (update (vec t) 6 +' h) 8 h))

(defn solve [state days]
  (apply +' (nth (iterate next-day state) days)))

(comment
  (solve (parse-input (utils/input 6)) 80)
  ;; => 351092

  (solve (parse-input (utils/input 6)) 256)
  ;; => 1595330616005
  ,)
