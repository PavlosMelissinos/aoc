(ns aoc.2021.day04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn input [_] (-> "inputs/2021/day04.txt" io/resource slurp))

(defn parse-numbers [s] (map parse-long (str/split (str/trim s) #" ")))

(defn parse-input [in]
  (-> in
      (str/replace #"\n\n" "#####")
      (str/replace #"[\n\s,]+" " ")
      (str/split #"#####")
      (as-> ps (map parse-numbers ps))))

(defn bingo? [b]
  (let [rows (partition 5 b)
        cols (apply map vector rows)]
    (or (some #(every? nil? %) rows)
        (some #(every? nil? %) cols))))

(defn score [last-draw board]
  (->> (filter identity board)
       (apply +)
       (* last-draw)))

(defn play-round [{:keys [boards draws]}]
  (let [update-board  (fn [b] (map #(if (= % (first draws)) nil %) b))
        active-boards (->> (remove bingo? boards)
                           (map #(fill-board % (first draws))))]
    {:draws  (rest draws)
     :boards active-boards
     :scores (map #(score (first draws) %) (filter bingo? active-boards))}))

(defn solve [[draws & boards]]
  (let [scores (->> {:draws draws, :boards boards :scores []}
                    (iterate play-round)
                    (take-while #(seq (:boards %)))
                    (mapcat :scores))]
    ((juxt first last) scores)))

(comment
  (solve (parse-input (input 4)))
  ;; => [54275 13158]
  ,)
