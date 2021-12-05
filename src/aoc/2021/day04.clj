(ns aoc.2021.day04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn input [_]
  (-> "inputs/2021/day04.txt"
      io/resource
      slurp))

(defn parse-numbers [s]
  (map #(Long/parseLong %) (str/split (str/trim s) #" ")))

(defn parse-input [in]
  (-> in
      (str/replace #"\n\n" "#####")
      (str/replace #"\n+" " ")
      (str/replace #"[\s,]+" " ")
      (str/split #"#####")
      (as-> ps (map parse-numbers ps))))

(defn wins? [b]
  (let [rows (partition 5 b)
        cols (apply map vector rows)]
    (boolean
     (or (some #(every? nil? %) rows)
         (some #(every? nil? %) cols)))))

(defn bingo? [boards]
  (some wins? boards))

(defn all-bingo? [boards]
  (every? wins? boards))

(defn score [last-draw winning-board]
  (->> (filter identity winning-board)
       (apply +)
       (* last-draw)))

(defn fill-board [b num]
  (map #(if (= % num) nil %) b))

(defn play-round [{:keys [boards winning-boards scores]
                   [draw & future-draws] :draws}]
  (let [new-boards (map #(fill-board % draw) boards)
        new-wins   (->> (map wins? new-boards)
                        (keep-indexed (fn [idx v] (when v idx)))
                        (remove (set winning-boards)))]
    {:draws          future-draws
     :boards         new-boards
     :winning-boards (concat winning-boards new-wins)
     :scores         (map
                      (fn [b s]
                        (if (or s (not (wins? b)))
                          s
                          (score draw b)))
                      new-boards scores)}))

(defn solve [[draws & boards]]
  (let [states
        (iterate play-round {:draws draws, :boards boards, :winning-boards []
                             :scores (repeat (count boards) nil)})

        {:keys [winning-boards boards scores] :as win-state}
        (first (filter (fn [{:keys [boards]}] (all-bingo? boards)) states))]
    (def win-state win-state)
    [(nth scores (first winning-boards))
     (nth scores (last winning-boards))]))

(comment
  (solve (parse-input (input 4)))

  ;; => [54275 5304]

  (take 2 (input 4))

  (let [{:keys [last-draw winning-boards boards]} win-state]
    [(score last-draw (nth boards (first winning-boards)))
     (score last-draw (nth boards (last winning-boards)))]
    (nth boards (first winning-boards)))
  ,)
