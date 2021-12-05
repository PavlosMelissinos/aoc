(ns aoc.2021.day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-command [s]
  (let [[direction distance] (str/split s #" ")]
    (merge {:up 0, :down 0, :forward 0}
           {(keyword direction) (Long/parseLong distance)})))

(def input02
  (-> (io/resource "inputs/2021/day02.txt")
      slurp
      (str/split #"\n")
      (as-> c
        (map parse-command c))))


(defn solve1 [inp]
  (->> (reduce (fn [[hpos depth] {:keys [forward down up]}]
                 [(+ hpos forward)
                  (+ depth down (- up))])
               [0 0]
               inp)
       (apply *)))
;;=> 1936494

(defn solve2 [inp]
  (->> (reduce (fn [[hpos depth aim] {:keys [forward down up]}]
                 [(+ hpos forward)
                  (+ depth (* aim forward))
                  (+ aim down (- up))])
               [0 0 0]
               inp)
       (take 2)
       (apply *)))
;;=> 1997106066

(comment
  (let [distance 2]
    (- distance))
  (first input02)
  (solve1 input02)
  (solve2 input02)

  (let [input02 "forward 5
down 5
forward 8
up 3
down 8
forward 2
"]
    (-> input02
        (str/split #"\n")
        (as-> c (map #(str/split % #" ") c))
        day02b))
  )
