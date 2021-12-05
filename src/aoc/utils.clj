(ns aoc.utils
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(defn input [day]
  (-> (format "inputs/2021/day%02d.txt" day)
      io/resource
      slurp
      (str/split #"\n")))
