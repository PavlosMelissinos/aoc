(ns advent2020
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Day 1

(def input1
  (map #(Integer. %) (-> (io/resource "inputs/day1.txt")
                         slurp
                         (str/split #"\n"))))

(defn day01a [inp target]
  (loop [asc (sort inp)]
    (if (and (>= (count asc) 2)
             (< (+ (first asc) (second asc)) target)) ;; small optimization
      (let [sum (+ (first asc) (last asc))]
        (cond
          (> sum target)    (recur (drop-last 1 asc))
          (< sum target)    (recur (drop 1 asc))
          :else             (* (first asc) (last asc)))))))
;; (day01a input1 2020) => 960075

(defn day01b [inp]
  (->> (filter #(day01a inp (- 2020 %)) inp)
       (apply *)))
;; (day01b input1) => 212900130


;; Day 2

(def input2
  (-> (io/resource "inputs/day2.txt")
      slurp
      (str/split #"\n")))

(defn day02-parse [ln]
  (->> (str/split ln #"(-|:| )")
       (remove empty?)
       (map #(%1 %2) [#(Integer. %) #(Integer. %) identity identity])))
;; (day02parse "1-3 a: abcde") => (1 3 "a" "abcde")
;; (day02parse "3-11 z: zzzzzdzzzzlzz") => (3 11 "z" "zzzzzdzzzzlzz")

(defn day02-is-valid-a? [ln]
  (let [[min max c s] (day02-parse ln)]
    (-> (str/split s #"")
        frequencies
        (get c 0)
        (#(<= min % max)))))
;; (day02-is-valid-a? "1-3 a: abcde") => true
;; (day02-is-valid-a? "3-11 b: zzzzzdzzzzlzz") => false

(defn day02a [inp]
  (->> (filter day02-is-valid-a? inp)
       count))
;; (day02a input2) => 550


(defn xor [& preds]
  (-> (filter true? preds) count odd?))
;; (xor true false false) => true
;; (xor false true true) => false

(defn day02-is-valid-b? [ln]
  (let [[min max c s] (day02-parse ln)]
    (def ln-glb [min max c s])
    (-> (str/split s #"")
        (#(xor (= (nth % (dec min)) c) (= (nth % (dec max)) c))))))
;; (day02-is-valid-b? "1-3 a: abcde") => true
;; (day02-is-valid-b? "3-11 z: zzzzzdzzzzzzz") => false

(defn day02b [inp]
  (count (filter day02-is-valid-b? inp)))
;; (day02b input2) => 634


;; Day 3

(def input3
  (-> (io/resource "inputs/day3.txt")
      slurp
      (clojure.string/split #"\n")))

(defn day03a [inp right down]
  (->> (take-nth down inp)
       (map-indexed
        (fn [idx row]
          (nth row (mod (* idx right) (count row)))))
       (keep #{\#})
       count))
;; (day03a input3 3 1) => 167

(defn day03b [inp & cases]
  (apply * (map #(apply day03a inp %) cases)))
;; (day03b input3 [1 1] [3 1] [5 1] [7 1] [1 2]) => 736527114


;; Day 4


(def input4
  (-> (io/resource "inputs/day4.txt")
      slurp
      (str/split #"\n\n")))

(defn day04-parse [inp]
  (let [parse-field (fn [s]
                      (mapv (fn [f x] (f x)) [keyword identity] (str/split s #":")))
        parse-pass  (fn [s]
                      (->> (str/split s #"\n| ")
                           (map parse-field)
                           (into {})))]
    (map parse-pass inp)))

(defn day04a [inp]
  (->> (day04-parse inp)
       (filter #(every? % [:byr :iyr :eyr :hgt :hcl :ecl :pid]))
       count))
;; (day04a input4) => 200

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn day04b [inp]
  (let [validators {:byr (fn [byr] (and (= (count byr) 4) (<= 1920 (Integer/parseInt byr) 2002)))
                    :iyr (fn [iyr] (and (= (count iyr) 4) (<= 2010 (Integer/parseInt iyr) 2020)))
                    :eyr (fn [eyr] (and (= (count eyr) 4) (<= 2020 (Integer/parseInt eyr) 2030)))
                    :hgt (fn [hgt] (cond
                                     (str/ends-with? hgt "cm") (<= 150 (parse-int hgt) 193)
                                     (str/ends-with? hgt "in") (<= 59 (parse-int hgt) 76)))
                    :hcl (fn [hcl] (re-matches #"#[0-9a-f]{6}" hcl))
                    :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
                    :pid (fn [pid] (re-matches #"[0-9]{9}" pid))}
        valid?     (fn [id]
                     (and
                      (every? id (keys validators))
                      (every? (merge-with #(%1 %2) validators id) (keys validators))))]
    (->> (day04-parse inp)
         (filter valid?)
         count)))
;; (day04b input4) => 116


;; Day 5

(def input5
  (-> (io/resource "inputs/day5.txt")
      slurp
      (str/split #"\n")))

(defn seat [i]
  (let [i (-> i
              (str/replace #"F" "0")
              (str/replace #"B" "1")
              (str/replace #"L" "0")
              (str/replace #"R" "1"))
        row (-> (apply str (take 7 i)) (BigInteger. 2))
        col (-> (apply str (drop 7 i)) (BigInteger. 2))]
    (-> row (* 8) (+ col) int)))
;; (seat "BFFFFFBLRL") => 522

(defn day05a [inp]
  (->> (map seat inp) (apply max)))
;; (day05a input5) => 816


(defn day05b [inp]
  (let [seats (map seat inp)]
    (-> (set/difference (set (range (apply min seats) (inc (apply max seats))))
                        (set seats))
        first)))
;; (day05b input5) => 539

;; Day 6

(def input6
  (->> (io/resource "inputs/day6.txt")
       slurp
       (#(str/split % #"\n\n"))
       (map #(str/split % #"\n"))))

(defn day06a [inp]
  (->> (map (comp count set str/join) inp)
       (apply +)))
;; (day06a input6) => 6612

(defn day06b [inp]
  (->> (map (comp count #(apply str %) #(apply clojure.set/intersection %) #(map set %)) input6)
       (apply +)))
;; (day06b input6) => 3268

;; Day 7

(def input7
  (->> (io/resource "inputs/day7.txt")
       slurp
       (#(str/split % #"\n"))))

(defn day07-parse [sentence]
  (-> (str/replace sentence #",|\d.|\.|bags|bag|contain|no other" "")
      (str/trim)
      (str/trim)
      (str/split #"  +")))

(defn day07a [inp]
  (let [bagushka  (->> (map (comp (fn [[h & t]] (zipmap t (repeat #{h}))) day07-parse) inp)
                       (apply merge-with into))
        visited   (atom #{})
        remaining (atom (get bagushka "shiny gold"))]
    (while (not-empty @remaining)
      (let [nxt (first @remaining)]
        (swap! remaining concat (get bagushka nxt))
        (swap! visited #(set (conj % nxt)))
        (swap! remaining #(set (rest %)))))
    (count @visited)))
;; (day07a input7) => 278

(defn day07-parse2 [sentence]
  (let [sentence       (-> sentence
                           (str/replace #"no other" "0")
                           (str/replace #"\.| bags| bag|" "")
                           (str/split #" contain "))
        inner-parse-fn (fn [s] (-> (str/trim s)
                                   (str/split #" " 2)
                                   (#(zipmap [:cnt :color] %))
                                   (update :cnt #(Long/parseLong %))))
        parse-fn       (fn [s] (->> (str/split s #"\d. |,")
                                    (map inner-parse-fn)))]
    (map (fn [f x] (f x)) [identity parse-fn] sentence)))

(defn day07a2 [inp]
  (let [bagushka  (->> (map (comp (fn [[h t]] (zipmap (map :color t) (repeat #{h}))) day07-parse2) inp)
                       (apply merge-with into))
        visited   (atom #{})
        remaining (atom (get bagushka "shiny gold"))]
    (while (not-empty @remaining)
      (let [nxt (first @remaining)]
        (swap! remaining concat (get bagushka nxt))
        (swap! visited #(set (conj % nxt)))
        (swap! remaining #(set (rest %)))))
    (count @visited)))
;; (day07a2 input7) => 278

(defn day07b [inp]
  (let [bagushka  (->> (map day07-parse2 inp)
                       (apply concat)
                       (apply hash-map))
        remaining (atom (get bagushka "shiny gold"))
        count     (atom 0)]
    (while (not-empty @remaining)
      (let [{:keys [cnt color]} (first @remaining)
            new_rem             (map #(update % :cnt * cnt) (get bagushka color))]
        (swap! remaining rest)
        (swap! remaining concat new_rem)
        (swap! count + cnt)))
    @count))
;; (day07b input7) => 45157


(comment
  (->> "drab black bags contain 5 dull maroon bags, 2 dark silver bags, 5 bright red bags, 5 bright cyan bags."
       day07-parse2
       #_second
       #_(apply hash-map))

  (def inp-sample
    ["shiny gold bags contain 2 dark red bags."
     "dark red bags contain 2 dark orange bags."
     "dark orange bags contain 2 dark yellow bags."
     "dark yellow bags contain 2 dark green bags."
     "dark green bags contain 2 dark blue bags."
     "dark blue bags contain 2 dark violet bags."
     "dark violet bags contain no other bags."])
  (->> inp-sample
       (map day07-parse2)
       (apply concat)
       (apply hash-map))

  ["clear olive" "0"]
  (let [sentence       "clear olive bags contain no other bags."
        sentence       (-> sentence
                           (str/replace #"no other" "0")
                           (str/replace #"\.| bags| bag|" "")
                           (str/split #" contain "))
        inner-parse-fn (fn [s] (-> (str/trim s) (str/split #" " 2) reverse))
        parse-fn       (fn [s] (->> (str/split s #"\d. |,")
                                    (map inner-parse-fn)
                                    (apply concat)
                                    (apply hash-map)))
        ]
    (map (fn [f x] (f x)) [identity parse-fn] sentence)
    #_sentence)

  (("clear gold" {"pale tomato" "4", "light maroon" "5", "clear blue" "5"}))

  (->> input7
       (map day07-parse2)
       (take 5)
       #_(apply merge-with into))

  (select-keys {:a 1 :b 2} #{:a :c})

  (let [a-set #{:a :c}
        vector-of-maps [{:a 1 :b 2} {:b 5 :c 1} {:a 1 :b 2 :c 3 :d 4}]]
    (map #(select-keys % a-set) vector-of-maps))
  [{:a 1} {:c 1} {:c 3, :a 1}]

  (defn mapset [function collection]
    (set (reduce (fn [final-collection item]
                   (println final-collection)
                   (into final-collection [(function item)]))
                 []
                 collection)))

  (mapset :a [{:a 1 :b 2} {:b 5 :c 1} {:a 1 :b 2 :c 3 :d 4}])
  (mapset inc [5 10 15 20])

)
