(ns aoc.2021.day08-test
  (:require [aoc.2021.day08 :as sut]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(def sample-input
  ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
   "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
   "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
   "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
   "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
   "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
   "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
   "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
   "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
   "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"])

(deftest test-parse-line
  (is (= {:input ["be" "cfbegad" "cbdgef" "fgaecd" "cgeb" "fdcge" "agebfd" "fecdb" "fabcd" "edb"], :output ["fdgacbe" "cefdb" "cefbgd" "gcbe"]}
         (sut/parse-line "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"))))

(deftest test-decoder
  (is (= {"abcdeg"  0
          "ab"      1
          "acdfg"   2
          "abcdf"   3
          "abef"    4
          "bcdef"   5
          "bcdefg"  6
          "abd"     7
          "abcdefg" 8
          "abcdef"  9}
         (sut/decoder
          #{"ab" "abd" "abef" "acdfg" "bcdef" "abcdf" "abcdef" "bcdefg" "abcdeg" "abcdefg"}))))

(deftest test-output-number
  (is (= 5353
         (sut/output-number
          {:input ["acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"]
           :output ["cdfeb" "fcadb" "cdfeb" "cdbaf"]})))
  (is (= 8394
         (sut/output-number {:input ["be" "cfbegad" "cbdgef" "fgaecd" "cgeb" "fdcge" "agebfd" "fecdb" "fabcd" "edb"],
                             :output ["fdgacbe" "cefdb" "cefbgd" "gcbe"]}))))

(deftest test-simple-number-count
  (is (= 26
         (sut/simple-number-count (sut/parse-input sample-input)))))

(deftest test-output-sum
  (is (= 61229
         (sut/output-sum (sut/parse-input sample-input)))))

(deftest test-solve
  (is (= [26 61229]
         (sut/solve (sut/parse-input sample-input)))))
