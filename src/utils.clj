(ns utils
  (:require [clojure.string :refer [split]]))

(defn print-help []
  (println "
======================================================================
Advent of Code Helper
======================================================================
Options:
  part=2         Runs part 1 or 2. (Required)
  input=example  Accepts one of 3 values: given or example or <path>. (Required)

Usage examples:
  clojure -M -m day01 part=2 input=given
  clojure -M -m day01 part=1 input=./inputs/test.txt
======================================================================
"))


; space-split: str -> (str vector)
(defn space-split [value] (split value #"\s+"))

(defn read-file [filepath] (split (slurp filepath) #"\n"))

(defn- parse-input [inp day-name]
  (read-file
    (case inp
      "given" (str "inputs/" day-name ".txt")
      "example" (str "inputs/" day-name "-example.txt")
      inp)))

(defn- parse-arg [result arg day-name]
  (let [arg2 (split arg #"=")]
    (if (= (count arg2) 2)
      (case (get arg2 0)
        "part" (assoc result :part (parse-long (get arg2 1)))
        "input" (assoc result :content (parse-input (get arg2 1) day-name))
        result)
      result)))

(defn- is-result-valid? [result]
  (and (:part result) (:content result)))

(defn parse-arguments [day-name args]
  (let 
    [result (reduce #(parse-arg %1 %2 day-name) {} args)]
    (if (is-result-valid? result)
      result
      ((print-help)
       (throw (Exception. "Could not parse argments!"))))))

