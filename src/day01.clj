(ns day01
  "Day 1"
  (:require [utils])
  (:require [clojure.string :refer [split]])
)

(def day-num "day01")

; space-split: str -> (str vector)
(defn- space-split [value] (split value #"\s+"))

; collect: [(long list) (long list)] -> [str str]    -> [(long list) (long list)]
; collect: ((long list) vector)      -> (str vector) -> (long list) vector
(defn- collect [result line]
  (let [values (space-split line)]
    [(conj (get result 0) (parse-long (get values 0)))
     (conj (get result 1) (parse-long (get values 1)))
    ]))

(defn part1 [content]
  (let [result (reduce collect [`() `()] content)] 
    (let [lhs (sort (get result 0))
          rhs (sort (get result 1))]
      (println lhs)
      (println rhs)
      (println "result =" (reduce + (map #(abs (- %1 %2)) lhs rhs)))
    )
  ))

(defn part2 [content]
  (println "Running part2!!"))

(defn -main [& args]
  (let [result (utils/parse-arguments day-num args)]
    (case (:part result)
      1 (part1 (:content result))
      2 (part2 (:content result))
    ))
)
