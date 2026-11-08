(ns day01
  "Day 1"
  (:require [utils])
  (:require [clojure.string :refer [split]])
)

(def day-num "day01")

; collect: [(long list) (long list)] -> [str str]    -> [(long list) (long list)]
; collect: ((long list) vector)      -> (str vector) -> (long list) vector
(defn- collect [result line]
  (let [values (utils/space-split line)]
    [(conj (get result 0) (parse-long (get values 0)))
     (conj (get result 1) (parse-long (get values 1)))
    ]))

(defn- accumulate [ls result]
  (if (empty? ls)
    result
    (let [head (first ls)
          tail (rest ls)]
      (let [total (inc (get result head 0))]
       (recur tail (assoc result head total))))))

(defn- lookup [result value summary] (+ result (* value (get summary value 0))))

(defn part1 [content]
  (let [result (reduce collect [`() `()] content)] 
    (let [lhs (sort (get result 0))
          rhs (sort (get result 1))]
      (println lhs)
      (println rhs)
      (println "result =" (reduce + (map #(abs (- %1 %2)) lhs rhs)))
    )))

(defn part2 [content]
  (let [result (reduce collect [`() `()] content)] 
    (let [lhs (sort (get result 0))
          rhs (sort (get result 1))]
      (let [summary (accumulate rhs {})]
        (println "result =" (reduce #(lookup %1 %2 summary) 0 lhs))
      )
    )))

(defn -main [& args]
  (let [result (utils/parse-arguments day-num args)]
    (case (:part result)
      1 (part1 (:content result))
      2 (part2 (:content result))
    ))
)
