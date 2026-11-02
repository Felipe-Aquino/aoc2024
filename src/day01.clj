(ns day01
  "Day 1"
  (:require [utils])
)

(def day-num "day01")

(defn part1 [content]
  (println "Running part1!!"))

(defn part2 [content]
  (println "Running part2!!"))

(defn -main [& args]
  (let [result (utils/parse-arguments day-num args)]
    (case (:part result)
      1 (part1 (:content result))
      2 (part2 (:content result))
    ))
)
