(ns day03
  "Day 3"
  (:require [utils])
)

(def day-num "day03")

(defn mul-eval [values]
  (let [a (parse-long (get values 1))
        b (parse-long (get values 2))]
    (* a b)))

(defn instruction-total-calc [instr]
  (let [matcher (re-matcher #"mul\((\d+),(\d+)\)" instr)]
    (loop [match (re-find matcher)
           total 0]
      (if (nil? match)
        total
        (recur (re-find matcher) (+ total (mul-eval match)))))))

(defn part1 [content]
  (loop [instructions content
         total 0]
    (if (empty? instructions)
      (println "result =" total)
      (let [instr-total (instruction-total-calc (first instructions))]
        (println (first instructions))
        (println "\\->" instr-total)
        (recur (rest instructions) (+ total instr-total))))))

(defn part2 [content]
  (println "Running part2!!"))

(defn -main [& args]
  (let [result (utils/parse-arguments day-num args)]
    (case (:part result)
      1 (part1 (:content result))
      2 (part2 (:content result))
    ))
)
