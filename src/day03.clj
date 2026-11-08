(ns day03
  "Day 3"
  (:require [utils])
)

(def day-num "day03")

(defn- mul-eval [values]
  (let [a (parse-long (get values 1))
        b (parse-long (get values 2))]
    (* a b)))

(defn- instruction-total-calc [instr]
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

(defn- instruction-total-calc-2 [instr is-enabled]
  (let [matcher (re-matcher #"(?:do\(\)|don't\(\)|mul\((\d+),(\d+)\))" instr)]
    (loop [match (re-find matcher)
           total 0
           enabled is-enabled]
      (println match)
      (if (nil? match)
        {:total total :enabled enabled}
        (case (get match 0)
          "do()" (recur (re-find matcher) total true)
          "don't()" (recur (re-find matcher) total false)
          (recur (re-find matcher) (if enabled (+ total (mul-eval match)) total) enabled))))))

(defn part2 [content]
  (loop [instructions content
         total 0
         enabled true]
    (if (empty? instructions)
      (println "result =" total)
      (let [result (instruction-total-calc-2 (first instructions) enabled)]
        (println (first instructions))
        (println "\\->" (:total result) "," (:enabled result))
        (recur (rest instructions) (+ total (:total result)) (:enabled result))))))

(defn -main [& args]
  (let [result (utils/parse-arguments day-num args)]
    (case (:part result)
      1 (part1 (:content result))
      2 (part2 (:content result))
    ))
)
