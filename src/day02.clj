(ns day02
  "Day 2"
  (:require [utils])
)

(def day-num "day02")

; is-list-increasing?: (long list) -> bool
(defn- is-list-increasing? [ls]
  (let [fst (first ls)
        snd (second ls)]
    (or (nil? fst) (nil? snd) (> snd fst)))
  )

; is-report-safe?: (long vector) -> long -> bool -> bool
(defn- is-report-safe? [report last-value increasing]
  (let [value (first report)
        tl (rest report)]
  (if (nil? value)
    true
    (let [diff (- value last-value)]
      (if (or (and increasing (>= diff 1) (<= diff 3))
              (and (not increasing) (>= diff -3) (<= diff -1)))
        (recur tl value increasing)
        false
      )))))

(defn- safe-count [reports]
  (let [safe-map-count
         (fn [r]
           (if (is-report-safe? (rest r) (first r) (is-list-increasing? r)) 1 0))]
    (reduce + (map safe-map-count reports))))

(defn- read-report [line]
  (map parse-long (utils/space-split line)))

(defn part1 [content]
  (let [reports (map read-report content)]
    (println (safe-count reports))
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
