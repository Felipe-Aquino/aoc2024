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

(defn- ok 
  ([] {:type :ok :value nil})
  ([value] {:type :ok :value value}))
(defn- err
  ([] {:type :err :value nil})
  ([value] {:type :err :value value}))

(defn- list-remove-at-aux [ls at result]
  (if (= at 0)
    (concat result (into [] (rest ls)))
    (recur (rest ls) (dec at) (conj result (first ls)))))

(defn- list-remove-at [ls at] (list-remove-at-aux ls at []))

; is-report-safe?: (long vector) -> long -> bool -> bool
(defn- is-report-safe-with-sigle-bad-level-aux [report last-value idx increasing]
  (let [value (first report)
        tl (rest report)]
    (if (nil? value)
      (ok)
      (let [diff (- value last-value)]
        (if (or (and increasing (>= diff 1) (<= diff 3))
                (and (not increasing) (>= diff -3) (<= diff -1)))
          (recur tl value (inc idx) increasing)
          (err idx)
        )))))

(defn- is-report-safe-with-sigle-bad-level? [report]
  (let [r report]
    (is-report-safe-with-sigle-bad-level-aux (rest r) (first r) 1 (is-list-increasing? r))))

(defn- test-report-levels [r]
  (let [result (is-report-safe-with-sigle-bad-level? r)]
    (case (:type result)
      :ok 1
      :err (loop [test-values (list (:value result) (dec (:value result)) 0)]
        (let [hd (first test-values) tl (rest test-values)]
          (case (:type (is-report-safe-with-sigle-bad-level? (list-remove-at r hd)))
            :ok 1
            :err (if (empty? tl) 0 (recur tl))))))))

(defn- safe-count-with-sigle-bad-level [reports]
  (reduce + (map test-report-levels reports)))

(defn part2 [content]
  (let [reports (map read-report content)]
    (println (safe-count-with-sigle-bad-level reports))
  ))

(defn -main [& args]
  (let [result (utils/parse-arguments day-num args)]
    (case (:part result)
      1 (part1 (:content result))
      2 (part2 (:content result))
    ))
)
