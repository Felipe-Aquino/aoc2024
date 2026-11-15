(ns day04
  "Day 4"
  (:require [utils])
)

(def day-num "day04")

(defn- split-chars [value] (clojure.string/split value #""))

(defn- read-grid [content] (mapv #(clojure.string/split % #"") content))
(defn- grid-size [grid]
  {:nrows (count grid)
   :ncols (count (get grid 0))})
(defn- grid-get [grid row col]
  (get (get grid row) col))

(defn- move-by-dir [row col dir]
  (case dir
    :north {:row (dec row) :col col}
    :south {:row (inc row) :col col}
    :east {:row row :col (inc col)}
    :west {:row row :col (dec col)}
    :north-east {:row (dec row) :col (inc col)}
    :north-west {:row (dec row) :col (dec col)}
    :south-east {:row (inc row) :col (inc col)}
    :south-west {:row (inc row) :col (dec col)}
  ))

(defn- is-position-valid? [grid-size row col]
  (and
    (>= row 0)
    (>= col 0)
    (< col (:ncols grid-size))
    (< row (:nrows grid-size))))

(defn- grid-get-word-aux [grid row col nchars dir letters]
  (let [size (grid-size grid)
        new-pos (move-by-dir row col dir)]
    (if (and (> nchars 0) (is-position-valid? size row col))
      (let [new-row (:row new-pos)
            new-col (:col new-pos)
            new-nchars (dec nchars)
            new-letters (conj letters (grid-get grid row col))]
        (recur grid new-row new-col new-nchars dir new-letters))
      (clojure.string/join letters))))

(defn- grid-get-word [grid row col nchars dir]
  (grid-get-word-aux grid row col nchars dir []))

(defn- test-xmas [grid acc [row col dir]]
  (if (= (grid-get-word grid row col 4 dir) "XMAS") (inc acc) acc))

(defn part1 [content]
  (let [grid (read-grid content)
        size (grid-size grid)
        dirs [:north :south :east :west :north-east :north-west :south-east :south-west]
        xmas-count (
          reduce
            (fn [acc state] (test-xmas grid acc state))
            0
            (for [row (range (:nrows size))
                  col (range (:ncols size))
                  dir dirs]
              [row col dir]))
        ]
    (println "XMAS amount =" xmas-count)))

(defn part2 [content]
  (println "Running part2!!"))

(defn -main [& args]
  (let [result (utils/parse-arguments day-num args)]
    (case (:part result)
      1 (part1 (:content result))
      2 (part2 (:content result))
    ))
)
