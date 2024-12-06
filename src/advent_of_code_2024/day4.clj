(ns advent-of-code-2024.day4
  (:require [clojure.java.io :as io]))

(def lines
  (clojure.string/split-lines (slurp (io/resource "day4.txt"))))

(defn find-xs [ls]
  (for [y (range (count ls))
        x (range (.length (first ls)))
        :let [l (get (get ls y) x)]
        :when (= l \X)]
    [x y]))

(defn to-map [l]
  (let [at (fn [x y] (get (get l y) x))]
    (merge
      (for [y (range (count l))
            x (range (.length (first l)))]
        {[x y] (at x y)}))))

(def directions
  {:E  (fn [[x y]] [(inc x) y])
   :NE (fn [[x y]] [(inc x) (inc y)])
   :N  (fn [[x y]] [x (inc y)])
   :NW (fn [[x y]] [(dec x) (inc y)])
   :W  (fn [[x y]] [(dec x) y])
   :SW (fn [[x y]] [(dec x) (dec y)])
   :S  (fn [[x y]] [x (dec y)])
   :SE (fn [[x y]] [(inc x) (dec y)])})

(defn seq-in-dir [start dir]
  (iterate (directions dir) start))

(defn valid-coord [lines [x y]]
  (and
    (< -1 y (count lines))
    (< -1 x (.length (first lines)))))

(defn trunc-to-grid [lines seq]
  (take-while (partial valid-coord lines) seq))

(defn at [lines [x y]]
  (get (get lines y) x))

(defn seqs [lines start]
  (let [dirs (keys directions)
        seq-in-dir (fn [dir] (take 4 (trunc-to-grid lines (seq-in-dir start dir))))]
    (map seq-in-dir dirs)))

(defn seq-to-string [lines seq]
  (let [at (partial at lines)]
    (apply str (map at seq))))

(defn part1 []
  (count (for [x (find-xs lines)
               seq (seqs lines x)
               :when (= 4 (count seq))
               :let [word (seq-to-string lines seq)]
               :when (= "XMAS" word)]
           1)))

(comment
  lines
  (part1)
  (seq-to-string lines [[0 0] [0 2]])
  (str \M \X)
  (for [x [1 2 3] y [1 2 3]] [x y])
  (range (count lines))
  (seqs lines [0 0])
  (range (.length (first lines)))
  (find-xs lines)
  (valid-coord lines [0 0])
  (trunc-to-grid lines (seq-in-dir [0 0] :N))
  (take 5 (seq-in-dir [0 0] :E))
  (range (.length "asfsa"))
  (get (get lines 0) 0)
  (find-xs ["X" "Y"])
  (to-map lines))


(defn find-as [lines]
  (for [y (range (count lines))
        x (range (.length (first lines)))
        :let [l (get (get lines y) x)]
        :when (= l \A)]
    [x y]))

(defn cross-coords [[x y]]
  [[(inc x) (inc y)]
   [(inc x) (dec y)]
   [(dec x) (dec y)]
   [(dec x) (inc y)]])

(defn get-cross [lines [x y]]

  )

(defn part2 []
  )

(comment
  (part2)
  )