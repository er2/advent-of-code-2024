(ns advent-of-code-2024.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn readlines []
  (str/split-lines (slurp (io/resource "day6.txt"))))

(defn parse-obstacles [lines]
  (into #{}
        (for [y (range (count lines))
              x (range (.length (first lines)))
              :let [elem (get (get lines y) x)]
              :when (= elem \#)]
          [x y])))

(defn parse-guard [lines]
  (first (for [y (range (count lines))
               x (range (.length (first lines)))
               :let [elem (get (get lines y) x)]
               :when (#{\v \^ \< \>} elem)]
           {:loc [x y]
            :dir elem})))

(defn parse-map [lines]
  {:obstacles (parse-obstacles lines)
   :guard     (parse-guard lines)
   :trod      #{}
   :max-x     (.length (first lines))
   :max-y     (count lines)})

(def forward
  {\^ (fn [[x y]] [x (dec y)])
   \< (fn [[x y]] [(dec x) y])
   \> (fn [[x y]] [(inc x) y])
   \v (fn [[x y]] [x (inc y)])})

(def rotate
  {\^ \>
   \> \v
   \v \<
   \< \^})

(defn in-map [map]
  (fn [[x y]]
    (and
      (< -1 x (:max-x map))
      (< -1 y (:max-y map)))))

(defn one-more
  "takes a function and a collection.
  returns a new collection based on the original
  plus a new element from the function applied to the previous last element"
  [last-fn one]
  (let [new-last-elem (last-fn (last one))]
    (conj one new-last-elem)))

(defn why [last]
  (if (:unobstructed last)
    :off-map
    :obstructed))

(defn explain-ending [moves]
  (let [[last & others] (reverse moves)
        reason (why last)]
    {:reason reason
     :moves  (reverse others)}))

(defn walk-forward [m]
  (let [{guard     :guard
         obstacles :obstacles} m
        {loc :loc
         dir :dir} guard
        forward (forward dir)
        in-map (in-map m)
        unobstructed (fn [coord] (not (obstacles coord)))
        detailed (fn [c] {:in-map       (in-map c)
                          :unobstructed (unobstructed c)
                          :coord        c})]
    (->>
      (iterate forward loc)
      (map detailed)
      (take-while (every-pred :in-map :unobstructed))
      (one-more #(detailed (forward (:coord %))))
      ;(one-more (comp :coord forward detailed))
      (explain-ending)
      )))

(defn patrol [m]
  (let [{reason :reason
         moves  :moves} (walk-forward m)
        new-orientation (rotate (:dir (:guard m)))
        walked-off-edge (= reason :off-map)
        new-trod (conj (:trod map) moves)
        new-pos (last moves)
        new-map (assoc m :trod new-trod :guard {:loc new-pos :dir new-orientation})]
    (if walked-off-edge
      new-map
      (recur new-map))))

(defn part1 []
  (-> (readlines)
      (parse-map)
      (patrol)
      (:trod)
      (count)))

(comment
  (readlines)
  (range 10)
  (part1)
  ;({:foo {:bar 4}} :foo)
  (take 5 (iterate (forward \>) [0 0]))
  (parse-obstacles (readlines))
  (parse-guard (readlines))
  (parse-map (readlines))
  ((in-map (parse-map (readlines))) [0 0])
  (walk-forward (parse-map (readlines)))
  ;(post-pend inc (lazy-seq ))
  (explain-ending [{:in-map false :unobstructed true :coord [1 1]}])
  (explain-ending [{} {} {:in-map true :unobstructed false :coord [1 1]}])
  (take 5 (walk-forward (parse-map (readlines)))))