(ns genetic-search
  (:require [simple-search.core :as core])
   (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000
        simple-search.knapsack-examples.knapPI_11_200_1000
        simple-search.knapsack-examples.knapPI_13_200_1000
        simple-search.knapsack-examples.knapPI_16_200_1000))

(defn add-score
  [answer]
  (core/add-score core/penalized-score answer))

(defn first-generation
  "generate num random answers for instance"
  [instance num]
  (for [i (range num) ] (add-score (core/random-answer instance))))

(defn mutate-pop
  [population]
  (map  #(add-score (core/mutate-answer %))  population))

(defn next-generation
  [population]
  (->> population
      (concat (mutate-pop population))
      (sort-by #(-> % :score - ))
      (take (count population))))

(defn mutate-search
  "generate an answer through randomly mutating a population"
  [instance evals]
  (let [start (first-generation instance 100)
        generations (iterate next-generation start)
        final-pop (nth generations evals)
        bestest (apply max-key :score final-pop)]
    bestest))

(mutate-search knapPI_11_20_1000_4 200)

;; (next-generation (first-generation knapPI_13_20_1000_20 10 ))
