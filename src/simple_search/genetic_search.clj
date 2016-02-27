(ns simple-search.genetic-search
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

(defn wild-type
  [instance]
  "generate lazy seq of random individuals"
  (repeat (add-score (core/random-answer instance))))

(defn first-generation
  "generate num random answers for instance"
  [instance num]
  (take num (set (wild-type instance))))

(defn mutate-pop
  [population]
  (map  #(add-score (core/mutate-answer %)) population))

(defn next-generation
  "create a new generation from a population by mutating it, and taking the best
  n out of old and young combined, where n is population size"
  [population]
  (->> population
      (concat (mutate-pop population))
      (sort-by #(-> % :score - ))
      (take (count population))))

(defn next-gen-no-duplicates
  "like next-generation, but remove duplicates in the population. Fill in with wilds if we run out of room
  THOUGHT: we should use a set instead of a list for the population"
  [population]
  (let [instance (:instance (first population))
        fill-in-wilds #(lazy-cat % (wild-type instance))]
    (->> population
        (concat (mutate-pop population))
        (sort-by #(-> % :score - ))
        distinct
        fill-in-wilds
        (take (count population)))))

(defn mutate-search
  "generate an answer through randomly mutating a population"
  [instance evals]
  (let [start (first-generation instance 100)
        generations (iterate next-generation start)
        final-pop (nth generations evals)
        bestest (apply max-key :score final-pop)]
    bestest))

;;(mutate-search knapPI_11_20_1000_4 200)
;; (next-generation (first-generation knapPI_13_20_1000_20 10 ))
