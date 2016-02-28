(ns simple-search.genetic-search
  (:require [simple-search.core :as core])
   (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000
        simple-search.knapsack-examples.knapPI_11_200_1000
        simple-search.knapsack-examples.knapPI_13_200_1000
        simple-search.knapsack-examples.knapPI_16_200_1000))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn add-score
  [answer]
  (core/add-score core/penalized-score answer))


(defn wild-type
  [instance]
  "generate lazy seq of random individuals"
  (repeatedly #(add-score (core/random-answer instance))))


(defn best
  "take a population and return answer with highest score"
  [population]
  (apply max-key :score population))


(defn first-generation
  "generate num random answers for instance"
  [instance num]
  (take num (wild-type instance)))


(defn mutate-pop
  "generate children of population using mutation from core.clj"
  [population]
  (map  #(add-score (core/mutate-answer %)) population))


(defn modify-generation
  "create a new generation from a population by mutating it, and taking the best
  n out of old and young combined, where n is population size. Population should be free of duplicates"
  [population mutator]
  (->> population
      (concat (mutator population))
      (sort-by (comp - :score))
      dedupe
      (take (count population))))


(defn mutate-search
  "generate an answer through randomly mutating a population"
  [instance mutator evals]
  (let [start (first-generation instance 100)
        modify #(modify-generation % mutator)
        generatins (iterate modify start)
        final-pop(nth generations evals)]
    (best final-pp)))


 (defn tournament
  [population num-competitors]
  (best (take num-competitors (shuffle population))))


(defn crossover-mutator
  "use tournament selection to generate children"
  [population crossing]
  (let [gen-winner #(tournament population 10)
        gen-child #(crossing (gen-winner) (gen-winner))]
    (repeatedly (count population) gen-child )))


(defn two-point-crossover
  [answer-1 answer-2]
  (let [choices-1 (:choices answer-1)
        choices-2 (:choices answer-2)
        size (count choices-1)
        [first-split last-split] (sort (repeatedly 2 #(rand-int size)))

        new-choices (concat
                      (take first-split choices-1)

                      (->> choices-2
                           (drop first-split)
                           (take (- last-split first-split)))

                      (drop last-split choices-1))]

    (add-score (core/make-answer (:instance answer-1) new-choices))))


(apply two-point-crossover (take 2 (wild-type knapPI_11_20_1000_1)))

(defn uniform-crossover
  "Take two answers and create a third one using uniform crossover"
  [answer-1 answer-2]
  (let [answer-vector (map vector (:choices answer-1) (:choices answer-2))
        new-choices (map rand-nth answer-vector)]
    (add-score (core/make-answer (:instance answer-1) new-choices))))

(let [uniform-crossover (fn [pop] (crossover-mutator pop uniform-crossover))
      two-point-crossover (fn [pop] (crossover-mutator pop two-point-crossover))]
(mutate-search knapPI_16_20_1000_63 two-point-crossover 200))







