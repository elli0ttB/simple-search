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

(defn make-answer
  "wrapper around core/make-answer"
  [instance choices]
  (add-score (core/make-answer instance choices)))

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
  [instance mutator evals pop-size]
  (let [start (first-generation instance pop-size)
        modify #(modify-generation % mutator)
        generations (iterate modify start)
        final-pop (nth generations evals)]
    (best final-pop)))

(defn tournament
  [population num-competitors]
  (best (take num-competitors (shuffle population))))

(defn bits-to-ans
  "take a function that operates on bits returns an equivalent function that operates on answers"
  [bit-mutator]
  (fn [& answers]
    (let [choices (map :choices answers)
          instance (:instance (first answers)) ]
    (make-answer instance (apply bit-mutator choices)))))

(defn crossover-mutator
  "use tournament selection to generate children with a given algorithm, given a genetic crossing"
  [population crossing]
  (let [gen-winner #(tournament population 10)
        gen-child #( (bits-to-ans crossing) (gen-winner) (gen-winner) )]
    (repeatedly (count population) gen-child )))

(defn get-randoms
  "return a sorted list of random numbers between 0 and max"
  [length maximum]
  (sort (repeatedly length #(rand-int maximum))))

(defn two-point-crossover
  "take two bit lists and return a new answer with two cuts"
  [choices-1 choices-2]
  (let [[first-split last-split] (get-randoms 2 (count choices-1))]
      (concat
          (take first-split choices-1)
          (->> choices-2
                (drop first-split)
                (take (- last-split first-split)))
          (drop last-split choices-1))))

(defn uniform-crossover [choices-1 choices-2]
  (map (comp rand-nth vector) choices-1 choices-2))

(defn mutate-pop
  "generate three-armed children of population using mutation from core.clj"
  [population]
  (map  #(add-score (core/mutate-answer %)) population))

;;; testing stuff woohoo

(apply (bits-to-ans uniform-crossover) (take 2 (wild-type knapPI_11_20_1000_1)))

(let [uniform-crossover (fn [pop] (crossover-mutator pop uniform-crossover))
      two-point-crossover (fn [pop] (crossover-mutator pop two-point-crossover))]
(mutate-search knapPI_16_20_1000_63 two-point-crossover 100 30))







