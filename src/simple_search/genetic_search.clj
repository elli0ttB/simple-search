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



(defn lambda-select
  "take a population and have each child have lambda children, then take the best, including the original population"
  [lambda mutator]
  (fn [population]
    (->> #(mutator population)
      (repeatedly lambda)
      (apply concat population)
      (sort-by (comp - :score))
      dedupe
      (take (count population)))))

(defn dump-select
  "create a new generation from a population by mutating it, and taking the best
  n out of old and young combined, where n is population size. Population should be free of duplicates"
  [mutator]
  (lambda-select 1 mutator))



(defn simple-mutate-search
  "generate an answer through mutating a populatin with mutate, and taking the best of parents and children"
  [instance next-genration evals pop-size]
  (let [start (first-generation instance pop-size)
        generations (iterate next-genration start)
        final-pop (nth generations evals)]
    (best final-pop)))



(defn bits-to-ans
  "take a function that operates on bits returns an equivalent function that operates on answers"
  [bit-mutator]
  (fn [& answers]
    (let [choices (map :choices answers)
          instance (:instance (first answers)) ]
    (make-answer instance (apply bit-mutator choices)))))


(defn tournament
  [population num-competitors]
  (best (take num-competitors (shuffle population))))

(defn crossover-tournaments
  "given a crossing, return a func that uses tournaments to run that crossing on the population"
  [crossing]
    (fn [population]
    (let [gen-winner #(tournament population 2)
          gen-child #( (bits-to-ans crossing) (gen-winner) (gen-winner) )]
      (repeatedly (count population) gen-child ))))



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

(defn mutate-at-rate
  "mutate choices with probability p"
  ([choices]
   (mutate-at-rate 0.1 choices))
  ([p choices]
      (if (< (rand) p)
        (core/mutate-choices choices)
        choices)))

(defn mutate-pop
  "generate three-armed children of population using mutation from core.clj"
  [population]
  (map  #(add-score (core/mutate-answer %)) population))



;;; testing stuff

(apply (bits-to-ans uniform-crossover) (take 2 (wild-type knapPI_11_20_1000_1)))

(let [method (->> two-point-crossover
                  (comp mutate-at-rate)
                  crossover-tournaments
                  (lambda-select 5)   )]
  (simple-mutate-search knapPI_16_20_1000_63 method 20 30))
