(ns simple-search.genetic-exp
  (:require [simple-search.experiment :as exp])
  (:use simple-search.genetic-search)
  (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000
        simple-search.knapsack-examples.knapPI_11_200_1000
        simple-search.knapsack-examples.knapPI_13_200_1000
        simple-search.knapsack-examples.knapPI_16_200_1000))

(def tests
  (list
    (with-meta
      (let [method (->> two-point-crossover
                        (comp mutate-at-rate)
                        crossover-tournaments
                        (lambda-select 3))]
        (searcher method 20))
      {:label "two-point-cross with mutation 20+3"})

    (with-meta
      (let [method (->> two-point-crossover
                        crossover-tournaments
                        (lambda-select 3))]
        (searcher method 20))
      {:label "two-point crossover, no mutation 20+3"})

    (with-meta
      (let [method (->> uniform-crossover
                        crossover-tournaments
                        (lambda-select 3))]
        (searcher method 20))
      {:label "uniform-crossover, no mutation 20+3"})

    (with-meta
      (let [method (->> uniform-crossover
                        crossover-tournaments
                        (dump-select))]
        (searcher method 50))
      {:label "uniform-crossover, no mutation 50+1"})

    (with-meta
      (let [method (->> mutate-pop
                        (lambda-select 3))]
        (searcher method 20))
      {:label "random-mutation, 20+3"})

    (with-meta
      (let [method (->> mutate-pop
                        (lambda-select 3))]
        (searcher first-generation-skinny method 20))
      {:label "random-mutation, 20+3 with intial pop below capacity"})))


(defn now [] (new java.util.Date))

(defn research [reps evals tests]
  "do an experiment with a nice file name"
  (spit (format "data/genetic/_%d_reps_%s" reps evals (now))
    (apply exp/do-main reps evals tests)))

(research 3 3 tests)

(def possibly-best-way
  "run with 20 reps, 70 tests"
    (with-meta
      (let [method (->> two-point-crossover
                        (comp (partial mutate-at-rate 0.2) )
                        crossover-tournaments
                        )]
        (searcher method 50))
      {:label "uniform-crossover, no mutation 50+1"}))




