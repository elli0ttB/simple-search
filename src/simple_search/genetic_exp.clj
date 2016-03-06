(ns simple-search.genetic-exp
  (:require [simple-search.experiment :as exp])

  (:use simple-search.genetic-search)
  (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000
        simple-search.knapsack-examples.knapPI_11_200_1000
        simple-search.knapsack-examples.knapPI_13_200_1000
        simple-search.knapsack-examples.knapPI_16_200_1000
        simple-search.knapsack-examples.knapPI_16_1000_1000))

(def num-evals 100000)

(def tests
  (list
    (with-meta
      (let [method (->> two-point-crossover
                        (comp mutate-at-rate)
                        crossover-tournaments
                        (lambda-select 3))]
        (searcher method 100 3))
      {:label "two-point-cross_without_mutation_20+3"})


    (with-meta
      (let [method (->> uniform-crossover
                        crossover-tournaments
                        (lambda-select 3))]
        (searcher method 100 3))
      {:label "uniform-crossover_with_mutation_20+3"})))

(defn now [] (new java.util.Date))


(defn output-file
  "create a file to be written to with a nice name for research"
  [reps evals]
  (clojure.java.io/writer
    (format "data/genetic/%d_reps_%d_evals_%s" reps evals (now))))

(defn research [reps evals tests]
  "do an experiment with a nice file name"
  (time
    (with-open [file (output-file reps evals)]
      (binding [*out* file]
        (apply exp/do-main reps evals tests)))))


  (defn -main
    []
    (ns simple-search.genetic-exp)
    (research 30 num-evals tests)
    (shutdown-agents))
  ;;(use 'clojure.stacktrace)
  ;;(e)




