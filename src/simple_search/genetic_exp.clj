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
      {:label "two-point-cross with mutation 20+3"})


    (with-meta
      (let [method (->> uniform-crossover
                        crossover-tournaments
                        (lambda-select 3))]
        (searcher method 100 3))
      {:label "uniform-crossover, no mutation 20+3"})

    (with-meta
      (let [method (->> mutate-pop
                        (lambda-select 3))]
        (searcher method 100 3))
      {:label "random-mutation, 20+3"})

    #_(with-meta
      (let [method (->> mutate-pop
                        (lambda-select 3))]
        (searcher first-generation-skinny method 100 3))
      {:label "random-mutation, 20+3 with intial pop below capacity"})))

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
    (research 30 10000 tests)
    (shutdown-agents))
  ;;(use 'clojure.stacktrace)
  ;;(e)




