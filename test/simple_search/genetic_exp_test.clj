(ns simple-search.genetic-exp-test
  (:use midje.sweet)
  (:use simple-search.genetic-search)
  (:use simple-search.experiment)
  (:use simple-search.genetic-exp)
  (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000
        simple-search.knapsack-examples.knapPI_11_200_1000
        simple-search.knapsack-examples.knapPI_13_200_1000
        simple-search.knapsack-examples.knapPI_16_200_1000
        simple-search.knapsack-examples.knapPI_16_1000_1000))

(def test-example
  (with-meta
    (let
      [method (->> two-point-crossover
                   (comp mutate-at-rate)
                   crossover-tournaments
                   (lambda-select 3))]
      (searcher method 100 1))
    {:label "two-point-cross with mutation 20+3"}))

(fact "run-experiments runs without error"
  (try
    (print-experimental-results
        (run-experiment
          (list test-example)
          (map get-labelled-problem
                ["knapPI_11_20_1000_4" "knapPI_13_20_1000_4" "knapPI_16_20_1000_4"
                "knapPI_11_200_1000_4" "knapPI_13_200_1000_4" "knapPI_16_200_1000_4" "knapPI_16_1000_1000_3"])
          1
          1000))
    true (catch Exception e
           false))
=> true)

(fact "do-main runs without error"
     (try (do-main 1 300 test-example)
       true
      (catch Exception e
        false))
 => true)


;; the below tests are less important now.

(fact "we can use get-labelled-problem to modify a problem"
      (map? ( get-labelled-problem "knapPI_11_20_1000_4" )) => true
      (map? ( get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( get-labelled-problem "knapPI_13_20_1000_2")) => true)

(fact "we can use map on get-labelled-problem"
        (count (map get-labelled-problem
                            ["knapPI_11_20_1000_4" "knapPI_13_20_1000_4" "knapPI_16_20_1000_4"
                            "knapPI_11_200_1000_4" "knapPI_13_200_1000_4" "knapPI_16_200_1000_4"
                            "knapPI_16_1000_1000_3" ])) => 7)
