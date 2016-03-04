(ns simple-search.genetic-exp-test
  (:use midje.sweet)
  (:require [simple-search.experiment :as exp])
  (:use simple-search.genetic-exp)
  (:use simple-search.knapsack-examples.knapPI_13_20_1000))

(fact "do-main runs without error"
       (string?
         (with-out-str
           (exp/do-main 1 300 tests ))) => true)

(fact "we can use get-labelled-problem to modify a problem"
      (map? ( exp/get-labelled-problem "knapPI_11_20_1000_4" )) => true
      (map? ( exp/get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( exp/get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( exp/get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( exp/get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( exp/get-labelled-problem "knapPI_13_20_1000_4" )) => true
      (map? ( exp/get-labelled-problem "knapPI_13_20_1000_2")) => true)

(fact "we can use map on get-labelled-problem"
        (count (map exp/get-labelled-problem
                            ["knapPI_11_20_1000_4" "knapPI_13_20_1000_4" "knapPI_16_20_1000_4"
                            "knapPI_11_200_1000_4" "knapPI_13_200_1000_4" "knapPI_16_200_1000_4"
                            "knapPI_16_1000_1000_3" ])) => 7)
