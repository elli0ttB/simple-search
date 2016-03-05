(ns simple-search.genetic-search-test
  (:use midje.sweet)
  (:require [simple-search.core :as core])
  (:use simple-search.genetic-search)
  (:use simple-search.knapsack-examples.knapPI_13_20_1000))

(def population
  (set (first-generation knapPI_13_20_1000_1 10)))

(def sample-ans
  (first population))

(facts "about best"
        (contains? population (best population)) => true
        (best (list sample-ans)) => sample-ans)

(facts "about simple-mutate-search"
    (binding [simple-search.genetic-search/counter (atom 0)]
     (let [method (->> two-point-crossover crossover-tournaments (lambda-select 2))]
       (simple-mutate-search method 100 2 knapPI_13_20_1000_3 5000)

       (< @simple-search.genetic-search/counter 7000))) => true)


(comment
  (defn test-it [n] (binding [simple-search.genetic-search/counter (atom 0)]
     (let [method (->> two-point-crossover crossover-tournaments (lambda-select 2))]
       (simple-mutate-search method 100 2 knapPI_13_20_1000_3 n)))))
