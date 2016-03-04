(ns simple-search.genetic-search-test
  (:use midje.sweet)
  (:use simple-search.genetic-search)
  (:use simple-search.knapsack-examples.knapPI_13_20_1000))

(def population
  (set (first-generation knapPI_13_20_1000_1 10)))

(def sample-ans
  (first population))

(facts "about best"
        (contains? population (best population)) => true
        (best (list sample-ans)) => sample-ans)
