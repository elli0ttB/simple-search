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


(take 10 (distinct (wild-type knapPI_11_20_1000_7)))


(defn get-distinct
  "gets n distinct items from lazy seq of items"
  [num lazyseq]

  let [get-distinct #(distinct (take % lazyseq))
       add-more (fn [items]
                  (let [diff (- num (count items))
                        new-items (concat items (get-distinct diff))]
                    (if (< (count new-items) num)
                      (recur new-items)
                      new-items)))]
         (add-more '()))

(defn first-generation
  "generate num random answers for instance"
  [instance num]
  (take num (wild-type instance)))
  ;(get-distinct num (wild-type instance)))


(first-generation knapPI_11_20_1000_1 10)


(defn mutate-pop
  "generate children of population using mutation from core.clj"
  [population]
  (map  #(add-score (core/mutate-answer %)) population))

(defn modify-generation
  "create a new generation from a population by mutating it, and taking the best
  n out of old and young combined, where n is population size"
  [population mutator]
  (->> population
      (concat (mutator population))
      (sort-by #(-> % :score - ))
      distinct
      (take (count population))))


(defn mutate-search
  "generate an answer through randomly mutating a population"
  [instance mutator evals]
  (let [start (first-generation instance 100)
        modify #(modify-generation % mutator)
        generations (iterate modify start)
        final-pop (nth generations evals)
        bestest (apply max-key :score final-pop)]
    bestest))

 (mutate-search knapPI_11_20_1000_4 mutate-pop 100)

 (defn tournament
  [population num-competitors]
  (apply max-key :score (take num-competitors (shuffle population))))

(defn crossover-mutator
  "use tournament selection to generate children"
  [population crossing]
  (let [gen-winner #(tournament population 10)
        gen-child #(crossing (gen-winner) (gen-winner))]
    (repeat (count population) (gen-child))))


(defn uniform-crossover
  "Take two answers and create a third one using uniform crossover"
  [answer-1 answer-2]
  (let [answer-vector (map vec (:choices answer-1) (:choices answer-2))
        new-choices (map rand-nth answer-vector)]
    (add-score (core/make-answer (:instance answer-1) new-choices))))


