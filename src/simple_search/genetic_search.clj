(ns simple-search.genetic-search
  (:require [simple-search.core :as core])
   (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000
        simple-search.knapsack-examples.knapPI_11_200_1000
        simple-search.knapsack-examples.knapPI_13_200_1000
        simple-search.knapsack-examples.knapPI_16_200_1000))


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def ^:dynamic *add-score-counter*
  "keep track of add-score, use with binding"
  (atom 0))

(defn add-score
  "wrapper around core/add-score which increments counter"
  [answer]
  (swap! *add-score-counter* inc)
  (core/add-score
    core/penalized-score answer))

(defn make-answer
  "wrapper around core/make-answer"
  [instance choices]
  (add-score
    (core/make-answer instance choices)))

(defn wild-type
  [instance]
  "generate lazy seq of random individuals"
  (repeatedly
    #(add-score (core/random-answer instance))))

(defn best
  "take a population and return answer with highest score"
  ([population]
   (reduce max-key :score population)))

(defn first-generation
  "generate num random answers for instance"
  [instance num]
  (take num
    (distinct
      (wild-type instance))))

(defn first-generation-skinny
  [instance num]
  (take num
    (distinct
      (repeatedly
        #(add-score (core/random-answer-under-weight instance))))))

(defn lambda-select
  "take a population and have eac h child have lambda children, then take the best, including the original population"
  [lambda mutator]
  (fn [population]
    (->> #(mutator population)
      (repeatedly lambda)
      (apply concat population)
      (sort-by (comp - :score))
      dedupe
      (take (count population)))))

;; this was mutate-search
(defn dump-select
  "create a new generation from a population by mutating it, and taking the best
  n out of old and young combined, where n is population size. Population should be free of duplicates"
  [mutator]
  (lambda-select 1 mutator))

(defn simple-mutate-search
  "search by mutating population with given method, restricted to up to `evals` calls to add-score"
  ([next-generation pop-size instance evals]
   (simple-mutate-search
     first-generation next-generation pop-size instance evals))

  ([initialize-pop next-generation pop-size instance evals]
     (binding [*add-score-counter* (atom 0)]
       (best
         (take-while (fn [ _ ] (< @*add-score-counter* evals))
           (flatten
             (iterate next-generation
               (initialize-pop instance pop-size))))))))

(defn birth
  "apply a bit-mutator to answers and return a new answer"
  [bit-mutator & answers]
    (make-answer (:instance (first answers))
      (apply bit-mutator
        (map :choices answers))))

(defn tournament
  "run a genetic tournament on the population, presumably to generate parents"
  [population sample-size]
  (best (take sample-size (shuffle population))))

(defn crossover-tournaments
  "given a crossing, return a func that uses tournaments to run that crossing on the population generating a child population"
  [crossing]
  (fn [population]
    (let [gen-winner #(tournament population 2) ]
      (take (count population)
        (distinct
          (repeatedly
            #(birth crossing (gen-winner) (gen-winner))))))))


(defn get-randoms
  "return a sorted list of random numbers between 0 and max"
  [length maximum]
  (sort
    (repeatedly length
      #(rand-int maximum))))

(defn two-point-crossover
  "take two bit lists and return a new answer with two cuts"
  [choices-1 choices-2]
  (let [[first-split last-split] (get-randoms 2 (count choices-1))]
      (concat
         (take first-split choices-1)
         (take (- last-split first-split)
               (drop first-split choices-2))
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
  (map  #(add-score (core/mutate-answer %))
     population))


(defn searcher
  "creates a seracher for use in experiments"
  ([method pop-size]
   (partial simple-mutate-search method pop-size))
  ([initialize-pop method pop-size]
   (partial simple-mutate-search initialize-pop method pop-size)))


;;; random garbage

#_(apply birth two-point-crossover (take 2 (wild-type knapPI_11_20_1000_1)))

(let [method (->>; two-point-crossover
                 ; crossover-tournaments
                   mutate-pop
                   )]
  (simple-mutate-search method 100 knapPI_16_20_1000_63 100000))


(defn by-the-book-crossover-tournaments
  "like the first one but as implemented in essentials of metaheuristics"
  [crossing]
    (fn [population]
    (let [gen-winner #(tournament population 2)
          have-children (fn [p1 p2]
                           (repeatedly 2 #(birth crossing p1 p2)))]
      ( ->> #(have-children (gen-winner) (gen-winner))
            repeatedly
            flatten
            distinct
            take (/ (count population) 2)))))

;;(defn GeneticAlgorithm
;;  "Genetic Algorithm by the book"
;;  [pop-size]
;;  (let [first-gen (set (first-generation pop-size))
;;        bestist (best first-gen)]
;;    (for )
;;    )
;;  )
