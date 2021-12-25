(ns propeller.selection
  (:require [clojure.set :as s]))

(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors (map rand-nth (vals (group-by :errors pop)))
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn rolling-lexicase-selection
  "Selects an individual from the population using Rolling Lexicase Selection"
  [pop argmap]
  (lexicase-selection pop argmap))
;;uses regular lexicase but argmap changes setting

(defn get-new-case-sample-indices
  "returns a list of sample-size random case indices from the training data"
  [sample-size all-data]
  (let [all-data-size (count
                       (if (seq? all-data) all-data (:inputs all-data)))
        selected-indices (take sample-size (shuffle (range all-data-size)))]
    selected-indices)); for now, we just return the indices of the cases in sample

(defn get-cases-from-indices
  "converts a set of indices into an input->output map"
  [selected-indices all-data]
  (if (seq? all-data)
    (map #(nth all-data %) selected-indices)
    (hash-map :inputs (map #(nth (:inputs all-data) %) selected-indices)
              :outputs (map #(nth (:outputs all-data) %) selected-indices))))

(defn change-case-sample-indices
  [current-sample-indices all-data step-size queue?]
  (let [data-range (range 
                    (count (if (seq? all-data) all-data (:inputs all-data))))
        unused-cases (into '() (s/difference (set data-range) (set current-sample-indices)))
        current-sample-before-drop (if queue?
                                     (vec current-sample-indices)
                                     (shuffle current-sample-indices))
        current-sample-after-drop (drop-last step-size current-sample-before-drop)
        current-sample-after-add (apply conj 
                                        current-sample-after-drop 
                                        (take step-size (shuffle unused-cases)))]
    current-sample-after-add))

(defn refined-lexicase-selection
  "Selects an individual from the population using refined lexicase selection."
  [pop argmap]
  (let [initial-population (map rand-nth (vals (group-by :errors pop)))
        survivor-score-map
        (repeatedly
         (:rlexicase-samples argmap)
         (fn [] (loop [survivors initial-population
                       cases (shuffle (range (count (:errors (first pop)))))
                       depth-counter 0]
                  (if (or (empty? cases)
                          (empty? (rest survivors)))
                    (assoc {} :survivor (rand-nth survivors) :depth depth-counter)
                    (let [min-err-for-case (apply min (map #(nth % (first cases))
                                                           (map :errors survivors)))]
                      (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                                     survivors)
                             (rest cases)
                             (inc depth-counter)))))))
        selected (apply max-key :depth survivor-score-map)]
    ;(println "rlex selected with depth: " (:depth selected))
    (:survivor selected)
      ;(assoc (:survivor selected) :selection-depth (:depth selected))
    ))


(defn reverse-refined-lexicase-selection
  "Selects an individual from the population using reverse refined lexicase selection."
  [pop argmap]
  (let [initial-population (map rand-nth (vals (group-by :errors pop)))
        survivor-score-map
        (repeatedly
         (:rlexicase-samples argmap)
         (fn [] (loop [survivors initial-population
                       cases (shuffle (range (count (:errors (first pop)))))
                       depth-counter 0]
                  (if (or (empty? cases)
                          (empty? (rest survivors)))
                    (assoc {} :survivor (rand-nth survivors) :depth depth-counter)
                    (let [min-err-for-case (apply min (map #(nth % (first cases))
                                                           (map :errors survivors)))]
                      (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                                     survivors)
                             (rest cases)
                             (inc depth-counter)))))))
        selected (apply min-key :depth survivor-score-map)]
    ;(println "rrlex selected with depth: " (:depth selected))
    (:survivor selected)
      ;(assoc (:survivor selected) :selection-depth (:depth selected))
    ))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)
    :rolling-lexicase (rolling-lexicase-selection pop argmap)
    :rlexicase (refined-lexicase-selection pop argmap)
    :rrlexicase (reverse-refined-lexicase-selection pop argmap)))
