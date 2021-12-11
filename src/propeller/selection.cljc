(ns propeller.selection)

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

(defn get-new-case-sample
(defn get-new-case-sample-indices
  [sample-size all-data]
  (let [all-data-size (count (:inputs all-data))
        selected-indices (take sample-size (shuffle (range all-data-size)))]
    ;
    selected-indices)); for now, we just return the indices of the cases in sample
(defn get-cases-from-indices
  [selected-indices all-data]
  "converts a set of indices into an input->output map"
  (hash-map :inputs (map #(nth (:inputs all-data) %) selected-indices)
          :outputs (map #(nth (:outputs all-data) %) selected-indices)))


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
