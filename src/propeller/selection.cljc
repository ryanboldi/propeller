(ns propeller.selection
  (:require [propeller.tools.math :as math-tools]))

(defn tournament-selection
  "Selects an individual from the population using tournaments of
  tournament-size by taking the individual in the tournament with the lowest :total-error. "
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection.
  Lexicase parent selection filters the population by considering one random training case at a time,
  eliminating any individuals with errors for the current case that are worse than the best error in the selection pool,
  until a single individual remains."
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

(defn fitness-proportionate-selection 
  "Selects an individual from the population using a fitness proportionate selection."
  [pop argmap]
  (let [pop-fits (->> pop ;convert from error to fitness, where fitness (probability) is (1/ (1+ tot_err))
                      (map #(assoc % :fitness (/ 1 (inc (:total-error %))))))
        pop-total-fit (->> pop-fits
                           (map :fitness)
                           (reduce +))
        random-num (* (rand) pop-total-fit) 
        sorted-by-fitness (->> pop-fits
                               (sort-by :fitness)
                               (reverse))]
    (loop [tot (:fitness (first sorted-by-fitness)) individuals sorted-by-fitness]
      (if (< random-num tot)
        (first individuals)
        (recur (+ tot (:fitness (first (rest individuals)))) (rest individuals))))))

(defn epsilon-list
  "List of epsilons for each training case based on median absolute deviation of errors."
  [pop]
  (let [error-list (map :errors pop)
        length (count (:errors (first pop)))]
    (loop [epsilons [] i 0]
      (if (= i length)
        epsilons
        (recur (conj epsilons
                     (math-tools/median-absolute-deviation
                       (map #(nth % i) error-list)))
               (inc i))))))

(defn epsilon-lexicase-selection
  "Selects an individual from the population using epsilon-lexicase selection.
  Epsilon lexicase selection follows the same process as lexicase selection except,
  for a test case, only individuals with an error outside of a predefined epsilon are filtered."
  [pop argmap]
  (let [epsilons (:epsilons argmap)]
    (loop [survivors pop
           cases (shuffle (range (count (:errors (first pop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (rand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map :errors survivors)))
              epsilon (nth epsilons (first cases))]
          (recur (filter #(<= (Math/abs (- (nth (:errors %)
                                                (first cases))
                                           min-err-for-case))
                              epsilon)
                         survivors)
                 (rest cases)))))))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)
    :roulette (fitness-proportionate-selection pop argmap)
    :epsilon-lexicase (epsilon-lexicase-selection pop argmap)))
