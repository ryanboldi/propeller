(ns propeller.selection
  "Propeller includes many kinds of genetic operators to select parents within the population such as tournament selection,
  lexicase selection, and epsilon lexicase selection."
  {:doc/format :markdown}
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

(defn assign-ifs-error-to-population
  "overwrites the :total_error of each individual with that weighted as perscribed by implicit fitness sharing."
  [pop argmap]
  (let [errors (map #(:errors %) pop)
        max-errors (map #(apply max %) (apply map list errors))
        normalized-error (map (fn [err] (map #(if (zero? %2) 1.0 (/ %1 %2)) err max-errors)) errors)
        summed-reward-on-test-cases (map (fn [list-of-errors]
                                           (reduce + (map #(- 1.0 %) list-of-errors)))
                                         (apply map list normalized-error))
        ifs-reward (map (fn [err] (apply + (map #(if (zero? %2) 1.0 (/ %1 %2))
                                                (map #(- 1.0 %) err)
                                                summed-reward-on-test-cases))) normalized-error)
        ifs-err (map (fn [rew] (cond
                  (< 1e20 rew) 0.0
                  (zero? rew) 1e20
                  (< 1e20 (/ 1.0 rew)) 1e20
                  :else (/ 1.0 rew))) ifs-reward)]
    (map #(assoc %1 :total-error %2) pop ifs-err)))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)
    :roulette (fitness-proportionate-selection pop argmap)
    :epsilon-lexicase (epsilon-lexicase-selection pop argmap)
    :ifs (tournament-selection (assign-ifs-error-to-population pop argmap) argmap)))
