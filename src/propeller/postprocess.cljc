(ns propeller.postprocess
  "Useful postprocessing functions, after GP is done, before we can use the results."
  (:require [clojure.zip :as zip]
            [clojure.repl :as repl]
            [propeller.genome :as genome]
            [propeller.simplification :as simplification]
            [propeller.tools.metrics :as metrics]
            [propeller.tools.math :as math]
            [propeller.push.instructions.parentheses :as parentheses]))


(defn majority-voting [voting-population testing-data argmap]
  "given a list of individuals and data, returns whether the most frequent output is the correct one"
  (let [error-function (:error-function argmap)
        error-comparator (:error-comparator argmap)
        plushies (map :plushy voting-population)
        behaviors (map (fn [plushy] (:behaviors (error-function argmap testing-data plushy))) plushies)
        true-outputs (map (fn [i] (get i :output)) testing-data)
        behavior->correct (fn [behavior true-outputs] (map error-comparator behavior true-outputs))
        correct-or-not (map (fn [behavior] (behavior->correct behavior true-outputs)) behaviors)
        _ (prn correct-or-not)
        transposed-correct-or-not (apply map list correct-or-not)
        _ (prn transposed-correct-or-not)
        all-scores (count (first transposed-correct-or-not))
        _ (prn all-scores)
        score-counts (map (fn [column] (count (filter true? column))) transposed-correct-or-not)
        _ (prn score-counts)]
        (map #(> % (* all-scores 0.5)) score-counts)))

(defn get-all-successful-solutions [evaluated-pop]
    "gets all solutions with a zero error"
    (filter (fn [individual] (<= (:total-error individual) 0)) evaluated-pop))

;(defn get-all-successful-solutions
;    "gets all solutions with a zero error")

(defn should-end-run? [generation best-individual-passes-ds evaluated-pop indexed-training-data argmap]
    "returns true if the run should end, false otherwise"
    (let [best-individual (first evaluated-pop)
          solution-error-threshold (:solution-error-threshold argmap)
          error-function (:error-function argmap)
          downsample? (:downsample? argmap)]
        (or (and best-individual-passes-ds
                       (<= (:total-error (error-function argmap indexed-training-data best-individual))
                           solution-error-threshold))
                  (and (not downsample?)
                       (<= (:total-error best-individual)
                           solution-error-threshold)))))

; only ends the run if more than proportion of the individuals pass the training data
; default is 50%
(defn proportion-solves? [generation evaluated-pop proportion]
    (let [errors (map :total-error evaluated-pop)
          individuals-solving (count (filter (fn [error] (<= error 0)) errors))]
        (> individuals-solving (* proportion (count errors)))))

(defn should-end-run-arc? [generation best-individual-passes-ds evaluated-pop indexed-training-data argmap]
    (let [proportion (:proportion argmap)]
        (proportion-solves? generation evaluated-pop proportion)))



(defn print-run-stats [generation best-individual error-function argmap]
    "prints the run stats"
    (prn {:success-generation generation})
    (prn {:successful-plushy (:plushy best-individual)})
    (prn {:successful-program (genome/plushy->push (:plushy best-individual) argmap)})
    (prn {:total-test-error
                      (:total-error (error-function argmap (:testing-data argmap) best-individual))})
    (when (:simplification? argmap)
                  (let [simplified-plushy (simplification/auto-simplify-plushy (:plushy best-individual) error-function argmap)]
                    (prn {:total-test-error-simplified
                          (:total-error (error-function argmap (:testing-data argmap) {:plushy simplified-plushy}))})
                    (prn {:simplified-plushy simplified-plushy})
                    (prn {:simplified-program (genome/plushy->push simplified-plushy argmap)}))))

(defn print-run-stats-arc [generation indexed-training-data evaluated-pop error-function argmap]
    (prn {:success-generation generation})
    ;(prn {:successful-plushy (:plushy best-individual)})
    ;(prn {:successful-program (genome/plushy->push (:plushy best-individual) argmap)})
    (prn {:total-test-error
      (majority-voting evaluated-pop (:testing-data argmap) argmap)}))
; TODO: add simplification for all solving individuals before majority voting

(defn run-too-long? [generation evaluations indexed-training-data argmap]
    "returns true if the run has been going on for too long, false otherwise"
    (let [max-generations (:max-generations argmap)
          population-size (:population-size argmap)
          downsample? (:downsample? argmap)]
        (or (and (not downsample?)
                    (>= generation max-generations))
                (and downsample?
                    (>= evaluations (* max-generations population-size (count indexed-training-data)))))
    )
)

;(defn get-best-solution
;    "gets the best solution from the population")

;(defn get-all-solutions
;    "gets all solutions from the population")
