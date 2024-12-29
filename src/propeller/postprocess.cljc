(ns propeller.postprocess
  "Useful postprocessing functions, after GP is done, before we can use the results."
  (:require [clojure.zip :as zip]
            [clojure.repl :as repl]
            [propeller.genome :as genome]
            [propeller.simplification :as simplification]
            [propeller.tools.metrics :as metrics]
            [propeller.tools.math :as math]
            [propeller.push.instructions.parentheses :as parentheses]))


(defn majority-voting [behaviors]
  "Given a list of behaviors (outputs), returns the most frequent behavior."
  (->> behaviors
       (frequencies)
       (sort-by val)
       (last)
       (key)))

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

; only ends the run if more than 50% of the individuals pass the training data
;(defn should-end-run-ARC?)

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
