(ns propeller.gp
  (:require [clojure.string]
            [clojure.pprint]
            [propeller.genome :as genome]
            [propeller.variation :as variation]
            [propeller.selection :as selection]
            [propeller.push.instructions.bool]
            [propeller.push.instructions.character]
            [propeller.push.instructions.code]
            [propeller.push.instructions.input-output]
            [propeller.push.instructions.numeric]
            [propeller.push.instructions.polymorphic]
            [propeller.push.instructions.string]
            [propeller.push.instructions.vector]))

(defn report
  "Reports information each generation."
  [pop generation argmap]
  (let [best (first pop)]
    (println best)
    (clojure.pprint/pprint {:generation            generation
                            :best-plushy           (:plushy best)
                            :best-program          (genome/plushy->push (:plushy best) argmap)
                            :best-total-error      (:total-error best)
                            :best-errors           (:errors best)
                            :best-behaviors        (:behaviors best)
                            :genotypic-diversity   (float (/ (count (distinct (map :plushy pop))) (count pop)))
                            :behavioral-diversity  (float (/ (count (distinct (map :behaviors pop))) (count pop)))
                            :average-genome-length (float (/ (reduce + (map count (map :plushy pop))) (count pop)))
                            :average-total-error   (float (/ (reduce + (map :total-error pop)) (count pop)))
                            })
    (println)))

(defn gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size solution-error-threshold mapper]
    :or   {solution-error-threshold 0.0
           ;; The `mapper` will perform a `map`-like operation to apply a function to every individual
           ;; in the population. The default is `map` but other options include `mapv`, or `pmap`.
           mapper #?(:clj pmap :cljs map)}
    :as   argmap}]
  ;;
  (prn {:starting-args (update (update argmap :error-function str) :instructions str)})
  (println)
  ;;
  (loop [generation 0
         population (mapper
                      (fn [_] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
                      (range population-size))
         case-indices (selection/get-new-case-sample-indices (:downsample-size argmap) (:training-data argmap))]
    (let [evaluated-pop (sort-by :total-error
                                 (mapper
                                   (partial error-function argmap 
                                            (selection/get-cases-from-indices case-indices (:training-data argmap))) ;applies error function on current sample of data
                                   population))
          best-individual (first evaluated-pop)]
      (if (:custom-report argmap)
        ((:custom-report argmap) evaluated-pop generation argmap)
        (report evaluated-pop generation argmap))
      (cond
        ;; Success on training cases is verified on testing cases
        (<= (:total-error best-individual) solution-error-threshold)
        (do (prn {:success-generation generation})
            (prn {:total-test-error
                  (:total-error (error-function argmap (:testing-data argmap) best-individual))}))
        ;;
        (>= generation max-generations)
        nil
        ;;
        :else (recur (inc generation)
                     (if (:elitism argmap)
                       (conj (repeatedly (dec population-size)
                                         #(variation/new-individual evaluated-pop argmap))
                             (first evaluated-pop))
                       (repeatedly population-size
                                   #(variation/new-individual evaluated-pop argmap)))
                     (selection/change-case-sample-indices case-indices 
                                                           (:training-data argmap)
                                                           (:case-step argmap)
                                                           (:case-queue? argmap)))))))