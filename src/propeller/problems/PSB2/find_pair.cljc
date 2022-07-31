(ns propeller.problems.PSB2.find-pair
  (:require [psb2.core :as psb2]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.problems.data-creation :as dc]
            [propeller.utils :as utils]
            [propeller.push.instructions :refer [get-stack-instructions]]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.gp :as gp]
            #?(:cljs [cljs.reader :refer [read-string]])))

(def train-data (dc/read-data-formatted "find-pair" "train"))
(def test-data (dc/read-data-formatted "find-pair" "test"))

(defn random-int [] (- (rand-int 201) 100))

(defn map-vals-input
  "Returns all the input values of a map"
  [i]
  (vals (select-keys i [:input1 :input2])))

(defn map-vals-output
  "Returns the output values of a map"
  [i]
  (vals (select-keys i [:output1 :output2])))

(def instructions
  (utils/not-lazy
    (concat
      ;;; stack-specific instructions
      (get-stack-instructions #{:exec :integer :vector_integer :boolean :print})
      ;;; input instructions
      (list :in1 :in2)
      ;;; close
      (list 'close)
      ;;; ERCs (constants)
      (list -1 0 1 2 random-int))))

(defn error-function
  [argmap data individual]
  (let [program (genome/plushy->push (:plushy individual) argmap)
        inputs (map (fn [i] (map-vals-input i)) data)
        correct-outputs (map (fn [i] (map-vals-output i)) data)
        outputs (map (fn [input]
                       (state/peek-stack
                         (interpreter/interpret-program
                           program
                           (assoc state/empty-state :input {:in1 (nth input 0)
                                                            :in2 (nth input 1)})
                           (:step-limit argmap))
                         :print))
                     inputs)
        parsed-outputs (map (fn [output]
                              (try (read-string (str "(" output ")"))
                                   #?(:clj  (catch Exception e 1000000.0)
                                      :cljs (catch js/Error. e 1000000.0))))
                            outputs)
        errors (map (fn [correct-output output]
                      (if (not= (count output) (count correct-output))
                        1000000.0
                        (apply + (map (fn [c o] (if (and (number? c) (number? o))
                                                  (math/abs (- c o))
                                                  1000000.0)) correct-output output))))
                    correct-outputs
                    parsed-outputs)
         ;null (prn {:output (first outputs) :correct-output (first correct-outputs) :parsed (first parsed-outputs) :error (first errors)})
        ]
    (assoc individual
      :behaviors parsed-outputs
      :errors errors
      :total-error #?(:clj  (apply +' errors)
                      :cljs (apply + errors)))))

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (gp/gp
    (merge
      {:instructions            instructions
       :error-function          error-function
       :training-data           train-data
       :testing-data            test-data
       :case-t-size             (count train-data)
       :ds-parent-rate          0
       :ds-parent-gens          1
       :max-generations         300
       :population-size         1000
       :max-initial-plushy-size 250
       :step-limit              2000
       :parent-selection        :lexicase
       :tournament-size         5
       :umad-rate               0.1
       :variation               {:umad 1.0 :crossover 0.0}
       :elitism                 false}
      (apply hash-map (map #(if (string? %) (read-string %) %) args))))
  (#?(:clj shutdown-agents)))