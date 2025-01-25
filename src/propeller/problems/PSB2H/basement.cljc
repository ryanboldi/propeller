(ns propeller.problems.PSB2H.basement
  "BASEMENT from PSB2

 Given a vector of integers, return the first
 index such that the sum of all integers from the start of the
 vector to that index (inclusive) is negative.

 Source: https://arxiv.org/pdf/2106.06086.pdf"
  {:doc/format :markdown}
  (:require
   #?(:cljs [cljs.reader :refer [read-string]])
   [clojure.data.json :as json]
   [propeller.genome :as genome]
   [propeller.gp :as gp]
   [propeller.push.instructions :refer [get-stack-instructions]]
   [propeller.push.interpreter :as interpreter]
   [propeller.push.state :as state]
   [propeller.tools.math :as math]
   [propeller.utils :as utils]
   [psb2.core :as psb2]))


;load train data from json file


(def train-data (map #(json/read-str % :key-fn keyword) (line-seq (clojure.java.io/reader "data/datasets/basement/basement-hyp0.json"))))

train-data

(defn load-hyp-data [hyp-num]
  (map #(json/read-str % :key-fn keyword) (line-seq (clojure.java.io/reader (str "data/datasets/basement/basement-hyp" hyp-num ".json")))))

(load-hyp-data 0)
(load-hyp-data 1)

(def train-and-test-data "Data taken from https://zenodo.org/record/5084812" (psb2/fetch-examples "data" "basement" 200 2000))

(defn random-int
  "Random integer between -100 and 100 (from smallest)"
  [] (- (rand-int 201) 100))

(def instructions
  "Stack-specific instructions, input instructions, close, and constants"
  (utils/not-lazy
   (concat
      ;;; stack-specific instructions
    (get-stack-instructions #{:exec :integer :boolean :vector_integer :print})
      ;;; input instructions
    (list :in1)
      ;;; close
    (list 'close)
      ;;; ERCs (constants)
    (list random-int -1 0 1 []))))

(defn error-function
  "Finds the behaviors and errors of an individual: Error is 0 if the value and
  the program's selected behavior match, or 1 if they differ, or 1000000 if no
  behavior is produced. The behavior is here defined as the final top item on
  the INTEGER stack."
  [argmap data individual]
  (let [program (genome/plushy->push (:plushy individual) argmap)
        inputs (map (fn [i] (get i :input1)) data)
        correct-outputs (map (fn [i] (get i :output1)) data)
        outputs (map (fn [input]
                       (state/peek-stack
                        (interpreter/interpret-program
                         program
                         (assoc state/empty-state :input {:in1 input})
                         (:step-limit argmap))
                        :integer))
                     inputs)
        errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        (math/abs (- correct-output output))))
                    correct-outputs
                    outputs)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error #?(:clj  (apply +' errors)
                           :cljs (apply + errors)))))

(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (let [hyp-num (Integer/parseInt (first args))
        train-data (load-hyp-data hyp-num)
        test-data (load-hyp-data hyp-num)
        args (rest args)]
    (gp/gp
     (merge
      {:instructions            instructions
       :error-function          error-function
       :training-data           train-data
       :testing-data            test-data
       :max-generations         300
       :population-size         1000
       :max-initial-plushy-size 250
       :step-limit              2000
       :parent-selection        :lexicase
       :tournament-size         5
       :umad-rate               0.1
       :downsample?             true
       :downsample-rate         0.2
       :variation               {:umad 1.0 :crossover 0.0}
       :elitism                 false}
      (apply hash-map (map #(if (string? %) (read-string %) %) args))))))
