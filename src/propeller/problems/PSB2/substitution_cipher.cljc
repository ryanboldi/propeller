(ns propeller.problems.PSB2.substitution-cipher
  (:require [psb2.core :as psb2]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.utils :as utils]
            [propeller.push.utils.helpers :refer [get-stack-instructions]]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.tools.metrics :as metrics]))

; ===========  PROBLEM DESCRIPTION  =========================
; SUBSTITUTION CIPHER from PSB2
;This problem gives 3 strings.
;The first two represent a cipher, mapping each character in
;one string to the one at the same index in the other string.
;The program must apply this cipher to the third string and
;return the deciphered message.
;
; Source: https://arxiv.org/pdf/2106.06086.pdf
; ============================================================

(defn map-vals-input
  "Returns all the input values of a map (specific helper method for substitution-cipher)"
  [i]
  (vals (select-keys i [:input1 :input2 :input3])))

(defn map-vals-output
  "Returns the output values of a map (specific helper method for substitution-cipher)"
  [i]
  (vals (select-keys i [:output1])))

(def instructions
  (utils/not-lazy
    (concat
      ;;; stack-specific instructions
      (get-stack-instructions #{:exec :integer :boolean :char :string :print})
      ;;; input instructions
      (list :in1 :in2 :in3)
      ;;; close
      (list 'close)
      ;;; ERCs (constants)
      (list 0 ""))))


(defn error-function
  ([argmap individual]
   (error-function argmap individual :train))
  ([argmap individual subset]
   (let [program (genome/plushy->push (:plushy individual) argmap)
         data (get (get argmap :train-and-test-data) subset)
         inputs (map (fn [i] (map-vals-input i)) data)
         correct-outputs (map (fn [i] (map-vals-output i)) data)
         outputs (map (fn [input]
                        (state/peek-stack
                          (interpreter/interpret-program
                            program
                            (assoc state/empty-state :input {:in1 (nth input 0)
                                                             :in2 (nth input 1)
                                                             :in3 (nth input 2)})
                            (:step-limit argmap))
                          :string))
                      inputs)
         parsed-outputs (map (fn [output]
                               (try (read-string output)
                                    #?(:clj (catch Exception e 1000.0)
                                       :cljs (catch js/Error. e 1000.0))))
                             outputs)
         errors (map (fn [correct-output output]
                       (if (= output :no-stack-item)
                         10000
                         (metrics/levenshtein-distance correct-output output)))
                     correct-outputs
                     parsed-outputs)]
     (assoc individual
       :behaviors parsed-outputs
       :errors errors
       :total-error #?(:clj  (apply +' errors)
                       :cljs (apply + errors))))))

