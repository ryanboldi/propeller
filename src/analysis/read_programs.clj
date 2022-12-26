(ns analysis.read-programs
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.gp :as gp]
            [propeller.problems.PSB1.count-odds :as co]
            [propeller.problems.simple-classification :as sc] ;<--- important
            [propeller.selection :as sel]
            [propeller.downsample :as ds]
            [clojure.set :as set]
            #?(:cljs [cljs.reader :refer [read-string]])))

(def pop (read-string (slurp "./run-data/parents-ds-0.1-0.01-100-200.edn")))

pop
(def ind (first pop))

co/train-data

(defn eval-pop-co [pop]
  (map #(co/error-function {:step-limit 2000} co/train-data %) pop))

(def evald-pop (eval-pop-co pop))

evald-pop


(defn evaluate-push-population [argmap problem-data file err-func]
  (map #(err-func argmap problem-data %) (read-string (slurp file))))

(def evaluated-population-sc
  (evaluate-push-population 
   {:step-limit 2000}
   (:train sc/train-and-test-data)
   "./run-data/parents-ds-0.1-0.01-100-200.edn" 
   sc/error-function))


(def cases (ds/initialize-case-distances {:training-data (ds/assign-indices-to-data (:train sc/train-and-test-data)) :population-size 1000}))

cases

(def updated-cases (ds/update-case-distances evaluated-population-sc cases cases :solved))
updated-cases


(def mm-ds (ds/select-downsample-maxmin updated-cases {:downsample-rate 0.1}))
(def r-ds (ds/select-downsample-random updated-cases {:downsample-rate 0.1}))

(def eval-on-mmds (evaluate-push-population 
   {:step-limit 2000}
   mm-ds
   "./run-data/parents-ds-0.1-0.01-100-200.edn" 
   sc/error-function))

(def eval-on-rds (evaluate-push-population 
   {:step-limit 2000}
   r-ds
   "./run-data/parents-ds-0.1-0.01-100-200.edn" 
   sc/error-function))

(def eval-on-full (evaluate-push-population 
   {:step-limit 2000}
   (:train sc/train-and-test-data)
   "./run-data/parents-ds-0.1-0.01-100-200.edn" 
   sc/error-function))


; select 10 individuals using the two ds strategies, and compare to a lot of lexicase selections
(def new-pop-mmds (map #(:index %) (repeatedly 10 #(sel/lexicase-selection eval-on-mmds {}))))
(def new-pop-rds (map #(:index %) (repeatedly 10 #(sel/lexicase-selection eval-on-rds {}))))
(def new-pop-full (map #(:index %) (repeatedly 10000 #(sel/lexicase-selection eval-on-full {}))))

(defn jaccard 
  "jaccard similarity of two lists, a and b. = | A ∩ B | / | A U B|"
  [a b]
  (let [A (set a)
        B (set b)
        intersect (set/intersection A B)
        numerator (count intersect)
        union (set/union A B)
        denom (count union)]
    (/ numerator denom)))

(jaccard new-pop-mmds new-pop-full)
(jaccard new-pop-rds new-pop-full)