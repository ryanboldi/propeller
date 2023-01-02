(ns analysis.read-programs
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.gp :as gp]
            [propeller.problems.PSB1.count-odds :as co]
            [propeller.problems.simple-classification :as sc] ;<--- important
            [propeller.selection :as sel]
            [propeller.utils :as u]
            [propeller.downsample :as ds]
            [clojure.set :as set]))

; Questions I could answer:


; 1:
; How often is the individual selected with an informed down-sample one that gets selected with lexicase selection
; for each population, generate 1000 informed down-samples, each selecting 10 indivudals, also generate 10000 lexicase selections, jaccard similarity
; do the same for a random down-sample, show that the jaccard is more similar, repeat across generations and runs

; 2:
; How often does an sparsely informed ds approximate the correct full representative down-sample?
; for a set of runs, use parent sampling to calculate distances. 
; Compare relative distances between true distances and the parent samples.

; 3:
; how does the size of the down-samples change this? how does it interact with parent sampling, and others.
; maybe interpolate between down-sample sizes, and measure the lexicase selection jaccard

(defn get-train-data-from-prefix
  "prefix is something like co-50. We want to get co/train-data"
  [prefix]
  (let [prf (str (get (str prefix) 0) (get (str prefix) 1))]
    (cond (= prf "co") co/train-data
          (= prf "sc") (:train sc/train-and-test-data)))
    ;(eval (symbol (str (get (str prefix) 0) (get (str prefix) 1) "/train-data")))
  )

(defn get-error-fn-from-prefix
  "prefix is something like co-50. We want to get co/error-function"
  [prefix]
  (let [prf (str (get (str prefix) 0) (get (str prefix) 1))]
    (cond (= prf "co") co/error-function
          (= prf "sc") sc/error-function))
    ;(eval (symbol (str (get (str prefix) 0) (get (str prefix) 1) "/train-data")))
  )

(defn initialise-cases
  "initializes cases distances to be maximally far away"
  [data] 
  (ds/initialize-case-distances
   {:training-data (ds/assign-indices-to-data data) :population-size 1000}))

(defn evaluate-push-population [argmap problem-data file err-func]
  (map #(err-func argmap problem-data %) (read-string (slurp file))))

(defn full-eval->ds-eval
  "takes an already fully evaluated population and fakes a down-sample evaluation"
  [evald-pop downsample]
  (map 
   (fn [ind] 
     (assoc ind :errors 
            (u/filter-by-index (:errors ind) (map #(:index %) downsample))))
   evald-pop))

(defn update-distances
  [cases evald-pop] 
  (ds/update-case-distances evald-pop cases cases :solved))

(defn sample-informed-ds
  "creates n informed down-samples from an evaluated population. 
   uses a new set of parent reps to generate each ds.
   ds rate given by r and parent rate given by rho"
  [n evald-pop cases r rho]
    (repeatedly n 
                #(let 
                  [reps (take (* rho (count evald-pop)) (shuffle evald-pop))
                   updated-cases (update-distances cases reps)]
   (ds/select-downsample-maxmin updated-cases {:downsample-rate r}))))

(defn sample-random-ds
  "creates n random down-samples from an evaluated population. 
   ds rate given by r"
  [n cases r]
    (repeatedly n #(ds/select-downsample-random cases {:downsample-rate r})))

(defn coverage-ds
  "Meaures the coverage of a list of down-samples (prop of all cases included)"
  [dss]
  (let [ds-indices (map #(map (fn [a] (:index a)) %) dss)
        all-indices (set (flatten ds-indices))]
    (/ (count all-indices) 200)))

(for [r '(0.01 0.1)]
  (let [n (* r 5)]
    (+ n 1)
    (+ n 2)))

;lein run -m propeller.problems.PSB1.count-odds :parent-selection :lexicase :downsample? true :ds-function :case-maxmin :downsample-rate 0.1 :max-generations 300 
; :ds-parent-rate 0.01 :ds-parent-gens 100 :save-gens 3 :save-pref "co-small-0" :population-size 6

(defn -main
  "For each population starting with prefix, for each ds size in ds-size-list, for x given, for n given
   - generate x full info downsamples
   - generate x samples with IDS
   - generate x random downsamples
   1) compare samples with IDS to those with full info, showing if IDS is close to full info
   2) measure coverage of all test cases with random vs ids vs full info
   Then
   - Select nx parents with full lexicase
   - Select nx parents with full info
   - Select nx parents with IDS samples
   - Select nx parents with random samples
   3) compare selected individuals using jaccard similarity to full lexicase" 
  [& args]
  (let [arguments (merge
                   {:prefix "par-co-"
                    :ds-size-list '(0.01 0.05 0.1 0.2 0.5)
                    :x 10
                    :n 100}
                   (apply hash-map (map #(if (string? %) (read-string %) %) args)))
        gen 27 ; potentially should use gen 0, gen middle, and last gen
        file (str "./run-data/par-" (:prefix arguments) "-lexicase-case-maxmin-0.05-0.01-100-" gen ".edn")
        data (initialise-cases (get-train-data-from-prefix (:prefix arguments)))
        err-func (get-error-fn-from-prefix (str (:prefix arguments)))
        individuals (evaluate-push-population {:step-limit 2000} data file err-func)]
    (prn (for [r (:ds-size-list arguments)]
      (let [full-info-ds (sample-informed-ds (:x arguments) individuals data r 1)
            info-ds (sample-informed-ds (:x arguments) individuals data r 0.01)
            rand-ds (sample-random-ds (:x arguments) data r)
            coverage {:r r :full (coverage-ds full-info-ds) :info (coverage-ds info-ds) :rand (coverage-ds rand-ds)}]
        coverage)))) )


(comment
(let [reps (take (* 0.01 (count evaluated-population-sc)) (shuffle evaluated-population-sc))
                   updated-cases (update-distances cases reps)]
   (ds/select-downsample-maxmin updated-cases {:downsample-rate 0.01})))

(comment 
(coverage-ds (sample-informed-ds 10 evaluated-population-sc (:train sc/train-and-test-data) 0. 0.1))
;18/50

(coverage-ds (sample-random-ds 10 (:train sc/train-and-test-data) 0.05))
;21/50

(conj '(12/13) 12/13)

; How does the coverage change with downsample size?
(def coverages
  (loop [i 1
         cm '()
         cr '()]
    (if (>= i 100)
      {:coverage-mm cm :coverage-cr cr}
      (recur (+ i 5) (conj cm 
                           (coverage-ds (sample-informed-ds 10 evaluated-population-sc (:train sc/train-and-test-data) (/ i 100) 1)))
             (conj cr (coverage-ds (sample-random-ds 10 (:train sc/train-and-test-data) (/ i 100))))))))

coverages

(map #(float %) (reverse '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 99/100 1 1 99/100 1 49/50 97/100 1 99/100 49/50 19/20 19/20 19/20 47/50 9/10 22/25 83/100 21/25 79/100 17/25 16/25 57/100 11/25 3/10 19/100 0)))



pop
(def ind (first pop))

co/train-data

(defn eval-pop-co [pop]
  (map #(co/error-function {:step-limit 2000} co/train-data %) pop))

(def evald-pop (eval-pop-co pop))

evald-pop


(def cases 

cases

(def updated-cases )
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
(jaccard new-pop-rds new-pop-full))
)