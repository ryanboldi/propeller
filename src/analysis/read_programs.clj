(ns analysis.read-programs
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.gp :as gp]
            [propeller.hyperselection :as h]
            [propeller.tools.math :as math]
            [propeller.problems.PSB1.count-odds :as co]
            [propeller.problems.PSB2.fizz-buzz :as fb]
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
          (= prf "fb") fb/train-data
          (= prf "sc") (:train sc/train-and-test-data)))
    ;(eval (symbol (str (get (str prefix) 0) (get (str prefix) 1) "/train-data")))
  )

(defn get-error-fn-from-prefix
  "prefix is something like co-50. We want to get co/error-function"
  [prefix]
  (let [prf (str (get (str prefix) 0) (get (str prefix) 1))]
    (cond (= prf "co") co/error-function
          (= prf "fb") fb/error-function
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

(defn ds-eval->full-eval
  "takes a population selected wtih downsampling and repopulates all the error vecs"
  [full-pop evald-pop]
  (let [indices (map #(:index %) evald-pop)]
     (map #(nth full-pop %) indices)))

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

(defn test-case-coverage
  "% of the cases that are solved atleast once by an evaluated population"
  [popu]
  (let [psize (count popu)
        errs (map :errors popu)
        step-err (map #(map (fn [e] (math/step e)) %) errs)]
    (apply + (map #(if (= psize %) 0 1) (apply map + step-err))))) ;place a 0 if nobody in the pop solves, 1 otherwise

(defn select-population-with-ds
  [evald-pop ds]
  (let [ds-evald-pop (full-eval->ds-eval evald-pop ds)]
    (repeatedly (count evald-pop) #(sel/lexicase-selection ds-evald-pop {}))))


; from https://rosettacode.org/wiki/Entropy#Clojure
(defn entropy [s]
  (let [len (count s), log-2 (Math/log 2)]
    (->> (frequencies s)
         (map (fn [[_ v]]
                (let [rf (/ v len)]
                  (-> (Math/log rf) (/ log-2) (* rf) Math/abs))))
         (reduce +))))

(defn get-stats
  "takes a list of numbers and returns a map of the stats: max, min, avg"
  [li]
  (let [maxi (apply max li)
        mini (apply min li)
        tot (apply + li)
        avg (/ tot (count li))]
    {:max maxi :min mini :avg avg}))

(defn behavioral-entropy
  [pop]
  (->> pop
       (map #(:errors %))
       entropy))

(defn phenotypic-entropy
  [pop]
  (->> pop
       (map #(:plushy %))
       entropy))

(defn id-entropy
  [pop]
  (->> pop
       (map #(:index %))
       entropy))

(def ind 
  (h/reindex-pop (->> "./run-data/parents-ds-0.1-0.01-100-0.edn"
       slurp
       read-string)))

;(ds-eval->full-eval ind (full-eval->ds-eval (list (first ind)) '({:index 1})))

(defn unique-individuals
  "number of unique individuals in a pop (based on ID)"
  [pop]
  (->> pop
       (map #(:index %))
       set
       count))

(defn unique-phenotypes
  "number of unique phenotypes in a pop (based on plsuhy)"
  [pop]
  (->> pop
       (map #(:plushy %))
       set
       count))

(defn unique-behaviors
  "number of unique behaviors in a pop (based on error)"
  [pop]
  (->> pop
       (map #(:errors %))
       set
       count))

(defn min-aggregate-error
  "best aggregate score of a population"
  [pop]
  (->> pop
       (map #(:total-error %))
       (apply min)))


;(unique-individuals '({:index 0} {:index 1} {:index 0} {:index 3}))

;(unique-phenotypes (list ind1 ind1))


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
                    :ds-size-list '(0.05 0.1)
                    :x 10
                    :gen 27
                    :n 1000}
                   (apply hash-map (map #(if (string? %) (read-string %) %) args)))
        gen (:gen arguments); potentially should use gen 0, gen middle, and last gen
        file (str "./run-data/par-" (:prefix arguments) "-lexicase-case-maxmin-0.05-0.01-100-" gen ".edn")
        data (initialise-cases (get-train-data-from-prefix (:prefix arguments)))
        err-func (get-error-fn-from-prefix (str (:prefix arguments)))
        individuals (h/reindex-pop (evaluate-push-population {:step-limit 2000} data file err-func))]
    (clojure.pprint/pprint (for [r (:ds-size-list arguments)]
      (let [full-info-ds (sample-informed-ds (:x arguments) individuals data r 1)
            info-ds (sample-informed-ds (:x arguments) individuals data r 0.01)
            rand-ds (sample-random-ds (:x arguments) data r)
            lex-cases data
            coverage {:lex (coverage-ds lex-cases) :full (coverage-ds full-info-ds) :info (coverage-ds info-ds) :rand (coverage-ds rand-ds)}
            test-case-coverage-before (test-case-coverage individuals)
            ; for full, ids, rand. 
            ; A list of 10 populations that are selected with 10 successive downsamples of each kind
            ; to be used to measure population statistics
            full-selections (map #(ds-eval->full-eval individuals (select-population-with-ds individuals %)) full-info-ds)
            ids-selections (map #(ds-eval->full-eval individuals (select-population-with-ds individuals %)) info-ds)
            rand-selections (map #(ds-eval->full-eval individuals (select-population-with-ds individuals %)) rand-ds)
            lex-selections (select-population-with-ds individuals lex-cases)
            ;test case coverage
            full-test-coverage (map #(test-case-coverage %) full-selections)
            ids-test-coverage (map #(test-case-coverage %) ids-selections)
            rand-test-coverage (map #(test-case-coverage %) rand-selections)
            lex-test-coverage (test-case-coverage lex-selections)
            test-coverages {:before test-case-coverage-before :full full-test-coverage :ids ids-test-coverage :rand rand-test-coverage :lex lex-test-coverage}
            ; phenotypic and genotypic entropy before
            ph-ent-before (phenotypic-entropy individuals)
            be-ent-before (behavioral-entropy individuals)
            id-ent-before (id-entropy individuals)
            ; after selection
            ph-ent-lex (phenotypic-entropy lex-selections)
            be-ent-lex (behavioral-entropy lex-selections)
            id-ent-lex (id-entropy lex-selections)
            ; ds
            full-ph-ent (map #(phenotypic-entropy %) full-selections)
            full-be-ent (map #(behavioral-entropy %) full-selections)
            full-id-ent (map #(id-entropy %) full-selections)
            ids-ph-ent (map #(phenotypic-entropy %) ids-selections)
            ids-be-ent (map #(behavioral-entropy %) ids-selections)
            ids-id-ent (map #(id-entropy %) ids-selections)
            rand-ph-ent (map #(phenotypic-entropy %) rand-selections)
            rand-be-ent (map #(behavioral-entropy %) rand-selections)
            rand-id-ent (map #(id-entropy %) rand-selections)
            be-entropies {:before be-ent-before :full full-be-ent :ids ids-be-ent :rand rand-be-ent :lex be-ent-lex}
            ph-entropies {:before ph-ent-before :full full-ph-ent :ids ids-ph-ent :rand rand-ph-ent :lex ph-ent-lex}
            id-entropies {:before id-ent-before :full full-id-ent :ids ids-id-ent :rand rand-id-ent :lex id-ent-lex}
            ;unique individuals selected
            before-unq-i (unique-individuals individuals)
            lex-unq-i (unique-individuals lex-selections)
            full-unq-i (map #(unique-individuals %) full-selections)
            ids-unq-i (map #(unique-individuals %) ids-selections)
            rand-unq-i (map #(unique-individuals %) rand-selections)
            ;unique phenotypes
            before-unq-p (unique-phenotypes individuals)
            lex-unq-p (unique-phenotypes lex-selections)
            full-unq-p (map #(unique-phenotypes %) full-selections)
            ids-unq-p (map #(unique-phenotypes %) ids-selections)
            rand-unq-p (map #(unique-phenotypes %) rand-selections)
            ;unique behaviors
            before-unq-b (unique-behaviors individuals)
            lex-unq-b (unique-behaviors lex-selections)
            full-unq-b (map #(unique-behaviors %) full-selections)
            ids-unq-b (map #(unique-behaviors %) ids-selections)
            rand-unq-b (map #(unique-behaviors %) rand-selections)
            unq-i {:before before-unq-i :full full-unq-i :ids ids-unq-i :rand rand-unq-i :lex lex-unq-i} 
            unq-p {:before before-unq-p :full full-unq-p :ids ids-unq-p :rand rand-unq-p :lex lex-unq-p} 
            unq-b {:before before-unq-b :full full-unq-b :ids ids-unq-b :rand rand-unq-b :lex lex-unq-b} 
            ;minimum agg err
            before-min-err (min-aggregate-error individuals)
            lex-min-error (min-aggregate-error lex-selections)
            full-min-err (map #(min-aggregate-error %) full-selections)
            ids-min-err (map #(min-aggregate-error %) ids-selections)
            rand-min-err (map #(min-aggregate-error %) rand-selections)
            min-err {:before before-min-err :full full-min-err :ids ids-min-err :rand rand-min-err :lex lex-min-error}]
        {:r r
         :coverage coverage
         :test-coverages test-coverages
         :phenotypic-entropies ph-entropies
         :behavioral-entropies be-entropies
         :id-entropies id-entropies
         :unique-individuals unq-i
         :unique-phenotypes unq-p
         :unique-behaviors unq-b
         :min-error min-err})))))


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
