(ns ^:no-doc propeller.problems.ARC.arc-1d
  "1D Abstraction and Reasoning Corpus"
  (:require
   #?(:cljs [cljs.reader :refer [read-string]])
   [clojure.data.json :as json]
   [propeller.genome :as genome]
   [propeller.gp :as gp]
   [propeller.push.instructions :refer [get-stack-instructions]]
   [propeller.push.interpreter :as interpreter]
   [propeller.push.state :as state]
   [propeller.utils :as utils]))

(defn string-keys-to-symbols [map]
  (reduce #(assoc %1 (-> (key %2) keyword) (val %2)) {} map))

(defn random-int [] (- (rand-int 201) 100))
;test

; list files and folders in directory
(clojure.java.io/file "./1D-ARC")
  (->> (clojure.java.io/file "./1D-ARC/dataset/") 
       (.listFiles)
       (map #(.getName %))
       (println))

(def train-and-test-data
  (->> "./1D-ARC/dataset/1d_denoising_1c/1d_denoising_1c_0.json"
       (slurp)
       (json/read-str)
       (string-keys-to-symbols)))

(defn convert-vals-to-single-vector
  [m]
  (reduce (fn [newmap [k v]] (assoc newmap k (first v))) {} m))

;convert from {:input [[1 2 3 4]]} to {:input [1 2 3 4]}
(defn process-data [data]
  (->> data
       (map string-keys-to-symbols)
       (map convert-vals-to-single-vector)))

(def train-data (process-data (:train train-and-test-data)))
(def test-data (process-data (:test train-and-test-data)))

(def instructions
  (utils/not-lazy
   (concat
    (get-stack-instructions #{:exec :integer :boolean :vector_integer :print})
    (list :in1)
    (list 'close)
    (list random-int 0 1 2))))

(defn error-function 
  [argmap data individual]
  (let [program (genome/plushy->push (:plushy individual) argmap)
        inputs (map (fn [i] (get i :input)) data)
        correct-outputs (map (fn [i] (get i :output)) data)
        outputs (map (fn [input]
                       (state/peek-stack
                        (interpreter/interpret-program
                         program
                         (assoc state/empty-state :input {:in1 input})
                         (:step-limit argmap))
                        :vector_integer))
                     inputs)
        errors (mapcat (fn [correct-output output]
                        (if (= output :no-stack-item)
                          (repeat (count correct-output) 1000000)
                          (let [len-diff (Math/abs (- (count correct-output) (count output)))
                                element-errors (map (fn [c o] (if (and (not= c :padding) (not= o :padding)) (if (= c o) 0 1) 1000000)) 
                                                  correct-output
                                                  (concat output (repeat len-diff :padding)))]
                            element-errors)))
                      correct-outputs 
                      outputs)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error #?(:clj  (apply +' errors)
                           :cljs (apply + errors)))))


(defn -main
  "runs the top-level genetic programming function, giving it a map of arguments with defaults that can be overridden from the command line or through a passed map."
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
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))