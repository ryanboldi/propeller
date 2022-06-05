(ns propeller.problems.data-creation
  (:require [psb2.core :as psb2]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn generate-data [problem train-or-test]
  (let [train-and-test-data (psb2/fetch-examples "data" problem 200 1000)
        cleaned-data (cons (vector "input1" "output1") (map #(vector (:input1 %) (:output1 %)) ((keyword train-or-test) train-and-test-data)))]
    (prn cleaned-data)
    (with-open [writer (io/writer (str problem "-" train-or-test ".csv"))]
      (csv/write-csv writer
                     (doall cleaned-data)))))

(defn generate-data-gcd [train-or-test]
  (let [train-and-test-data (psb2/fetch-examples "data" "gcd" 200 1000)
        cleaned-data (cons (vector "input1" "input2" "output1") (map #(vector (:input1 %) (:input2 %) (:output1 %)) ((keyword train-or-test) train-and-test-data)))]
    (prn cleaned-data)
    (with-open [writer (io/writer (str "gcd-" train-or-test ".csv"))]
      (csv/write-csv writer
                     (doall cleaned-data)))))

(defn generate-data-find-pair [train-or-test]
  (let [train-and-test-data (psb2/fetch-examples "data" "find-pair" 200 1000)
        cleaned-data (cons (vector "input1" "input2" "output1" "output2") (map #(vector (:input1 %) (:input2 %) (:output1 %) (:output2 %)) ((keyword train-or-test) train-and-test-data)))]
    (prn cleaned-data)
    (with-open [writer (io/writer (str "find-pair-" train-or-test ".csv"))]
      (csv/write-csv writer
                     (doall cleaned-data)))))


(generate-data-find-pair "test")
(generate-data-find-pair "train")
(generate-data-gcd "test")
(generate-data-gcd "train")


(defn generate-data-for-problem [problem]
  (map (partial generate-data problem) '["test" "train"]))

(defn generate-data-for-all-problems []
  (map (partial generate-data-for-problem) '["gcd"
                                             "find-pair"]))

(generate-data-for-all-problems)

;--------PSB1

(defn read-data [problem qual]
  (with-open [reader (io/reader (str "src/propeller/problems/PSB1/" problem "-" qual ".csv"))]
    (doall
     (csv/read-csv reader))))

(defn edge-cases-for-problem [problem]
  (read-data problem  "edge"))

(defn training-cases-for-problem [shuffled-data problem]
  (let [edge-cases (edge-cases-for-problem problem)
        left (- 201 (count edge-cases)) ;because labels is first
        random-cases (take left shuffled-data)]
    (concat edge-cases random-cases)))

(defn testing-cases-for-problem [shuffled-data problem]
  (take 1000 (drop 500 shuffled-data)))

(defn save-train-test-data [shuffled-data problem train-or-test]
  (with-open [writer (io/writer (str problem "-" train-or-test ".csv"))]
    (csv/write-csv writer
                   (if (= train-or-test "train") 
                     (training-cases-for-problem shuffled-data problem)
                     (testing-cases-for-problem shuffled-data problem)))))

(defn save-data-for-problem [problem]
  (let [shuffled-data (shuffle (rest (read-data problem  "random")))]
  (map (partial save-train-test-data shuffled-data problem) '["test" "train"])))

(defn save-data-for-all-problems []
  (map (partial save-data-for-problem) '["small-or-large"
                                             "scrabble-score"
                                             "grade"
                                             "count-odds"]))

(defn read-data-that-has-no-strings [problem train-or-test]
  (apply list (with-open [reader (io/reader (str "picked/" problem "-" train-or-test ".csv"))]
    (let [csv-data (csv/read-csv reader)]
     (mapv zipmap
          (->> (first csv-data) ;; First row is the header
               (map keyword) ;; Drop if you want string keys instead
               repeat)
          (map (fn [elem] (map #(read-string %) elem)) (rest csv-data)))))))

(read-data-that-has-no-strings "fuel-cost" "test")