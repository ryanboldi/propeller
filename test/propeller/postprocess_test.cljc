(ns propeller.postprocess-test
  (:require [clojure.test :as t]
            [propeller.postprocess :as pp]
            [propeller.genome :as genome]
            [propeller.simplification :as simplification]))

(t/deftest majority-voting-test
  (t/testing "majority voting"
    (let [voting-population [{:plushy {:id 1}} {:plushy {:id 1}} {:plushy {:id 2}}]
          testing-data [{:output 1} {:output 2} {:output 1}]
          argmap {:error-function (fn [_ _ plushy] {:behaviors (repeat 3 {:output (:id plushy)})})
                  :error-comparator (fn [output true-output] (= (:output output) true-output))}]
      (t/is (= '(true false true) (pp/majority-voting voting-population testing-data argmap))))))

(t/deftest get-all-successful-solutions-test
  (t/testing "get all successful solutions"
    (let [evaluated-pop [{:total-error 0} {:total-error 1} {:total-error -1}]]
      (t/is (= [{:total-error 0} {:total-error -1}] (pp/get-all-successful-solutions evaluated-pop))))))

(t/deftest should-end-run?-test
  (t/testing "should end run"
    (let [generation 1
          best-individual-passes-ds true
          evaluated-pop [{:total-error 0}]
          indexed-training-data [{:input 1 :output 1}]
          argmap {:solution-error-threshold 0
                  :error-function (fn [_ _ _] {:total-error 0})
                  :downsample? false}]
      (t/is (pp/should-end-run? generation best-individual-passes-ds evaluated-pop indexed-training-data argmap)))))

(t/deftest proportion-solves?-test
  (t/testing "proportion solves"
    (let [generation 1
          evaluated-pop [{:total-error 0} {:total-error 1} {:total-error 0}]
          proportion 0.5]
      (t/is (pp/proportion-solves? generation evaluated-pop proportion)))))

(t/deftest should-end-run-arc?-test
  (t/testing "should end run arc"
    (let [generation 1
          best-individual-passes-ds true
          evaluated-pop [{:total-error 0} {:total-error 1} {:total-error 0}]
          indexed-training-data [{:input 1 :output 1}]
          argmap {:proportion 0.5}]
      (t/is (pp/should-end-run-arc? generation best-individual-passes-ds evaluated-pop indexed-training-data argmap)))))

(t/deftest print-run-stats-test
  (t/testing "print run stats"
    (let [generation 1
          best-individual {:plushy {:id 1}}
          error-function (fn [_ _ _] {:total-error 0})
          argmap {:testing-data [{:input 1 :output 1}]
                  :simplification? false}]
      (t/is (nil? (pp/print-run-stats generation best-individual error-function argmap))))))

(t/deftest print-run-stats-arc-test
  (t/testing "print run stats arc"
    (let [generation 1
          indexed-training-data [{:input 1 :output 1}]
          evaluated-pop [{:plushy {:id 1}} {:plushy {:id 2}} {:plushy {:id 3}}]
          error-function (fn [_ _ plushy] {:behaviors [{:output (:id plushy)}]})
          argmap {:testing-data [{:input 1 :output 1}]
                  :error-function error-function
                  :error-comparator =}]
      (t/is (nil? (pp/print-run-stats-arc generation indexed-training-data evaluated-pop error-function argmap))))))

(t/deftest run-too-long?-test
  (t/testing "run too long"
    (let [generation 100
          evaluations 1000
          indexed-training-data [{:input 1 :output 1}]
          argmap {:max-generations 100
                  :population-size 10
                  :downsample? false}]
      (t/is (pp/run-too-long? generation evaluations indexed-training-data argmap)))))
