(ns propeller.utils-test
  (:require [clojure.test :as t]
            [propeller.utils :as u]
            [propeller.simplification :as s]))

(t/deftest count-points-test
  (t/is (= 6 (u/count-points '(:a :b (:c :d)))))
  (t/is (= 1 (u/count-points '())))
  (t/is (= 2 (u/count-points '(:a)))))


(t/deftest simplification-utils-test
  (t/is (= '(:hi1 :hi2) (s/delete-at-indices '(0 3) '(:hi0 :hi1 :hi2 :hi3))))
  (t/is (= '(:hi1 :hi2 :hi3) (s/delete-at-indices '(0 10) '(:hi0 :hi1 :hi2 :hi3))))
  (t/is (= '(:hi1 :hi2) (s/delete-at-indices '(0 0 0 0 3 3 3) '(:hi0 :hi1 :hi2 :hi3))))
  (t/is (= (count (s/delete-at-indices (s/choose-random-k 3 (range 10)) (range 10))) 7))
  (t/is (= (count (s/delete-k-random 3 (range 10))) 7))
  (t/is (apply < (s/delete-k-random 3 (range 10)))) ; checks that order is maintained
           ) 