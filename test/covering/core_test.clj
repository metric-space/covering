(ns covering.core-test
  (:require [clojure.test :refer :all]
            [covering.utils :as utils]
            [covering.redblack-tree :as rb-tree]
            [covering.core :refer :all]))


;; Ideally should run some generative property based testing

(deftest utils-distinct
  (testing "test distinct functionality (test-1)"
    (is (=
         (utils/disjoint? [2 3] [3 4])
         true))
    )
  (testing "test distinct functionality (should be true for reverse list) (test-3)"
    (is (=
         (utils/disjoint? [3 4] [2 3])
         true))
    )
  (testing "test distinct functionality (test-1)"
    (is (=
         (utils/disjoint? [2 5] [3 4])
         false))
    )
  (testing "test distinct functionality (should be true for reverse list) (test-4)"
    (is (=
         (utils/disjoint? [3 10] [5 7])
         false))
    )
  (testing "test distinct functionality (should be true for reverse list) (test-4)"
    (is (=
         (utils/disjoint? [3 5] [2 4])
         false))
    ))



(deftest redblack-tree-insertion-test
  (testing "Redblack tree insertion algorithm"
    (is (= [{:val [4 5], :color :black, :left {:val [3 4], :color :black, :left {:val [2 3], :color :black, :right nil}, :right nil}, :right {:val [7 9], :color :red}}
            [[7 9] [4 5] [3 4] {[2 4] [[2 3] [3 4]]} [2 4]]]
           (reduce (fn [[t s] x]
                     (let [[t_ s_] (rb-tree/insert-into-rb-tree x t)]
                       ;(prn "output " t_)
                       [t_ (concat s_ s)]))
                   [nil []]
                   [[2 4] [3 5] [7 9]])))))



(deftest overlapping-events-test
  (testing "Main function test"
    (is (=  [{:val [7 8],
              :color :black,
              :right {:val [8 9], :color :red},
              :left {:val [2 3], :color :black,
                     :left nil,
                     :right {:val [3 6], :color :red}}}

             {[7 8] [[7 9]],
              [8 9] [[8 9] [7 9]],
              [3 6] [[2 6] [3 6]],
              [2 3] [[2 6]]}]
           (overlapping-events [[8 9] [7 9] [2 6] [3 6]])))))
