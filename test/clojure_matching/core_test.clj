(ns clojure-matching.core-test
  (:require [clojure.test :refer :all]
            [clojure-matching.core :refer :all]))

(deftest test-default
  (is (match nil nil true))
  (is (match "foo" "foo" true)))

(deftest test-keywords
  (is (match (fn []) :fn true))
  (is (match 42 :int true))
  (is (match 7 :odd true))
  (is (match 66 :even true))
  (is (match "string" :str true))
  (is (match [(range 5)] :seq true))
  (is (match false :else true))
  (is (match [1 2 3]
             [4 5 6] false
             [_ _ 9] false
             :else true)))

(deftest test-symbols
  (is (match ["map" 1]
             [_ _] true))

  (is (= (match ["map" 1]
                _ true)
         nil))

  (is (match true x x))

  (is (match [true true]
             [x x] x))

  (is (match [:yes :no]
             [x x] false
             [x y] true))

  (is (let [x "foo"]
        (match "bar"
               x false
               y true))))

(deftest test-seqences
  (is (match [3] (:or 1 (inc 1) (+ 1 1 1)) true))
  (is (match nil (println :eval) true))
  (is (match [0 1] (range 2) true))
  (is (match (range 2) [0 1] true)))

(deftest test-regular-expressions
  (is (match "foo" #"fo+" true))
  (is (nil? (match "bar" #".{1,2}" true))))

(deftest test-numbers
  (is (match 5 5.0 true))
  (is (match 3.5 7/2 true))
  (is (match 2 10/5 true)))

(deftest test-functions
  (is (match 2 even? true))
  (is (match 55 #(zero? (mod % 5)) true))
  (is (match map (fn [f] (= f map)) true))
  (is (match :x (fn [x] {x true}) true))
  (is (match 0 0 (let [x true] x))))

(deftest test-references
  (is (match "ref" (atom "ref") true))
  (is (match "ref" (agent :str) true))
  (is (match 4 #'even? true))
  (is (match 5.0 (ref 5) true)))

(deftest test-sets
  (is (match "foo" #{:int :str} true))
  (is (match true #{true false} true)))

(deftest test-vectors
  (is (match ["foo" 1 map]
             [:str odd? :fn] true))

  (is (= [2 3 4] (match [1 2 3 4]
                      [x & xs] xs)))

  (is (= [1 2 3] (match [1 (inc 1) (+ 1 1 1)]
                        [a b c :as nums] nums)))

  (is (nil? (match [1 (inc 1) (+ 1 1 1)]
                   [a b :as nums] nums))))

(deftest test-maps
  (is (match {:foo "bar", :baz "tuna"}
             {:foo "bar" :baz :str} true))

  (is (match {:foo "bar", :baz "tuna", :some "more"}
             {:foo "bar" :baz :str} true))

  (is (nil? (match {:foo "bar", :baz "tuna", :some "more"}
                   {:only [:foo :baz]} true)))

  (is (match {:foo true, :baz "tuna"}
             {:foo x :baz :str} x)))

(deftest test-or
  (is (= (match 2 (:or 1 (inc 1) (+ 1 2))
                "1, 2 or 3") "1, 2 or 3"))

  (is (= (match 2 [:or 1 (inc 1) (+ 1 2)]
                "1, 2 or 3") "1, 2 or 3"))

  (is (= (match [1 (inc 1) 3]
                [4 _ :int] false
                [1 (+ 2 0) (:or 4 3 19)] true
                ["a" "b" "c"] false
                ["c" (prn 10) []] false)
         true)))

(deftest test-types
  (is (match "ooops"
             1 :1
             2 :2
             t true
             3 :3)))
