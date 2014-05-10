(ns clojure-matching.core
  (:require [clojure.walk :as walk])
  (:import  java.util.regex.Pattern
            clojure.lang.IDeref))

(defn regex?
  "Returns true if x is a regular expression."
  [x] (instance? Pattern x))

(defn ideref?
  "Returns true if x can be dereferenced."
  [x] (instance? IDeref x))

(def hierarchy
  "Used as a hierarchy for the 'matcher'
  multimethod. Empty by default."
  (make-hierarchy))

(defn dispatch
  "Used as a dispatch function
  for the 'matcher' multimethod."
  [pattern]
  (condp apply [pattern]
    keyword?    ::keyword
    symbol?     ::symbol
    number?     ::number
    regex?      ::regex
    vector?     ::vec
    map?        ::map
    set?        ::set
    sequential? ::seq
    ideref?     ::deref
    ifn?        ::fn
    nil))

(defmulti matcher
  "Returns an appropriate matcher
  function for the given value."
  #'dispatch
  :hierarchy #'hierarchy)

(defn ->seq
  "Converts x to a sequence."
  [x] (if (sequential? x) x [x]))

(defn- flatcat
  "Flattens any kind of collection."
  [coll]
  (->> coll
       (tree-seq coll? seq)
       (remove coll?)))

(defn- glue
  "Glues together any number of maps.
  Returns nil if any of them is empty
  or duplicate keys with different
  values are present."
  ([] {})
  ([m] m)

  ([m1 m2]
   (when (and m1 m2)
     (reduce-kv
      (fn [m k v]
        (if (and (contains? m k)
                 (not= v (m k)))
          (reduced nil)
          (assoc m k v)))
      m1 m2)))

  ([m1 m2 & maps]
   (->> (list* m2 maps)
        (reduce glue m1))))

(defn- kw->pred
  "Converts the keyword kw to a predicate.
  Some keywords have special semantics.
  All others are tested for equality."
  [kw]
  (case kw
    :id    identity
    :fn    ifn?
    :kw    keyword?
    :sym   symbol?
    :str   string?
    :num   number?
    :int   integer?
    :float float?
    :rat   rational?
    :ratio ratio?
    :pos   #(and (number? %) (pos? %))
    :neg   #(and (number? %) (neg? %))
    :even  #(and (integer? %) (even? %))
    :odd   #(and (integer? %) (odd? %))
    :seq   sequential?
    :assoc associative?
    :list  list?
    :vec   vector?
    :map   map?
    :set   set?
    :else  (constantly true)
    (partial = kw)))

(defmethod matcher :default [x]
  #(when (= % x) {}))

(defmethod matcher ::keyword [kw]
  #(when ((kw->pred kw) %) {}))

(defmethod matcher ::symbol [sym]
  #(if (= '_ sym) {}
     {(keyword sym) %}))

(defmethod matcher ::regex [re]
  #(when (and (string? %)
              (re-matches re %))
     {}))

(defmethod matcher ::number [n]
  #(when (and (number? %)
              (== n %))
     {}))

(defmethod matcher ::deref [x]
  (matcher @x))

(defmethod matcher ::fn [f]
  #(when (f %) {}))

(defmethod matcher ::seq [s]
  (fn [value]
    (let [value (->seq value)]
      (when (= (count value)
               (count s))
        (->> (map matcher s)
             (conj () value)
             (apply map #(%1 %2))
             (apply glue))))))

(defmethod matcher ::vec [v]
  (let [[v {as :as}]
        (split-with (complement #{:as}) v)]
    (if as
      #(let [value (->seq %)]
         (glue ((matcher (vec v)) value)
               {(keyword as) value}))
      (let [[v {more '&}]
            (split-with (complement #{'&}) v)]
        (if more
          #(let [[head tail]
                 (split-at (count v) (->seq %))]
             (glue ((matcher more) tail)
                   ((matcher v) head)))
          (matcher v))))))

(defmethod matcher ::map [m]
  (let [{:keys [only as strs syms]
         ks :keys, ors :or} m]
    (cond
     only
     #(when (and (associative? %)
                 (= (set (keys %))
                    (set only)))
        ((matcher (dissoc m :only)) %))

     as
     #(glue ((matcher (dissoc m :as)) %)
            {(keyword as) %})

     :else
     (let [m (dissoc m :keys :strs :syms :or)]
       #(when (associative? %)
          (let [g     (partial get %)
                binds (->> (map (juxt (comp matcher val)
                                      (comp g key)) m)
                           (map (fn [[f x]] (f x)))
                           (apply glue))]
            (when binds
              (let [ks   (->> ks   (map keyword) (map g))
                    strs (->> strs (map str)     (map g))
                    syms (->> syms (map symbol)  (map g))]
                (merge ors ks strs syms binds)))))))))

(defmethod matcher ::set [s]
  (fn [value]
    (some #(% value)
          (map matcher s))))

(defn- free?
  "Returns true if sym is a free name,
  given the environment map env.
  '& and '_ are never considered free."
  [sym env]
  (not (or ('#{_ &} sym)
           (namespace sym)
           (some #{\.} (name sym))
           (sym env)
           (resolve sym))))

(defn- extract-free
  "Extracts all free names from coll,
  given the environment map env."
  [coll env]
  (->> (flatcat coll)
       (filter symbol?)
       distinct
       (filter #(free? % env))))

(defn- prepare
  "Prepares the pattern for matching."
  [pattern]
  (walk/prewalk
   #(if (and (sequential? %)
             (= :or (first %)))
      (-> % rest set) %)
   (->seq pattern)))

(defmacro match
  "Matches expr against the clauses.
  Returns nil if no match found."
  [expr & clauses]
  (when-let [[pattern result & clauses] clauses]
    (if (= :else pattern)
      result
      (let [env      (-> &env keys set)
            pattern  (prepare pattern)
            symbols  (extract-free pattern env)
            bindings (gensym)]
        `(let [val# ~expr]
           (if-let [~bindings (let [~@(mapcat #(list % (list `quote %))
                                              (conj symbols '_ '&))]
                                ((matcher (->seq ~pattern)) val#))]
             (let [~@(mapcat #(list % (list (keyword %) bindings))
                             symbols)]
               ~result)
             (match val# ~@clauses)))))))

(defmacro defmatch
  "Defines a function that implicitly
  uses 'match' on its arguments."
  [sym & clauses]
  (let [[attr-map [params & clauses]]
        (split-with (complement vector?) clauses)
        attr-map (map #(if (string? %) {:doc %} %) attr-map)
        sym      (apply vary-meta sym merge attr-map)]
    `(defn ~sym ~params
       (match ~params ~@clauses))))
