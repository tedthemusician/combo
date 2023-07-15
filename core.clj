(ns combo.core
  (:require [clojure.set :as set]
            [clojure.pprint :as pprint]))

(comment '[cartesian-product
           combinations
           count-combinations
           count-permutations
           count-subsets
           drop-permutations
           nth-combination
           nth-permutation
           nth-subset
           partitions
           permutation-index
           permutations
           permuted-combinations
           selections])

(defn small-subsets
  "Assuming s has at least two elements, create all subsets of s whose sizes
  are at most half the size of the original set, rounding *up* for sets of odd
  size."
  [s]
  (let [half-length (Math/ceil (/ (count s) 2))
        small-sets (loop [smaller-sets #{#{}}
                          largest-sets #{#{}}]
                     (let [new-sets (mapcat (fn [elem]
                                              (map (fn [small-set]
                                                     (conj small-set elem))
                                                   largest-sets))
                                            s)
                           current-size (apply max (map count new-sets))
                           previous-sets (set/union smaller-sets largest-sets new-sets)]
                       (if (>= current-size half-length)
                         (set/union previous-sets new-sets)
                         (recur previous-sets new-sets))))]
    (set small-sets)))

(defn subsets
  "Get all subsets of s, including {} and s"
  [s]
  (let [size (count s)]
    (case size
      0 #{#{}}
      1 #{s}
      (let [smalls (small-subsets s)
            larges (set (map #(set/difference s %) smalls))]
        (set/union smalls larges)))))

(map subsets (map #() (range 3)))

; (map subsets (map set (map #(range %) (range 5))))

