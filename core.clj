(ns combo.core
  (:require [clojure.set :as set]
            [clojure.pprint :as pprint]))

(comment '[count-combinations
           count-permutations
           count-subsets
           drop-permutations
           nth-combination
           nth-permutation
           nth-subset
           partitions
           permutation-index
           permuted-combinations
           selections])

(defn cartesian-product
  "Every unique pair of elements from c1 and c2"
  [c1 c2]
  (for [a c1 b c2] [a b]))

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

(defn combinations
  "Every combination of n items from coll"
  ([coll n]
   (->> (set coll)
        subsets
        (filter #(= n (count %))))))

(defn rotations
  [coll]
  (let [len (count coll)
        c (cycle coll)]
    (map (fn [offset]
           (take len (drop offset c)))
         (range len))))

(defn compare-seqs
  [s1 s2]
  (let [l1 (count s1)
        l2 (count s2)]
    (cond (< l1 l2) -1
          (> l1 l2) 1
          :else (loop [s1 s1, s2 s2]
                  (if (empty? s1)
                    0
                  (case (compare (first s1) (first s2))
                    -1 -1
                    1 1
                    0 (recur (rest s1) (rest s2))))))))

(declare permutation-indices)

(defn permutation-indices-raw
  [size]
  (case size
    0 []
    1 ['(0)]
    (let [index (dec size)
          tails (permutation-indices index)
          perms (mapcat (fn [tail]
                          (rotations (conj tail index)))
                        tails)]
      (sort compare-seqs perms))))

(def permutation-indices (memoize permutation-indices-raw))

(defn remap
  [coll indices]
  (if (not= (count indices) (count coll))
    (throw "Size mismatch")
    (let [v (vec coll)]
      (mapv #(nth v %) indices))))

(defn permutations
  [coll]
  (let [perms (permutation-indices (count coll))]
    (pprint perms)
    (map #(remap coll %) perms)))
