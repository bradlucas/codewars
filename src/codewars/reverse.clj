(ns codewars.reverse
   (:refer-clojure :exclude [reverse]))


(defn reverse
  [xs]
  (loop [acc []
         xs xs]
    (if (empty? xs)
      acc
      (recur (conj acc (last xs)) (drop-last xs)))))
