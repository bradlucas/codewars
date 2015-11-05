(ns codewars.nthseries)

;; http://www.codewars.com/kata/sum-of-the-first-nth-term-of-series/train/clojure

;; Your task is to write a function which returns the sum of following series upto nth term(parameter).
;; 
;; Series: 1 + 1/4 + 1/7 + 1/10 + 1/13 + 1/16 +...


;; You need to round the answer upto 2 decimal places and return it as String.
;; If the given value is 0 then it should return 0.00
;; You will only be given Natural Numbers as arguments.

;; failed because it is slow
(defn series-sum [n]
  (let [denominators (lazy-seq (cons 1 (iterate (partial + 3) 4)))]
    (format "%.2f" (float (reduce + (map #(+ (/ 1 %)) (take n denominators)))))))


(defn format-dec [n]
  (format "%.2f" (float n)))

(defn series-sum [n]
  (if (= 0 n) (format-dec 0)
      (let [denominators (iterate (partial + 3) 4)]
        (format-dec (+ 1 (reduce + (map #(+ (/ 1 %)) (take (dec n) denominators))))))))

(= (series-sum 1) "0.00")
(= (series-sum 1) "1")
(= (series-sum 2) "1.25")
(= (series-sum 5) "1.57")


