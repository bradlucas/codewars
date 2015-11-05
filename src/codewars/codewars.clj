(ns codewars.codewars)
 
;; return the next item after v in a sequence s
(defn next-item [s v]
  (if (seq s)
    (if (= (first s) v)
      (first (rest s))
      (recur (rest s) v))))

;; What's up next?
(= (next-item (range 1 10000) 7) 8)
(= (next-item ["Joe" "Bob" "Sally"] "Bob") "Sally")

;; (deftest SampleTests
;;   (is (= (next-item (range 1 25) 12) 13))
;;   (is (= (next-item "testing" \t) \e))
;;   (is (nil? (next-item [:a :b :c] :d)))
;;   (is (nil? (next-item [:a :b :c] :c))))

(defn slope [[a b c d]]
  (let [x (- c a)
        y (- d b)]
    (if-not (zero? x)
      (str (/ y x))
      "undefined")))

;; (deftest Tests
;;   (is (= (slope [19,3,20,3]) "0"))
;;   (is (= (slope [-7,2,-7,4]) "undefined"))
;;   (is (= (slope [10,50,30,150]) "5"))
;;   (is (= (slope [15,45,12,60]) "-5"))
;;   (is (= (slope [10,20,20,80]) "6"))
;;   (is (= (slope [-10,6,-10,3]) "undefined")))

;; Series: 1 + 1/4 + 1/7 + 1/10 + 1/13 + 1/16 +.
;; 1, 4, 7, 10, 13, 16
(defn series-sum [n]
  (let [denominators (lazy-seq (cons 1 (iterate (partial + 3) 4)))]
    (format "%.2f" (float (reduce + (map #(+ (/ 1 %)) (take n denominators)))))))

(defn series-sum [n]
  (let [denominators (cons 1 (iterate (partial + 3) 4))
        val (reduce 
             (fn [x y] (+ (/ 1 x) (/ 1 y)))
             (take n denominators))]
    (format "%.2f" (float val))))

;; (ns nthseries.core-test
;;   (:require [clojure.test :refer :all]
;;             [nthseries.core :refer :all]))

;; (deftest a-test1
;;   (testing "Test 1"
;;     (is (= (series-sum 9) "1.77"))))
;; (deftest a-test2
;;   (testing "Test 2"
;;     (is (= (series-sum 0) "0.00"))))
;; (deftest a-test3
;;   (testing "Test 3"
;;     (is (= (series-sum 15) "1.94"))))


;; SeriesSum(1) => 1 = "1"
;; SeriesSum(2) => 1 + 1/4 = "1.25"
;; SeriesSum(5) => 1 + 1/4 + 1/7 + 1/10 + 1/13 = "1.57"


;; Playing with digits
;; Given a positive integer n written as abcd... (a, b, c, d... being
;; digits) and a positive integer p we want to find a positive integer
;; k, if it exists, such as the sum of the digits of n taken to the
;; successive powers of p is equal to k * n. In other words:
;;
;; Is there an integer k such as : (a ^ p + b ^ (p+1) + c ^(p+2) + d ^ (p+3) + ...) = n * k


;; convert number abcd -> [a b c d]
;; build expondent range for p -> [p p+1 p+2 p+3 ..]
;; build 

(defn dig-pow [n p]
  (letfn [(num-digits [num]
            (map #(Character/getNumericValue %) (seq (str num))))
          (exp-range [start num]
            (range start (+ start num)))
          (exp [x n]
            (loop [acc 1
                   n n]
              (if (zero? n)
                acc
                (recur (* x acc) (dec n)))))]
  (let [digits (num-digits n)
        exps (exp-range p (count digits))
        val-sum (reduce + (map #(exp %1 %2) digits exps))
        k (/ val-sum n)]
    (if (integer? k)
      k
      -1))))

;; 348597 => [7,9,5,8,4,3]
(defn digitize [num]
  (vec (reverse (map #(Character/getNumericValue %) (seq (str num))))))

;; another's example
(defn digitize [num]
  (->> num
       str
       seq
       (map #(Character/getNumericValue %))
       reverse
       vec))

(defn websites []
  (let [val "codewars"]
    (loop [acc []
           cnt 1000]
    (if (zero? cnt)
      acc
      (recur (conj acc val) (dec cnt))))))

;; (element-at [1,2,3] 2) ; => 2
;;(element-at "haskell" 5) ; => \e
(defn element-at [s num]
  (nth (seq s) (- num 1)))

(defn howManyLightsabersDoYouOwn 
  ([] 0)
  ([name] (if (= name "Zach") 18 0)))

