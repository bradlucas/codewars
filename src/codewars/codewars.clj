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




;; ----------------------------------------------------------------------------------------------------
;; http://www.codewars.com/kata/localize-the-barycenter-of-a-triangle/train/clojure
;; (ns barycenter.core)

(defn bar-triang 
  [[a b] [c d] [e f]]
  (let [x (/ (+ a c e) 3)
        y (/ (+ b d f) 3)]
    [(Double. (format "%.4f" (float x))) (Double. (format "%.4f"  (float y)))]))


;; ----------------------------------------------------------------------------------------------------
;; http://www.codewars.com/kata/number-pairs/train/clojure
;; (ns number-pairs)
;; (= arr1 [13, 64, 15, 17, 88])
;; (= arr2 [23, 14, 53, 17, 80])
;; (= (getLargerNumbers arr1 arr2) [23 64 53 17 88])

(defn get-larger-numbers [a b]
  (mapv #(apply max [%1 %2]) a b))


;; ----------------------------------------------------------------------------------------------------
;; http://www.codewars.com/kata/mergesort-merge-function/train/clojure

;; (deftest Tests
;;   (is (= (mergesorted [1 2] [3]) (range 1 4)))
;;   (is (= (mergesorted [1 2] [3 4]) (range 1 5)))
;;   (is (= (mergesorted [1] [2 3 4]) (range 1 5)))
;;   (is (= (mergesorted [] [1 2 3 4]) (range 1 5)))
;;   (is (= (mergesorted [1 2 3 4] []) (range 1 5))))

(defn mergesorted
  [[a_x & a_xs :as a] [b_x & b_xs :as b]]
  (cond
    (nil? a_x) b
    (nil? b_x) a
    :else (if (< a_x b_x)
            (cons a_x (mergesorted a_xs b))
            (cons b_x (mergesorted a b_xs)))))


;; ----------------------------------------------------------------------------------------------------
;;  http://www.codewars.com/kata/560a4962c0cc5c2a16000068/train/clojure

;; (deftest a-test1
;;   (testing "Basic tests"    
;;     (is (= (eq-sum-pow-dig 100 2) []))
;;     (is (= (eq-sum-pow-dig 1000 2) []))
;;     (is (= (eq-sum-pow-dig 2000 2) []))
;;     (is (= (eq-sum-pow-dig 200 3) [153]))
;;     (is (= (eq-sum-pow-dig 370 3) [153 370]))
;;     ))

;; this 1st solution is too slow for final test
(defn digits [num]
  (map #(Character/getNumericValue %) (seq (str num))))

(defn power [x n] (reduce * (repeat n x)))

(defn sum-pow-same [num po]
  (let [digits (digits num)
        total (apply + (map #(power % po) digits))]
    (= total num)))

(defn eq-sum-pow-dig [hmax po]
  (filter #(sum-pow-same % po) (range 2 (inc hmax))))

;; 2nd version
(defn eq-sum-pow-dig [num po]
  (letfn [(power [x n] (reduce * (repeat n x)))
          (testfn [num po] (= (apply + (map #(power (Character/getNumericValue %) po) (seq (str num)))) num))]
    (filter #(testfn % po) (range 2 (inc num)))))

;; 3rd version
;; Precalculate powers of digits into a map
(defn build-single-digit-power-map 
  "Build a map of powers for the digits 0 to 9
  For example, for the power 2
  {0 0, 7 49, 1 1, 4 16, 6 36, 3 9, 2 4, 9 81, 5 25, 8 64}"
  [po]
  (let [digits (map #(Integer/valueOf %) (range 1 10))]
    (letfn [(power [x n] (reduce * (repeat n x)))]
      (loop [acc {0 0}
             nums digits]
        (if (empty? nums) 
          acc
          (let [num (first nums)
                pow (power num po)]
            (recur (assoc acc num pow) (rest nums))))))))

(defn sum-powers 
  "Using our powers map from 'build-single-digit-power-map' sum the digits raised to the power.
  - Convert the number to a series of digits
  - Use the digits to key into the map
  - Add the results"
  [powers num] 
  (let [digits (map #(Integer/valueOf (str %)) (seq (str num)))]
    (apply + (map #(powers %) digits))))

(defn eq-sum-pow-dig [num po]
  (let [powers (build-single-digit-power-map  po)]
    (filter #(= (sum-powers powers %) %) (range 2 (inc num)))))


;; ----------------------------------------------------------------------------------------------------
;;
;; (let [fives (factory 5)]      ; returns a function - fives
;;   (fives [1 2 3]))   

(defn factory [x]
  (fn [xs] (map #(* x %) xs)))


;; ----------------------------------------------------------------------------------------------------
;; 7 kyu Excel sheet column numbers

;; (defn title-to-nb [title]
  ; your code)

;; (deftest a-test1
;;   (testing "Basic tests"
;;     (is (= (title-to-nb "A") 1))
;;     (is (= (title-to-nb "Z") 26))
;;     (is (= (title-to-nb "AA") 27))
;;     (is (= (title-to-nb "AZ") 52))
;;     (is (= (title-to-nb "BA") 53))
;;     (is (= (title-to-nb "CODEWARS") 28779382963))    
;; ))


(defn twenty-six-power [n]
  (reduce * (repeat n 26)))

(defn letter-value 
  "A == 1, ... Z == 26"
  [c]
  (- (int c) 64))

(defn title-to-nb 
  [title]
  ;; reverse
  ;; recurse and get value of each letter (1-26) * 26 ^ n
  ;; where n is the position in the string from right to left
  ;; the first position is 0
  (let [s (reverse title)]
    (loop [s (reverse title)
           acc 0
           n 0]
      (if (empty? s)
        acc
        (recur (rest s) (+ acc (* (letter-value (first s)) (twenty-six-power n))) (inc n))))))


;; ----------------------------------------------------------------------------------------------------
;; 7 - Growth of a Population

;; population
;; percent change
;; other additions
;; desired total
;; new-population = existing-total + (existing-total * percent) + aug

(defn nb-year [p0 percent aug p]
  (let [percent (/ percent 100)
        additions aug
        desired-total p]
    (loop [population p0
           number-of-years 0]
      (if (>= population desired-total)
        number-of-years
        (let [new-population (+ (+ population (* percent population)) aug)]
          (recur new-population (inc number-of-years)))))))

;; ----------------------------------------------------------------------------------------------------
;; 7 kyu - A Rule of Divisibility by 13

;; When you divide the successive powers of 10 by 13 you get the following remainders of the integer divisions:
;; 1, 10, 9, 12, 3, 4.

(defn mult-list [] (cycle [1 10 9 12 3 4]))

(defn digitize [num] (vec (reverse (map #(Character/getNumericValue %) (seq (str num))))))

(defn thirtfn [num] (apply + (map #(* %1 %2) (digitize num) (mult-list))))

(defn thirt
  [num]
  (loop [num num]
    (let [next (thirtfn num)]
      (if (= next num)
        num
        (recur next)))))


;; ----------------------------------------------------------------------------------------------------
;; 
;; 6 kyu - Highest Rank Number in an Array

;; [12 10 8 12 7 6 4 10 12]

;; (val (first (reverse (sort-by #(val %) (seq (frequencies [12 10 8 12 7 6 4 10 12 11 11 11]))))))

(defn highest-rank 
  "Returns the most frequent entry in the data ISeq"
  [data]
  (let [ranks (reverse (sort-by #(val %) (seq (frequencies data))))
        max-val (val (first ranks))
        ]
    (apply max (map first (filter (fn [[x y]] (= max-val y)) ranks)))
    )
  )

;; ----------------------------------------------------------------------------------------------------
;;

;; a = [121, 144, 19, 161, 19, 144, 19, 11] 
;; b = [11*11, 121*121, 144*144, 19*19, 161*161, 19*19, 144*144, 19*19]

;; elements in b are squares of elements in a


(defn compSame [a b]
  (if (and (seq a) (seq b))
    (let [as (into #{} (map (fn [x] (* x x)) a))
          bs (into #{} b)]
      (= as bs))
    (if (and (empty? a) (empty? b))
      (if (and (not (nil? a)) (not (nil? b)))
        true
        false)
      false
      )
    )
  )


(compSame 
[121, 144, 19, 161, 19, 144, 19, 11]  
[132, 14641, 20736, 361, 25921, 361, 20736, 361])
(compSame 
 [121, 144, 19, 161, 19, 144, 19, 11]  
 [121, 14641, 20736, 36100, 25921, 361, 20736, 361])
(compSame [] [1])
(compSame [] [])
(compSame [] nil)
(compSame [121, 144, 19, 161, 19, 144, 19, 11] [121, 14641, 20736, 361, 25921, 361, 20736, 361])
