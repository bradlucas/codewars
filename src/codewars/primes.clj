(ns codewars.primes)

;; Backwards Read Primes are primes that when read backwards in base
;; 10 (from right to left) are a different prime. (This rules out
;; primes which are palindromes.)
;;
;; 13 is such because it's prime and read from right to left writes 31 which is prime too.
;;
;; Find all Backwards Read Primes between two positive given numbers
;; (both inclusive), the second one being greater than the first one.
;; The resulting array or the resulting string will be ordered
;; following the natural order of the prime numbers.


(defn primeA? 
  "If no numbers less than n divide evenly into n then return true"
  [n]
  (if (even? n)
    false
    (empty? (filter #(= 0 (mod n %)) (range 3 n)))))

(defn primeB? 
  "http://swizec.com/blog/checking-for-primes-dumber-algorithm-is-faster-algorithm/swizec/1580"
  [n]
  (if (even? n) false
      (let [root (num (int (Math/sqrt n)))]
	(loop [i 3]
	  (if (> i root) true
	      (if (zero? (mod n i)) false
		  (recur (+ i 2))))))))
(defn primeC? [n]
  (if (even? n) false
      (let [root (num (int (Math/sqrt n)))]
	(loop [i 3]
	  (if (> i root) true
	      (if (zero? (mod n i)) false
		  (recur (+ i 2))))))))

(defn primeD? 
  "python example
https://github.com/staticor/Leetcode/blob/master/codewars/backwards-read-primes.py

def is_prime(n):
    if n in (2, 3, 5): return True
    if n % 2 == 0 or n % 3 == 0: return False
    return all(n % i for i in xrange(5, int(n**.5)+1, 2))
  "
  [n]
  (if (some #(= n %) [2 3 5])
    true
    (if (or (= 0 (mod 2 n)) (= 0 (mod 3 n)))
      false
      (empty? (filter #(= 0 (mod n %)) (range 5 n 2))))))

(defn primeE? 
  "http://swizec.com/blog/checking-for-primes-dumber-algorithm-is-faster-algorithm/swizec/1580"
  ([n] (primeE? n [2]))
  ([n known] 
   (letfn [(any? [l]
             (reduce #(or %1 %2) l))]
     (loop [cnt (dec (count known)) acc []]
       (if (< cnt 0) (not (any? acc))
           (recur (dec cnt) (concat acc [(zero? (mod n (nth known cnt)))])))))))


;; testing speed
(defn test-prime? 
  [n]
  (let [fns [#(primeA? n) #(primeB? n) #(primeC? n) #(primeD? n) #(primeE? n)]]
    (doseq [f fns] (time (println (f))))))


;; ----------------------------------------------------------------------------------------------------
(def prime? primeB?)

(defn reverse-number
  "Return the reverse of a number. For example, 123 => 321"
  [n]
  (Integer. (clojure.string/reverse (str n))))

(defn reverse-number 
  "http://pramode.net/clojure/2010/05/06/project-euler-palindrome-clojure/"
  [n]
  (java.math.BigInteger. (apply str (reverse (str n)))))

(defn reverse-prime?
  "If n is prime and the reverse of n is not only different but also prime"
  [n]
  (if (prime? n)
    (let [reversed (reverse-number n)]
      (if (= n reversed)
        false
        (prime? reversed)))
    false))

;; ----------------------------------------------------------------------------------------------------

(defn backwards-prime
  [from to]
  (filter reverse-prime? (range from (+ to 1))))


;; ----------------------------------------------------------------------------------------------------
(= (backwards-prime 2 100) [13, 17, 31, 37, 71, 73, 79, 97]) 
(= (backwards-prime 9900 10000) [9923, 9931, 9941, 9967])
(= (backwards-prime 7000 7100) [7027, 7043, 7057])
(= (backwards-prime 1095000 1095403) [1095047 1095209 1095319 1095403]) 
(= (backwards-prime 109500 109700) [109537 109579 109583 109609 109663])
(= (backwards-prime 70000 70245) [70001 70009 70061 70079 70121 70141 70163 70241])
(= (backwards-prime 70485 70600) [70489 70529 70573 70589])
(= (backwards-prime 109500 109700) [109537 109579 109583 109609 109663])
