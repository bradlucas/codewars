(ns codewars.ean-validator)


;; The digit at the first, third, fifth, etc. position (i.e. at the odd position) has to be multiplied with "1".
;; The digit at the second, fourth, sixth, etc. position (i.e. at the even position) has to be multiplied with "3".
;; Sum these results.
;; If this sum is dividable by 10, the checksum is 0. Otherwise the checksum has the following formula:
;;
;; checksum = 10 - (sum mod 10)




;; user=> (take-nth 2 [0 1 2 3 4 5 6 7 8 9])
;; (0 2 4 6 8)
;; user=> (take-nth 2 (rest [0 1 2 3 4 5 6 7 8 9]))
;; (1 3 5 7 9)

;; http://www.codewars.com/kata/ean-validation/train/clojure

(defn digits [num]
  (map #(Character/digit % 10) (str num)))

(defn validate-ean [ean-code]
  (let [digits (digits ean-code)]
    (let [odds (take 6 (take-nth 2 digits))
          evens (map #(* 3 %) (take 6 (take-nth 2 (rest digits))))
          checksum (last digits)
          sum (+ (reduce + odds) (reduce + evens))
          cs (mod sum 10)]
      (println odds)
      (println evens)
      (println checksum)
      (println sum)
      (println cs)
      (if (zero? cs)
        (= checksum 0)
        (= checksum (- 10 cs))))))

;; 4003301018398


(= (validate-ean "4003301018398") true)
(= (validate-ean "4003301018392") false)
(= (validate-ean "9783827317100") false)

