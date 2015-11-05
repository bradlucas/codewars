(ns codewars.waiting-room)

;; http://www.codewars.com/kata/542f0c36d002f8cd8a0005e5/train/clojure

;; Chairs 	1 	2 	3 	4 	5 	6 	7 	8 	9 	10
;; Patients 	1 	7 	5 	8 	3 	9 	4 	6 	10 	2

;; Your task is to find last patient's chair's number. In this case it is 9.

(defn last-chair [n]
  {1 1, 2, 7, 3 5, 4 8, 5 3, 6 9, 7 4, 8 6, 9 10, 10 2}
)


(defn run
  [xs]
  (loop [acc {}
         xs xs
         cnt 1]
    (if (empty? xs) 
      acc
      (recur (conj acc {(first xs) cnt}) (reverse (rest xs)) (inc cnt))
      )))
