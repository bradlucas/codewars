(ns codewars.evaporator)

(defn sub-pct [val pct]
  (- val (* val (/ pct 100))))

(defn evaporator [content, evap_per_day, threshold]
  (let [pct evap_per_day
        limit threshold]
    (loop [val 100
           cnt 0]
      (if (< val limit)
        cnt
        (recur (sub-pct val pct) (inc cnt))))))


 (= (evaporator 10 10 10) 22)



