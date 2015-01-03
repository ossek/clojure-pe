(ns p3-primefactor.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn intsUpTo
  [n]
  (loop [i 0
         res []]
    (if (= i n)
    res
    (recur (inc i) (conj res i)))
    ))

(filter odd? (intsUpTo 20))

(defn divBy 
  [x y]
  (= (mod x y) 0))

(divBy 5 2)
(divBy 32 2)

(defn sieve
  [n]
  (if (< n 2) 
    []
    (loop [p (first (rest (rest (intsUpTo n))))
           unmarked (rest (rest (intsUpTo n))) ]
      (if (>= p n)
        unmarked
        (recur (first unmarked) unmarked)))))

(sieve 4)

(defn unmarkedFactors
  [unmarked]
    (filter (fn [x] (not= (mod x (first unmarked)) 0)) (rest unmarked))
    )

(unmarkedFactors 
 (unmarkedFactors 
    (unmarkedFactors (rest (rest (intsUpTo 20)) )) ) )


;;(filter (fn [x] (not= (mod x 3) 0) ) (rest (intsUpTo 10) )) 
;;(filter (fn [x] (= (mod x 3) 0) ) (rest (intsUpTo 10) )) 

