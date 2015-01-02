(ns p1.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;sum of all multiples of 3 or 5 below 1000
;;aka fizzbuzz
((fn 
  []
  (loop [i 0
         sum 0]
    (if (< i 1000) 
        (if (or (= (mod i 5) 0) (= (mod i 3) 0))
            (recur (inc i) (+ sum i) ) 
            (recur (inc i) sum))
        sum))) )

