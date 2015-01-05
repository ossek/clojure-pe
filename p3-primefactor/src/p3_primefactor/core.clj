(ns p3-primefactor.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(filter odd? (intsUpTo 20))


(divBy 5 2)
(divBy 32 2)

(defn intsUpTo
  [n]
  (loop [i 0
         res []]
    (if (= i n)
    res
    (recur (inc i) (conj res i)))
    ))

;;GC Error so far
(take 12 (intsUpTo 600851475143))

;;seems this must be a vector returned, or else there may be some lazy 
;;evaluation issues if the result of this is used in many recursions.
;; possibly related: http://www.ksmpartners.com/2014/01/clojure-lazy-seq-and-the-stackoverflowexception/
;;http://stackoverflow.com/questions/1662336/clojure-simple-factorial-causes-stack-overflow
;;http://stackoverflow.com/questions/5294055/why-am-i-getting-a-stackoverflow
;;https://groups.google.com/forum/?fromgroups=#!topicsearchin/clojure/lazy$20seqs$20overflow$20the$20stack$3F/clojure/-d8m7ooa4c8
(defn unmarkedFactors
  [p unmarked]
    (into [] (filter (fn [x] (not= (mod x p) 0)) unmarked))
    )

(unmarkedFactors 3 [3 4 5 6 7 8 9])

(defn sieve
  [n]
  (if (< n 2) 
    []
    (loop [ [p & unmarked] (into [] (filter odd? (rest (rest (intsUpTo n)))))
           final-unmarked [] ]
      (do
;;        (println (str " p " p " unmarked " unmarked) )
        (if (or (nil? p))
        (do
;;          (println "end")
          final-unmarked)
        (do
;;          (println (str "p " p " unmarked " unmarked " final-unmarked " final-unmarked))
          (recur (unmarkedFactors p unmarked)  (conj final-unmarked p))))))))

;;(sieve 13059)
(sieve 13059)
(sieve 13999)
(sieve 139999)
(println (take 12 (sieve 600851475)))
(take 12 (sieve 600851475143))

(take 12 (largestPrimeFactor 600851475143))
(take 12 (largestPrimeFactor 60085147514))

(defn divBy 
  [x y]
  (= (mod x y) 0))

(defn largestPrimeFactor
  [n]
  (if (= n 2)
    2
    (loop [[first & restPrimesLessThanN] (sieve n)
           currentMax 0 ]
      (if (nil? restPrimesLessThanN)
        currentMax
        (if (and (divBy n first) (> first currentMax))
          (recur restPrimesLessThanN first)
          (recur restPrimesLessThanN currentMax))))))

(largestPrimeFactor 600851475143)
(largestPrimeFactor 19873)
(largestPrimeFactor 29843)

(unmarkedFactors 2 [3])

( (fn 
    [[p & others]]
    (do
      (println (str "p " p " and others " others )))) [] )

(unmarkedFactors 4 [3 4 5 6 7 8 9 10 11 12])

(unmarkedFactors (rest (rest (intsUpTo 10))))

(unmarkedFactors 
 (unmarkedFactors 
    (unmarkedFactors (rest (rest (intsUpTo 20)) )) ) )


(filter (fn [x] (not= (mod x 3) 0) ) (rest (intsUpTo 10) )) 
(filter (fn [x] (= (mod x 3) 0) ) (rest (intsUpTo 10) )) 

