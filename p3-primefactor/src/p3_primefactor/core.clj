(ns p3-primefactor.core
  (:require [clojure.math.numeric-tower :as math]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(clojure.math.numeric-tower/expt 600851475143 0.5)
(clojure.math.numeric-tower/expt 200 0.5)

(defn checkSegment
  [primesUpToSqrtMax segmentStart segmentEnd]
  (loop [[prime & remain] primesUpToSqrtMax 
           final-segment (filter odd? (range segmentStart segmentEnd))]
      (if (nil? prime)
        final-segment
        (recur remain (crossOffMultiples prime final-segment)))))

(checkSegment [2,3,5,7] 3 45)

;;cross off multiples of p starting from p^.  assuming that segment is odds only
(defn crossOffMultiples
  [p segment]
  (loop [check (* p p)
         crossed-segment segment]
    (if (<= check (last segment))
     (if (= (mod check p) 0)
       (recur (+ check 2) (remove (fn [x] (= x check)) crossed-segment))
       (recur (+ check 2) crossed-segment))
     crossed-segment)))

(crossOffMultiples 5 (filter odd? (range 10 80)))

;;GC Error
(take 12 (range 99999999))

;;there may be some lazy 
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

(defn divBy 
  [x y]
  (= (mod x y) 0))

(defn sieve
  [n]
  (if (< n 2) 
    []
    (loop [ [p & unmarked] (filter odd? (rest (rest (range n))))
           final-unmarked () ]
      (do
        (if (or (nil? p))
          final-unmarked
          (recur (unmarkedFactors p unmarked)  (conj final-unmarked p)))))))

(sieve 13059)
(sieve 13999)
(take 12 (sieve 139999))
(take 12 (sieve 999999))

(take 12 (sieve 9999999))
(take 12 (sieve 600851475))
(take 12 (sieve 600851475143))

(take 12 (largestPrimeFactor 600851475143))
(take 12 (largestPrimeFactor 60085147514))


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

