(ns p3-primefactor.core
  (:require [clojure.math.numeric-tower :as math]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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

(defn divByex
  [x y]
  (> (/ x y) 1))

;;inclusive of n
(defn sieve
  [n]
  (if (< n 2) 
    []
    (loop [ [p & unmarked] (filter (fn [x] (divByex x 7)) 
                                     (filter (fn [x] (divByex x 5)) 
                                               (filter (fn [x] (divByex x 3)) 
                                                         (filter odd? (range 2 (inc n))))))
           final-unmarked [] ]
      (do
        (if (nil? p)
          final-unmarked
          (recur (unmarkedFactors p unmarked)  (conj final-unmarked p)))))))

;;segment includes the segmentSize - segmentStart element.  eg if the
;; values are 3 4 respectively, includes 3 4 5 6 
(defn sieveSegment
  [primesUpToSqrtMax segmentStart segmentSize]
   (loop [[prime & remain] primesUpToSqrtMax 
          final-segment (into [] (filter odd? (range segmentStart (+ segmentStart segmentSize))))]
      (if (nil? prime)
        final-segment
        (recur remain (crossOffMultiples prime final-segment)))))

(sieveSegment [3,5,7] 8 7.0)
(sieveSegment [3,5,7] 18 7.0)
(sieveSegment [3,5,7] 27 7.0)
(sieveSegment [3,5,7] 36 7.0)

(defn segmentedSieve
  [primesThrough]
  (do
   (if (< primesThrough 50)
    (sieve primesThrough)
    (let [segSize (int (math/floor (math/expt primesThrough 0.5)))
          baseprimes (segmentedSieve segSize) ]
      (do
        (println (str "baseprimes " baseprimes))
       (loop [segStart (inc segSize)  
             result baseprimes ]  
        (do
          (if (> segStart primesThrough)
           result
           (if (< (- primesThrough segStart) segSize)
             (recur (+ segStart (inc segSize)) (into result (sieveSegment baseprimes segStart (- primesThrough segStart))))
             (do
               (println "baseprimes " baseprimes " segStart " segStart  " segSize " segSize)
              (recur (+ segStart (inc segSize)) (into result (sieveSegment baseprimes segStart segSize)))))))))))))

(segmentedSieve 60)
(sieve 70)
(segmentedSieve 70)
(sieve 444)
(segmentedSieve 444)
(sieve 99999)
(segmentedSieve 30000)
(segmentedSieve 99999)

;;cross off multiples of p starting from p^2.  assuming that segment is odds only
(defn crossOffMultiples
  [p segment]
  ;;the p^2 
  (loop [crossed-segment segment]

    )
  )

;;(defn crossOffMultiples
;;  [p segmentStart segmentEnd]
;;  (loop [check (* p p) 
;;         crossed-segment []]
;;    (if (<= check segmentEnd)
;;     (if (= (mod check p) 0)
;;       (recur (+ check 2) crossed-segment)
;;       (recur (+ check 2) (conj crossed-segment check)))
;;     crossed-segment)))
;;(crossOffMultiples 5 10 80)

;;GC Error
(take 12 (range 99999999))


;; it should be that to find the largestprimefactor of n we only have to get prime factors up to n/2.
;; Suppose that there are only 2 factors of n besides n,1, and these factors are 2 and n/2.
;; suppose that n/2 is prime.  for there to be a larger prime factor of n, we would have to have some other 
;; factor smaller than 2.  The only available is 1.  In that case the other factor is n, and n is prime.
;; Suppose that n/2 is not prime.  This means it is divisible by prime factors < n/2, so the largest prime 
;; factor of n is still < n/2.

;; for this case either 600851475143 is prime, or the largest prime factor <= n/2.  If we test all
;; primes <= n/2, and n is divisble by none of them, then n is prime
(defn largestPrimeFactor ;;test all primes <= n/2, if none are factors, n is prime
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

(math/floor (/ 600851475143 2.0))
(largestPrimeFactor 600851475143)
(largestPrimeFactor 19873)

