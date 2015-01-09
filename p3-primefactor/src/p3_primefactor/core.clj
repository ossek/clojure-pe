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

;;inclusive of n
(defn sieve
  [n]
  (if (< n 2) 
    []
    (loop [ [p & unmarked] (filter odd? (range 2 (inc n)))
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

(defn segmentedSieve
  [primesThrough]
  (do
   (if (< primesThrough 50)
    (sieve primesThrough)
    (let [segSize (int (math/floor (math/expt primesThrough 0.5)))
          baseprimes (segmentedSieve segSize) ]
      (do
       (loop [segStart (inc segSize)  
             result baseprimes ]  
        (do
          (if (> segStart primesThrough)
           result
           (if (< (- primesThrough segStart) segSize)
             (recur (+ segStart (inc segSize)) (into result (sieveSegment baseprimes segStart (- primesThrough segStart))))
             (do
              (recur (+ segStart (inc segSize)) (into result (sieveSegment baseprimes segStart segSize)))))))))))))
(sieve 70)
(segmentedSieve 70)
(sieve 444)
(segmentedSieve 444)
(sieve 99999)
(segmentedSieve 30000)
(segmentedSieve 99999)

;;cross off multiples of p starting from p^.  assuming that segment is odds only
(defn crossOffMultiples
  [p segment]
  (loop [check (* p p)
         crossed-segment segment]
    (if (<= check (last segment))
     (if (= (mod check p) 0)
       (recur (+ check 2) (into [] (remove (fn [x] (= x check)) crossed-segment)))
       (recur (+ check 2) crossed-segment))
     crossed-segment)))
(crossOffMultiples 5 (filter odd? (range 10 80)))

;;GC Error
(take 12 (range 99999999))


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

