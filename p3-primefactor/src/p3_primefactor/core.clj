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
  (and (= (mod x y) 0) (> (/ x y) 1)))

;;------------Trying incremental / generator approach
;;maintain a map in a closure.  keys are discovered primes, plus the next multiple of that prime that will be encountered.
;;when incrementing i, check the list's values for match.  For each discovered match, increment it by the key.  If no match, 
;;add i as a key, i+i as a value, and return i   

;;93169
(last (take 9999 (map :previousPrime (iterate nextPrime {:previousPrime 2 :primesToNextCompositeSeq [{:prime 2 :nextMultiple 4}]} ))))
(take 6 (iterate nextPrime {:previousPrime 2 :primesToNextCompositeSeq [{:prime 2 :nextMultiple 4}]} ))

;; we to end up with a function that takes the last prime, and gives us the next one, to use with iterator function.
;;return the mapping of primes to their next composites, either with any matched 
;; next composites incremented, or with the prime added
(defn nextPrime
  [primesToNextCompositeSeq_previousPrime]
  (loop [candidate (inc (:previousPrime primesToNextCompositeSeq_previousPrime))
         ptn_pp primesToNextCompositeSeq_previousPrime] 
          (if (containsNextMultipleValue 
                (:primesToNextCompositeSeq primesToNextCompositeSeq_previousPrime) 
                candidate)
            (recur 
              (inc candidate) 
              {:previousPrime (:previousPrime ptn_pp) 
                :primesToNextCompositeSeq (findAndUpdateNextCompositeByPrimeIfMatch (:primesToNextCompositeSeq ptn_pp) candidate)})
            {:previousPrime candidate
             :primesToNextCompositeSeq (conj 
                                         (:primesToNextCompositeSeq ptn_pp) 
                                         {:prime candidate :nextMultiple (+ candidate candidate)})}))))
(nextPrime {:previousPrime 3 
            :primesToNextCompositeSeq [{:prime 2 
                                        :nextMultiple 4} 
                                       {:prime 3 
                                        :nextMultiple 6}]})
(nextPrime 
  (nextPrime {:previousPrime 2 
              :primesToNextCompositeSeq [{:prime 2 :nextMultiple 4}]}))


;;check if any of the next composite values in a  list of prime-to-next-composite mappings 
;;matches findval
(defn containsNextMultipleValue
  [primesToNextCompositeSeq findval]
  (some (fn [primeToNextComposite]
          (= (:nextMultiple primeToNextComposite) findval)) primesToNextCompositeSeq))
;
(containsNextMultipleValue [{:prime 3 :nextMultiple 6} {:prime 2 :nextMultiple 4}] 4)

;; try having a sorted map m of nextcomposite to the prime(s) that it is a multiple of.
;; for a candidate c, 
;; at each iteration, check the first (least) entry of m. 
;; if 
;; then for each of the prime increments
;; either add the prime increment to existing entry or add a new one
(defn next-prime
  [iterdata]
  (let [previous-prime (:previous-prime iterdata)
        composites->prime-increments (:composites->prime-increments iterdata)]
  (loop [candidate (inc previous-prime)
         final-composites->prime-increments composites->prime-increments] 
    (let [first-composite->prime-increments (first final-composites->prime-increments)
          next-composite (key first-composite->prime-increments) ]
     (if (= candidate next-composite) ;;candidate is a composite if found
       (let [rest-composite->prime-increments (dissoc final-composites->prime-increments next-composite)]
         (recur (inc candidate) (update-composites-from-composites first-composite->prime-increments rest-composite->prime-increments)))
       (let [updated (update-composites-from-prime final-composites->prime-increments candidate)]
         {:composites->prime-increments updated :previous-prime candidate}))) )))
(take 30 (map :previous-prime (iterate next-prime {:previous-prime 2 :composites->prime-increments (sorted-map 4 [2])} )))
(take 7 (iterate next-prime {:previous-prime 2 :composites->prime-increments (sorted-map 4 [2])} ))

(first(sorted-map 4 [2]))
(rest(sorted-map 4 [2]))

;;takes a composite -> primes.
;; increment this composite by primes, and update or add the newly incremented values 
;; as keys pointing to the primes they were incremented by.
(defn update-composites-from-composites
  [composite->prime-increments composites->prime-increments]
  (let [composite-to-increment (key composite->prime-increments)
        prime-increments (val composite->prime-increments)]
    (reduce (fn [c->p-i p-i] 
              (update-composites c->p-i composite-to-increment p-i)) 
            composites->prime-increments 
            prime-increments)))
(update-composites-from-composites (first (sorted-map 4 [2,3])) (sorted-map 6 [3 4] 8 [2 5]) )
(update-composites-from-composites (first (sorted-map 4 [2,3])) (sorted-map 8 [2 5]) )

(defn update-composites
  [composites->prime-increments src by-prime]
  (let [new-composite (+ src by-prime)]
    (if (contains? composites->prime-increments new-composite)
      (assoc composites->prime-increments new-composite (conj (get composites->prime-increments new-composite) by-prime))
      (assoc composites->prime-increments new-composite [by-prime]))))

;;update upcoming composite or insert with the prime it will be incremented 
;;when next discovered.
(defn update-composites-from-prime
  [composites->prime-increments prime]
  (update-composites composites->prime-increments prime prime))

(update-composites-from-prime (sorted-map) 2)
(update-composites-from-prime (sorted-map 4 [1]) 2)


(conj (get (sorted-map 4 '(1)) 4) 3)

(sorted-map)

(get {3 [6 3] 4 [8]} 3)
(first (sorted-map 3 [6 3] 4 [8]))
(last (sorted-map 3 [6 3] 4 [8]))
{"3"  "4" 8}

;;update a nextComposite by prime if matches findval, otherwise just return the datastructure untransformed 
(defn updateNextCompositeByPrimeIfMatch
  [primeToNextComposite findval]
    (if (= (:nextMultiple primeToNextComposite) findval)
                 (update-in primeToNextComposite [:nextMultiple] ;;update it
                   (fn [nextMultiple]
                     (+ nextMultiple (:prime primeToNextComposite))))
                   primeToNextComposite))
;
(updateNextCompositeByPrimeIfMatch {:prime 3 :nextMultiple 6} 4)
(updateNextCompositeByPrimeIfMatch {:prime 3 :nextMultiple 6} 6)

;;go through sequence of prime to nextComposite pair and update the nextComposite by prime if 
;;it matches findval
(defn findAndUpdateNextCompositeByPrimeIfMatch
  [primeToNextCompositeSeq findval] 
  (map (fn [primeToNextComposite]
         (updateNextCompositeByPrimeIfMatch primeToNextComposite findval)) primeToNextCompositeSeq))
;
(findAndUpdateNextCompositeByPrimeIfMatch [{:prime 2 :nextMultiple 4} {:prime 3 :nextMultiple 6}] 4)
(findAndUpdateNextCompositeByPrimeIfMatch [{:prime 2 :nextMultiple 6} {:prime 3 :nextMultiple 6} {:prime 5 :nextMultiple 10}] 6)


;;------try an iterator that checks by division to exclude certain next values
(defn nextNotMultipleOf
  [factorsToCheck]
  (fn [lastNonMultiple] 
    (loop [candidate (inc lastNonMultiple)]
    (if (some (fn [factor] (divByex candidate factor)) factorsToCheck)
      (recur (inc candidate))
      candidate))))
;;lazy-seq that gives us any number not a multiple of the numbers passed in the vector
(take 70 (iterate (nextNotMultipleOf [2,3,5,7,11]) 0))

;;generate a range excluding the multiples specified (rather than filtering them out after)
(defn rangeExcludeMultiples
  ([factors max]
   (take-while (fn [x] (< x max)) (iterate (nextNotMultipleOf factors) 0)))
  ([factors start max]
   (take-while (fn [x] (< x max)) (iterate (nextNotMultipleOf factors) start))))
(rangeExcludeMultiples [2,3,5,7,11] 70)
;;this pegs for many minutes
(rangeExcludeMultiples [2,3,5,7,11] 99999999)
;;this doesn't
(rangeExcludeMultiples [2,3,5,7,11] 9999999)
(rangeExcludeMultiples [2,3,5,7,11] 5 70)

;;inclusive of n
(defn sieve
  [n]
  (if (< n 2) 
    []
    (loop [ [p & unmarked] (rangeExcludeMultiples [2,3,5,7,11] 2 (inc n))
           final-unmarked [] ]
      (do
        (if (nil? p)
          final-unmarked
          (recur (unmarkedFactors p unmarked)  (conj final-unmarked p)))))))
;99991
(last (sieve 99999))

;;GC Error
(take 12 (range 99999999))



;;-------------assuming I can get a sieve working, then check by trial division against the found primes.
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

