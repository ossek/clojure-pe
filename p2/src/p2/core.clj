(ns p2.core
  (:gen-class))

;;even fibonacci numbers.  find the sum of even-valued terms of the
;;fibonacci sequence,such that the term does not exceed 4 million.
(defn fib
  [termMaxValue
   termLast
   termCurrent
   resultVec]
    (if (>= termMaxValue termCurrent)
      (fib termMaxValue termCurrent (+ termLast termCurrent) (conj resultVec (+ termLast termCurrent)))
      resultVec) )
(filter even? (fib 10 0 1 [0 1]) )
(reduce + (filter even? (fib 10 0 1 [0 1]) ))


;;sln
(reduce + (filter even? (fib 4000000 0 1 [0 1]) ))

