; https://projecteuler.net/problem=9

(defun pythagorean-triplet (&optional (n 1000))
  (loop for a from 1 to n do
    (loop for b from 1 to n do
      (when (< a b)
        (let ((c (- n (+ a b))))
        (when (>= c 1)
          (let ((pyt (= (+ (expt a 2) (expt b 2)) (expt c 2))))
          (when (and pyt (= (+ a b c) n)) (return-from pythagorean-triplet (* a b c))))))))))