; https://projecteuler.net/problem=6

(defun sum-of-the-squares (&optional (start 1) (end 100))
  (loop for i from start to end summing (expt i 2)))

(defun square-of-the-sum (&optional (start 1) (end 100))
  (expt (loop for i from start to end summing i) 2))

(defvar *difference* (- (square-of-the-sum) (sum-of-the-squares)))