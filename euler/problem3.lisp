; https://projecteuler.net/problem=3

(defun is-prime (n)
  (when (> n 1)
    (loop for i from 2 to (isqrt n) never (zerop (mod n i)))))

;; 600851475143 because this is the value requested by Euler
(defun prime-factor-decomposition (&optional (n 600851475143))
  (let* ((ret nil)
         (factor 2)
         (value n))
        (loop while (<= factor value) dol
          (cond
            ((is-prime value) (return-from prime-factor-decomposition (reverse (push value ret))))
            ((integerp (/ value factor)) (push factor ret) (setf value (/ value factor)))
            (t (incf factor))))
        (return-from prime-factor-decomposition (reverse ret))))

(defun largest-factor (&optional (l (prime-factor-decomposition)))
  (last l))