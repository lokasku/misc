; https://projecteuler.net/problem=7

(defun fibonacci (&optional (limit 4e6))
  (let ((ret nil))
    (do ((b 1 a)
         (a 1 (incf a b)))
        ((>= a limit)
         (return-from fibonacci
          (reverse ret)))
        (push a ret))))

(defun sum-fibonacci (&optional (fs (fibonacci)))
  (reduce #'+ (remove-if #'oddp fs)))