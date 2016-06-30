(cons 3 '())

(cons 1 nil)

(cons 1
      (cons 2
            (cons 3 nil)))

(defun sum (numbers)
  "Simple recursive sum"
  (if (eq nil numbers)
      0
    (+ (car numbers) (sum (cdr numbers)))))

(sum '())

(sum (cons 10 (cons 2 nil)))

(sum '(1 2 3 4 5))

(defun sum2-aux (numbers acc)
  "Auxiliar for better recursive sum with tail recursion"
  (if (eq nil numbers)
      acc
    (sum2-aux (cdr numbers) (+ acc (car numbers)))))

(defun sum2 (numbers)
  "Basic sum interface"
  (sum2-aux numbers 0))

(sum2 '(1 2 3 4 5))
