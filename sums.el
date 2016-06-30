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

(sum2 (number-sequence 0 30))


(defun sum3 (numbers)
  "Using pattern matching"
  (pcase numbers
    (`nil 0)
    (`(,first . ,_) (+ first (sum3 (cdr numbers))))))


(sum3 '(1 2 3 4 5))


(defun apply-of-fun (fun val)
  (apply fun val))

;; Trying to understand how to pass functions as argument...
(apply-of-fun 'print '(1))


(defun reduce (fun acc elements)
  "Simple implementation of well-known form reduce"
  (pcase elements
    (`nil acc)
    (`(,first . ,_) (let ((elements-left (cdr elements))
                          (new-acc       (apply fun (list first acc))))
                      (reduce fun new-acc elements-left)))))

(defun add (a b)
  (+ a b))

(add 3 3)

(reduce 'add 0 '(3 3))
(reduce 'add 0 (number-sequence 1 8))


(defun reduce2 (fun acc elements)
  "Implementation of reduce as in the article"
  (pcase elements
    (`nil acc)
    (`(,first . ,_) (let ((elements-left (cdr elements)))
                      (let ((result (reduce fun acc elements-left)))
                           (apply fun (list first result)))))))

(reduce2 'add 0 '(3 3))
(reduce2 'add 0 (number-sequence 1 8))

(reduce2 'cons '(1) '(2 3 4 5 6))
(reduce 'cons '(1) '(2 3 4 5 6))
(reduce 'cons '(1 2) '(3 4))

(defun map (fun1 elements) ;; here the name has to be different `fun1' because ELisp confuses with the `fun' from the `reduce' implementation
  (let ((cons-apply (lambda (x y)
                      (cons (apply fun1 (list x)) y))))
    (nreverse (reduce cons-apply '() elements)))) ;; since `cons' add to beginning of list, nreverse turn the list into expected preserved order

(defun double (x)
  (* x x))

(double 2)

((lambda (x y) (cons (apply 'double (list x)) y)) 1 '())

(map 'double '(3 5 20))
(map 'double (number-sequence 2 6))
