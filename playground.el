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
    (`(,first . ,elements-left)
     (let ((new-acc (apply fun (list first acc))))
       (reduce fun new-acc elements-left)))))

(defun add (a b)
  (+ a b))

(add 3 3)

(reduce 'add 0 '(3 3))
(eq (reduce 'add 0 (number-sequence 1 8))
    36)

(defun reduce2 (fun acc elements)
  "Implementation of reduce as in the article"
  (pcase elements
    (`nil acc)
    (`(,first . ,elements-left)
     (let ((result (reduce2 fun acc elements-left)))
       (apply fun (list first result))))))

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

(list 'node 1 (cons (list 'node 2 nil)
               (cons (list 'node 3 nil)
                     nil)))

(print (symbol-function (car (read-from-string "+"))))

(apply (symbol-function (car (read-from-string "+"))) '(1 2))

(quote quote)

(apply (quote add) (quote (1 2)))


(defmacro from-seq (binding-names source-seq &rest body)
  (let ((head-var (car binding-names))
        (tail-var (car (cdr binding-names))))
    `(let ((,head-var (car ,source-seq))
           (,tail-var (cdr ,source-seq)))
       ,@body)))

(print (macroexpand '(from-seq (head tail) '(1 2 3)
                                 (print head))))

(from-seq (head tail) '(1 2 3)
          (progn (print head)
                 (print tail)))


(defun reduce-new (fun acc elements)
  "reduce using new macro"
  (if (eq nil elements)
      acc
    (from-seq (head tail) elements
              (let ((new-acc (apply fun (list head acc))))
                (reduce-new fun new-acc tail)))))

(reduce-new 'add 0 (number-sequence 1 8))

(print (cdr (assoc 'name '((name . "Rafael") (abc . 1)))))

(defmacro node (label child)
  `(list 'node ,label ,child))

(print (macroexpand '(node 1 (cons (node 2 nil) nil))))

(node 1 (cons (node 2 nil)
              (cons (node 3 nil)
                    nil)))

(defun right-child (tree-node)
  (caaddr tree-node))

(defun left-child (tree-node)
  (cadadr (cdr tree-node)))

(right-child (node 1 nil))

(right-child (node 2 (cons (node 3 nil) nil)))

(right-child (node 2 (cons (node 3 nil)
                           (cons (node 2 nil) nil))))

(left-child (node 2 (cons (node 3 nil)
                           (cons (node 2 nil) nil))))

(defun binary-tree (label &optional base-tree)
  (if base-tree
      (let ((root-label       (cadr base-tree))
            (right-child-node (right-child base-tree))
            (left-child-node  (left-child base-tree)))
        (if (> label root-label)
            (node root-label
                  (list
                   (binary-tree label right-child-node) left-child-node))
            (node root-label
                  (list
                   right-child-node (binary-tree label left-child-node)))))
      (node label nil)))

(binary-tree 1)

(binary-tree 2 (binary-tree 1))

(binary-tree 1 (binary-tree 2))

(binary-tree 3 (binary-tree 1 (binary-tree 2)))

(print (binary-tree 4 (binary-tree 3 (binary-tree 1 (binary-tree 2)))))

(print (binary-tree 4 (binary-tree 5)))

(print (binary-tree 3 (binary-tree 2 (binary-tree 1 (binary-tree 4 (binary-tree 5))))))

(print (binary-tree 9 (binary-tree 11 (binary-tree 5 (binary-tree 7 (binary-tree 10 (binary-tree 6 (binary-tree 8))))))))

(print (reduce-new 'binary-tree nil '(8 6 10 7 5 9 11)))


(defmacro partial-apply (orig-fun-ref &optional partial-arg-list)
  "Applies partially the arguments to a function.

Limitations:
- Does not support lambda functions as orig-fun-ref;
- May mess arguments with orig-fun if using pattern _i[0-9]+ for argument name in orig-fun (dynamic context)."
  (let* ((orig-fun (symbol-function (cadr orig-fun-ref)))
         (orig-fun-arity (length (cadr orig-fun)))
         (provided-args (cadr partial-arg-list))
         (n-provided-args (length provided-args))
         (n-missing-args (- orig-fun-arity n-provided-args))
         (missing-args (mapcar 'make-symbol
                               (mapcar (lambda (i) (concat "_i" (int-to-string i)))
                                       (number-sequence 1 n-missing-args))))
         (final-call-args (cons 'list (append provided-args missing-args))))
    `(lambda ,missing-args (apply ,orig-fun ,final-call-args))))

(print (macroexpand
        (partial-apply 'add '(1))))

(partial-apply 'add '(1))

(apply (partial-apply 'add '(1)) '(1))

(funcall (partial-apply 'add '(1)) 1)

(funcall (symbol-function 'add) 1 2)

(add 2 3)
