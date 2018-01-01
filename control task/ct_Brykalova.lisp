(defstruct row
  val
  child)

; Task 1

(defun make-tree (lst)
  (make-row :val (car lst)
	    :child (mapcar #'fun (cdr lst))))
  
(defun fun (elem)
  (if (listp elem)
      (make-tree elem)
      (make-row :val elem
		:child nil)))

; Test: (make-tree '(+ 1 2 (* 3 4 5) (- 10 2)))


; Task 2
(defun print-tree (my-struct)
  (let ((my-list nil))
    (if (numberp (row-val my-struct))
	(setf my-list (append my-list (row-val my-struct)))
	(setf my-list (append (list (row-val my-struct)) (mapcar #'print-tree (row-child my-struct)))))))

; Test: (print-tree (make-tree '(+ 1 2 (* 3 4 5) (- 10 2))))


; Task 3

(defun pre-order (my-struct)
  (if (atom (row-child my-struct))
      (format t "~A " (row-val my-struct))
      (progn (format t "~A " (row-val my-struct))
	     (mapcar #'pre-order (row-child my-struct))))
nil)

; Test: (pre-order (make-tree '(+ 1 2 (* 3 4 5) (- 10 2))))


; Task 4

(defun post-order (my-struct)
  (if (atom (row-child my-struct))
      (format t "~A " (row-val my-struct))
      (progn (mapcar #'post-order (row-child my-struct))
	     (format t "~A " (row-val my-struct))))
nil)

; Test: (post-order (make-tree '(+ 1 2 (* 3 4 5) (- 10 2))))

; Task 5

(defun breadth (my-struct)
  (if (row-child my-struct)
      (progn
	(help my-struct)
	(mapcar #'help (row-child my-struct))
	(apply #'breadth (row-child my-struct)))
      ())
nil)
	

(defun help (my-struct)
  (format t "~A " (row-val my-struct)))












; Task 6

(defun map-tree (my-struct)
  (make-row :val (if (numberp (row-val my-struct))
		     (+ (row-val my-struct) 1)
		     (row-val my-struct))
	    :child (if (atom (row-child my-struct))
		       (row-child my-struct)
		       (mapcar #'map-tree (row-child my-struct)))))

; Test: (map-tree (make-tree '(+ 1 2 (* 3 4 5) (- 10 2))))


; Task 7

(defun calc (my-struct)
  (eval (print-tree my-struct)))

; Test: (calc (make-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
