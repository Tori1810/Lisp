
(defparameter graf '( (K (M P) (0 1))
		     (M (C P) (2 5))
		     (P (M) (4))
		     (C (P) (3))))

(defun count-vertex (lst count)
    (dolist (x lst)
      (setq count (+ count 1)))
    count)


(defun degree (graf node)
  (let ((count 0))
    (dolist (elem graf)
      (if (equal node (car elem))
	  (setq count (count-vertex (cadr elem) count))))	
    count))


(defun delete-node (graf vertex)
  (let ((new-graf nil))
    (dolist (x graf)
      (if (equal (car x) vertex)
	  ()
	  (setq new-graf (append new-graf (list x)))))
    new-graf))


(defun nodes-list-help (graf count vertex new-list)
  (dolist (x graf)
    (if (<= count (degree graf (car x)))
	(progn
	  (setq count (degree graf (car x)))
	  (setq vertex (car x)))
	()))
  (setq graf (delete-node graf vertex))
  (setq new-list (append new-list (list (list vertex count))))
  (if graf
      (nodes-list-help graf 0 nil new-list)
      new-list))


(defun nodes-list (graf)
  (let ((graf-copy graf) (count 0) (vertex nil) (new-list nil))
    (setq new-list (nodes-list-help graf-copy count vertex new-list))
    new-list))


(defun get-node (node graf &optional res)
  (dolist (x graf)
    (if (equalp (car x) node)
	(setq res x)))
  res)


(defun depth-first (graf start-node finish-node &optional path (acc 0) min-cost curr res)
  (setq curr (get-node start-node graf))
  (cond
    ((and (equalp (car curr) finish-node) (or (not min-cost) (< acc min-cost)))
     (setq res (list (append path (list (car curr))) acc)))
    ((or (equalp (car curr) finish-node) (not (cadr curr)) (member (car curr) path))
     res)
    (t     (mapc (lambda (node cost)
	     (setq res (depth-first graf node finish-node (append path (list (car curr))) (+ acc cost) min-cost curr res))
	     (when (or (not min-cost) (< min-cost (cadr res)))
	       (setq min-cost (cadr res))))
	   (cadr curr) (caddr curr))))
  res)


(defun breadth-first (graf start-node finish-node &optional (lst (list (list (car (get-node start-node graf)) nil 0))) min-cost res curr)
  (setq curr (get-node start-node graf))
  (cond
    ((and (equalp (car curr) finish-node) (or (not min-cost) (< (caddar lst) min-cost)))
     (setq min-cost (caddar lst))
     (setq res (list (append (cadar lst) (list (car curr))) (caddar lst))))
    ((or (equalp (car curr) finish-node) (not (cadr curr)) (member (car curr) (cadar lst)))
     res)
    (t
     (mapc (lambda (node cost)
	     (setq lst (append lst (list (list node (append (cadar lst) (list start-node)) (+ (caddar lst) cost))))))
	   (cadr curr) (caddr curr))))
  (setq lst (cdr lst))
  (if lst
      (breadth-first graf (caar lst) finish-node lst min-cost res)
      res))

 
