(defun func-rec1 (elem k acc)
    (setq acc (cons elem acc))
    (if (> k 1)
	(func-rec1 elem (- k 1) acc)
	acc
	))

(defun func-rec2 (lst k acc)
  (if lst
      (func-rec2 (cdr lst) k (append acc (func-rec1 (car lst) k ())))
      acc))

(defun func (lst k)
  (let ((acc nil))
    (func-rec2 lst k acc)))

    

