(defun check-division (n k lst)
  (if (= k n)
      nil
      (if (/= (rem n k) 0)
	  (simple-div n (+ k 1) lst)
	  (setq lst (cons k lst)))))


(defun amount-div (n k acc)
  (if (= (rem n k) 0)
      (amount-div (/ n k) k (+ acc 1))
      acc))

(defun func-lab (n k lst)
  (if (= (rem n k) 0)
      (if (null (check-division k 2 ()))
	  (setq lst (cons (list k (amount-div n k 0)) lst))))
  lst)

(defun lab6-2 (n)
  (let ((result nil))
    (do ((i 2 (+ i 1)))
	((= i n) result)
      (setq result (func-lab n i result)))
  result))
