
(defparameter R1 '(R 25))
(defparameter R2 '(R 110))
(defparameter R3 '(R 56))
(defparameter R4 '(R 100))
(defparameter L1 '(L 20))
(defparameter C1 '(C 0.000002))
(defparameter C2 '(C 0.000005))

(defun consistent (param1 param2)
  (+ param1 param2))

(defun parallel (param1 param2)
  (/ (* param1 param2) (+ param1 param2)))

(defun choice (element frequency)
  (cond
    ((equal (car element) 'R)
     (cadr element))

    ((equal (car element) 'L)
     (* #c(0 1) frequency (cadr element)))

    ((equal (car element) 'C)
     (/ 1 (* #c(0 1) frequency (cadr element))))))

(defun result (frequency)
  (consistent
   (consistent
    (parallel
     (consistent
      (choice R1 frequency)
      (choice R2 frequency))
     (consistent
      (choice R4 frequency)
      (choice L1 frequency)))
    (choice C2 frequency))
   (parallel
    (choice C1 frequency)
    (choice R3 frequency))))
