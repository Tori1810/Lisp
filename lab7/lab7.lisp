
(defparameter db '(("The Shawshank Redemption" "Frank Darabont" "1994" "9.197")
		   ("Forrest Gump" "Robert Zemeckis" "1994" "9.002")
		   ("The Green Mile" "Frank Darabont" "1999" "9.149")))

(defun add-item ()
  (let ((tmp-list ()))
    (format t "Enter name of film:")
    (setq tmp-list (append tmp-list (list (read-line))))
    (format t "Enter director of film:")
    (setq tmp-list (append tmp-list (list (read-line))))
    (format t "Enter year of film:")
    (setq tmp-list (append tmp-list (list (read-line))))
    (format t "Enter rating of film:")
    (setq tmp-list (append tmp-list (list (read-line))))
    (setq db (cons tmp-list db)))
  nil)

;==============================================================

(defun print-record (rec)
  (format t "Film: ~a~%" (car rec))
  (format t "Director: ~a~%" (cadr rec))
  (format t "Year: ~a~%" (caddr rec))
  (format t "Rating: ~a~%~%"(car (cdddr rec))))
  
(defun print-db-1 (db)
  (print-record (car db))
  (if (cdr db)
      (print-db-1 (cdr db))))

(defun print-db ()
  (print-db-1 db))

;==============================================================

(defun find-by-name-1 (name db)
  (if (equal name (caar db))
      (print-record (car db))
      (if (cdr db)
	  (find-by-name-1 name (cdr db))
	  nil)))

(defun find-by-name ()
  (let ((name nil))
    (format t "Enter Name of Film: ")
    (setq name (read-line))
    (find-by-name-1 name db)))

;==============================================================

(defun find-by-director-1 (director db)
  (if (equal director (cadar db))
      (print-record (car db))
      (if (cdr db)
	  (find-by-director-1 director (cdr db))
	  nil)))

(defun find-by-director ()
  (let ((director nil))
    (format t "Enter Director: ")
    (setq director (read-line))
    (find-by-director-1 director db)))

;=============================================================

(defun find-by-rating-1 (rating db)
  (if (equal rating (car (cdddar db)))
      (print-record (car db))
      (if (cdr db)
	  (find-by-rating-1 rating (cdr db))
	  nil)))

(defun find-by-rating ()
  (let ((rating nil))
    (format t "Enter rating: ")
    (setq rating (read-line))
    (find-by-rating-1 rating db)))

;============================================================

(defun delete-item-1 (name base lst)
  (if (not (string= name (caar base)))
      (progn
	(setq lst (append lst (list (car base))))
	(if (cdr base)
	    (delete-item-1 name (cdr base) lst)
	    lst))
      (if (cdr base)
	  (delete-item-1 name (cdr base) lst)
	  lst)))


(defun delete-item ()
  (let ((lst nil) (name nil))
    (format t "Enter name of film: ")
    (setq name (read-line))
    (setq db (delete-item-1 name db lst))
    (format t "-------------------------------------------~%~%")))

;============================================================

(defun compare-str (mask str)
  (if (char= (car mask) #\*)
      (if (cdr mask)
	  (compare-str (cdr mask) str)
	  t)
      (if mask
	  (if (not (null (member (car mask) str)))
	      (if (cdr mask)
		  (compare-str (cdr mask) (cdr (member (car mask) str)))
		  t)))))
      
(defun find-by-mask-1 (mask db)
  (if db
      (if (compare-str mask (coerce (write-to-string (caar db)) 'list))
	  (progn
	    (print-record (car db))
	    (find-by-mask-1 mask (cdr db)))
	  (if (cdr db)
	      (find-by-mask-1 mask (cdr db))))))


(defun find-by-mask ()
  (let ((mask nil))
    (format t "Enter mask: ")
    (setq mask (coerce (read-line) 'list))
    (find-by-mask-1 mask db)))
;============================================================

(defun menu ()
  (let ((command nil))
    (format t "~%List all: 1~%")
    (format t "Add new: 2~%")
    (format t "Delete film: 3~%")
    (format t "List film by mask: 4~%")
    (format t "Find by name: 5~%")
    (format t "Find by director: 6~%")
    (format t  "Find by rating: 7~%")
    (format t "Exit: 0~%")
    (format t "~%~%==============================================~%")
    (format t  "Enter command: ")
    (setq command (read))
    (format t "~%")
    (if (numberp command)
	(cond ((= command 1) (progn (print-db) (menu)))
	      ((= command 2) (progn (add-item) (menu)))
	      ((= command 3) (progn (delete-item) (menu)))
	      ((= command 4) (progn (find-by-mask) (menu)))
	      ((= command 5) (progn (find-by-name) (menu)))
	      ((= command 6) (progn (find-by-director) (menu)))
	      ((= command 7) (progn (find-by-rating) (menu)))
	      ((= command 0) nil)
	      (t (menu)))
	(progn
	  (format t "WRONG COMMAND!!!~%")
	  (menu)))))
