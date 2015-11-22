;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun im:select-all-blocks|nested ( / s )
	(bm:search-blocks "*" (im:select-all-blocks))
)

(defun im:select-all-blocks|nested&filter ( a / s )
	(bm:search-blocks a (im:select-all-blocks))
)

(defun im:select-all-blocks ( / s )
	(cond
		((setq s (ssget "_X" '((0 . "INSERT")))))
		((princ "\nDrawing does not contain any blocks.") nil)
	)
	
	(lm:x->list s)
)

(defun im:select-blocks|filter ( a / s )
	(if (null (setq s (ssget  "_X" (list (cons 0 "INSERT") (cons 2 a)))))
		(princ "\nNo blocks were selected.")
	)
	
	(lm:x->list s)
)

(defun im:select-blocks ( / s )
	(if (null (setq s (ssget '((0 . "INSERT")))))
		(princ "\nNo blocks were selected.")
	)
	
	(lm:x->list s)
)

(defun im:select-block ( / s )
	(while 
		(and 
			(/= (em:type (setq e (car (entsel "\nSelect block reference: ")))) "INSERT") 
			(not (null e))
		)
	)
	
	e
)

(defun im:get-points ( x / l p )
	(setq x (lm:x->list x))
	
	(setq p (getpoint "\nSpecify first point: "))
	
	(while (= (type p) 'LIST)
		(setq l (cons p l))
		
		(if (> (length l) 1)
			(grvecs (list 7 (cadr l) (car l)))
		)
		
		(initget 128 (lm:list->string x " "))
		(setq p (getpoint (car l) (strcat "\nSpecify next point or [" (lm:list->string x "/") "] <" (car x) ">: ")))
	)
	
	(if (null p)
		(setvar "USERS1" (car x))
		(if (= (type p) 'STR) 
			(setvar "USERS1" p)
			(setvar "USERS1" "")
		)
	)
	
	l
)

(defun im:get-point ( a / p )
	(if (null (setq p (getpoint a))) (exit))
	p
)

(defun im:get-insertion-point ( / p )
	(setq p (getpoint "\nSpecify insertion point: "))
	
	(if (null p) 
		(setq p '(0 0 0))
		p
	)
)

(defun im:get-number ( a1 x / a2)
	(setq x (sm:to-string x))
	
	(initget 1)
	(setq a2 (getstring (strcat "\n"  a1 ": <" x ">: ")))
		
	(if (= a2 "") 
		(setq a2 x)
	)
	
	(cond 
		((> (atoi a2) 0) (setq x (itoa (1+ (atoi a2)))))
		;((sm:is-character (chr (1+ (ascii a2)))) (setq x (chr (1+ (ascii a2)))))
	)
	
	a2
)

(defun im:get-keyword ( a x )
	(setq x (lm:x->list x))
	
	(initget 128 (lm:list->string x " "))
	(cond ((getkword (strcat a " [" (lm:list->string x "/") "] <" (car x) ">: ")))((car x)))
)

(princ)