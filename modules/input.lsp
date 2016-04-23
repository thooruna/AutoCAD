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

(defun im:select-all-tables ( / s )
	(cond
		((setq s (ssget "_X" '((0 . "ACAD_TABLE")))))
		((princ "\nDrawing does not contain any tables.") nil)
	)
	
	(lm:x->list s)
)

(defun im:select-all-tables|current-tab ( / s )
	(cond
		((setq s (ssget "_X" (list (cons 0 "ACAD_TABLE") (cons 410 (getvar "CTAB"))))))
		((princ "\nCurrent tab does not contain any tables.") nil)
	)
	
	(lm:x->list s)
)

(defun im:select-tables( / s )
	(if (null (setq s (ssget '((0 . "ACAD_TABLE")))))
		(princ "\nNo tables were selected.")
	)
	
	(lm:x->list s)
)

(defun im:select-table ( / s )
	(while 
		(and 
			(/= (em:type (setq e (car (entsel "\nSelect table: ")))) "ACAD_TABLE") 
			(not (null e))
		)
	)
	
	e
)

(defun im:get-points-with-keywords ( aDefault xOptions / l p )
	(setq xOptions (lm:x->list xOptions))
	
	(initget 129)
	(setq p (getpoint (strcat "\nSpecify first point" (if aDefault (strcat " or <" aDefault ">:") ":"))))
	
	(cond 
		((and aDefault (null p)) -1)
		(T
			(while (= (type p) 'LIST)
				(setq l (cons p l))
		
				(if (> (length l) 1)
					(grvecs (list (cm:layer-active-color) (cadr l) (car l)))
				)
				
				(initget 128 (lm:list->string xOptions " "))
				(setq p (getpoint (car l) (strcat "\nSpecify next point or [" (lm:list->string xOptions "/") "] <" (car xOptions) ">: ")))
			)
			
			(if (null p)
				(setvar "USERS1" (car xOptions))
				(setvar "USERS1" (if (= (type p) 'STR) p ""))
			)
			
			l
		)
	)
)

(defun im:get-points ( / l p )
	(setq p (im:get-point "Specify first point:"))
	
	(while (= (type p) 'LIST)
		(setq l (cons p l))
		(if (> (length l) 1)
			(grvecs (list (cm:layer-active-color) (cadr l) (car l)))
		)
		
		(setq p (getpoint (car l) "\nSpecify next point: "))
	)
	
	l
)

(defun im:get-point ( a / p )
	(setq p (getpoint (strcat "\n" (if a a "Specify point:"))))
	
	p
)

(defun im:get-insertion-point ( / p )
	(setq p (im:get-point "Specify insertion point:"))
	
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

(defun im:get-keyword ( a aDefault x )
	(setq x (lm:x->list x))
	
	(setq aDefault (if aDefault aDefault (car x)))
	
	(initget 128 (lm:list->string x " "))
	(cond ((getkword (strcat a " [" (lm:list->string x "/") "] <" aDefault ">: ")))(aDefault))
)

(princ)