(defun im:select-all-blocks ( )
	(cond
		((ssget "_X" '((0 . "INSERT"))))
		(T 
			(princ "\nDrawing does not contain any blocks.")
			nil
		)
	)
)

(defun im:select-blocks ( / s )
	(if (null (setq s (ssget '((0 . "INSERT")))))
		(princ "\nNo blocks were selected.")
	)
	
	s
)

(defun im:get-point (a / p)
	(if (null (setq p (getpoint a))) (exit))
	p
)

(defun im:get-points ( a / l p )
	(setq 
		l nil
		p (getpoint "\nSpecify first point: ")
	)
	
	(while (and (listp p) (not (null p)))
		(setq l (cons p l))
		
		(if (> (length l) 1)
			(grvecs (list 7 (cadr l) (car l)))
		)
		
		(initget a)
		(setq p (getpoint (car l) (strcat "\nSpecify next point or [" a "] <" a ">:")))
	)
	
	l
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
		
	(if (= a2 "") (setq a2 x))
	
	(cond 
		((> (atoi a2) 0) (setq x (itoa (1+ (atoi a2)))))
		((IsCharacter (chr (1+ (ascii a2)))) (setq x (chr (1+ (ascii a2)))))
	)
	
	a2
)

(princ)