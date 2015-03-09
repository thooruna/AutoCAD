(defun sm:to-string ( x )
	(cond 
		((= (type x) 'INT) (setq x (itoa x)))
		((= (type x) 'REAL) (setq x (rtos x)))
		((= (type x) nil) "")
		(T x)
	)
)

(defun sm:string-length ( x )
	(strlen (sm:to-string x))
)

(defun sm:string-left-fill ( a1 a2 i )
	(if (null a1) 
		(setq a1 "")
	)
	
	(if (or (null a2) (= (strlen a2) 0)) 
		(setq a2 " ")
	)
	
	(while (<= (strlen a1) (- i 1))
		(setq a1 (strcat a2 a1))
	)
	
	a1
)

(defun sm:string-right-fill ( a1 a2 i )
	(if (null a1) 
		(setq a1 "")
	)
	
	(if (or (null a2) (= (strlen a2) 0)) 
		(setq a2 " ")
	)
	
	(while (<= (strlen a1) (- i 1))
		(setq a1 (strcat a1 a2))
	)
	
	a1
)

(defun sm:string-subst ( a1 a2 a3 / i1 i2 )
	(setq 
		i1 0
		i2 (strlen a1)
	)
	
	(while (setq i1 (vl-string-search a2 a3 i1))
		(setq a3 (vl-string-subst a1 a2 a3 i1)
			i1 (+ i1 i2)
		)
	)
	
	a3
)

(princ)