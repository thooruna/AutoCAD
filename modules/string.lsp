(defun sm:string-left-fill ( a b i )
	(if (null a) 
		(setq a "")
	)
	
	(if (or (null b) (= (strlen b) 0)) 
		(setq b " ")
	)
	
	(while (<= (strlen a) (- i 1))
		(setq a (strcat b a))
	)
	
	a
)

(defun sm:string-right-fill ( a b i )
	(if (null a) 
		(setq a "")
	)
	
	(if (or (null b) (= (strlen b) 0)) 
		(setq b " ")
	)
	
	(while (<= (strlen a) (- i 1))
		(setq a (strcat a b))
	)
	
	a
)