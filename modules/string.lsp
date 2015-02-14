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

(defun sm:string-subst ( new old str / inc len )
    (setq len (strlen new)
          inc 0
    )
    (while (setq inc (vl-string-search old str inc))
        (setq str (vl-string-subst new old str inc)
              inc (+ inc len)
        )
    )
    str
)