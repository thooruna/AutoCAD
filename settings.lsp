(defun c:snapang ( / a b e )
	(cm:ini)
	
	(if (= (getvar "SNAPANG") 0.0)
		(progn
			(setq e (entsel "Please choose an object or [Enter]: "))
			(if (= e nil)
				(command "_SNAPANG" pause)
			(progn
				(setq elist (entget (car e)))
					(if (= (cdr (assoc 0 elist)) "LINE")
						(progn
							(setq
								a (cdr (assoc 10 elist))
								b (cdr (assoc 11 elist))
							)
							(setvar "SNAPANG" (angle a b))
						)
						(princ "\nNot a line...")
					)
				)
			)
		)
		(setvar "SNAPANG" 0)
	)
	
	(cm:done)
)





