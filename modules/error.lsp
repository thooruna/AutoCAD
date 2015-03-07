(defun em:debug ( b )
	(setq *debug* b)
)

(defun em:error ( a )
	(defun ShowVariables ( / x )
		(princ "\nVariables in memory:")
		(foreach x (lm:diff (atoms-family 0) *atoms*) 
			(princ "\n")
			(princ x)
			(princ " ")
			(princ (type (eval x)))
		)
	)
	
	(if *debug* (ShowVariables))
	
	(if (not (member a '("Function Cancelled" "console break" "quit / exit abort")))
		(princ (strcat "\n; error: " a))
	)
	
	(em:done)
)

(defun em:setvar ( a x )
	(setq *setvar* (cons (cons (strcase a) (getvar a)) *setvar*))
	(setvar a x)
)

(defun em:ini ( )
	(if (null *setvar*) (command "_.UNDO" "BE"))
	
	(if *debug* (setq *atoms* (atoms-family 0)))
	
	(setq
		*temp* *error*
		*error* em:error
	)
)

(defun em:done ( / d )
	(command "_.REDRAW")
	
	(foreach d *setvar*
		(cond 
			((= (car d) "DIMSTYLE") (command "_.-DIMSTYLE" "_R" (cdr d)))
			(T (setvar (car d) (cdr d)))
		)
	)
	
	(setq 
		*atoms* nil
		*debug* nil
		*setvar* nil
		*error* *temp*
	)
	
	(command)
	(command "_.UNDO" "E")
	(princ)
)

