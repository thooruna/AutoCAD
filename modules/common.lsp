(defun cm:debug ( b )
	(if (setq *DEBUG* b)
		(cm:setvar "CMDECHO" 1)
		(cm:setvar "CMDECHO" 0)
	)
)

(defun cm:error ( a )
	(defun ShowVariables ( / x )
		(princ "\nVariables in memory:")
		
		(foreach x (lm:diff (atoms-family 0) *ATOMS*)
			(cond 
				((/= x '*ATOMS*)
					(princ "\n")
					(princ x)
					(princ " ")
					(princ (type (eval x)))
					(princ " ")
					(princ (eval x))
				)
			)
		)
		
		(princ "\n")
	)
	
	(if (not (member a '("Function Cancelled" "console break" "quit / exit abort")))
		(princ (strcat "\n; error: " a "\n"))
	)
	
	(if *DEBUG* (ShowVariables))
	
	(cm:done)
)

(defun cm:setvar ( a x )
	(cond
		((= a "DIMLDRBLK")
			(if (= x "")
				(setvar a ".")
				(setvar a x)
			)
		)
		((= a "DIMSTYLE")
			(setq *SETVAR* (cons (cons (strcase a) (getvar a)) *SETVAR*))
			(if (tblsearch "DIMSTYLE" x)
				(command "_.-DIMSTYLE" "_R" x)
				(command "_.-DIMSTYLE" "_S" x)
			)
		)
		((= a "OSNAP")
			(setq *SETVAR* (cons (cons "OSMODE" (getvar "OSMODE")) *SETVAR*))
			(if (wcmatch (sm:to-string x) "ON,1")
				(setvar "OSMODE" (BitCode (getvar "OSMODE") 16384 -1))
				(setvar "OSMODE" (BitCode (getvar "OSMODE") 16384 1))
			)
		)
		(T 
			(setq *SETVAR* (cons (cons (strcase a) (getvar a)) *SETVAR*))
			(setvar a x)
		)
	)
)

(defun cm:ini ( / a )
	(if (null *SETVAR*) 
		(command "_.UNDO" "BE")
	)
	
	(if *DEBUG* 
		(setq *ATOMS* (atoms-family 0))
	)
	
	(foreach a '("USERS1" "USERS2" "USERS3" "USERS4" "USERS5")
		(cm:setvar a "")
	)
	
	(setq
		*TEMP* *ERROR*
		*ERROR* cm:error
	)
)

(defun cm:done ( / d )
	(command "_.REDRAW")
	
	(foreach d *SETVAR*
		(cond 
			((= (car d) "DIMSTYLE") (command "_.-DIMSTYLE" "_R" (cdr d)))
			(T (setvar (car d) (cdr d)))
		)
	)
	
	(setq 
		*ATOMS* nil
		*DEBUG* nil
		*SETVAR* nil
		*ERROR* *TEMP*
	)
	
	(command "_.UNDO" "E")
	(command)
	
	(princ)
)

