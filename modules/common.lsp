;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright � 2015

(defun cm:version ()
	(cond
		((= (getvar "ACADVER") "") nil)
		((getvar "ACADVER"))
	)
)

(defun cm:debug ( b )
	(if (setq *DEBUG* b)
		(cm:setvar "CMDECHO" 1)
		(cm:setvar "CMDECHO" 0)
	)
)

(defun cm:error ( a )
	(defun ShowVariables ( / x )
		(princ "\nVariables in memory:")
		
		(foreach x (lm:difference (atoms-family 0) *ATOMS*)
			(cond 
				((and (/= x 'a) (/= x '*ATOMS*))
					(princ "\n")
					(princ x)
					(princ " ")
					(princ (type (eval x)))
					(princ " ")
					(princ (eval x))
				)
			)
		)
		
		(terpri)
	)
	
	(if (not (member a '("Function Cancelled" "console break" "quit / exit abort")))
		(princ (strcat "\n; error: " a "\n"))
	)
	
	(if *DEBUG* 
		(ShowVariables)
	)
	
	(cm:terminate)
)

(defun cm:getvar ( a )
	(cdr (assoc a *SETVAR*))
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
				(setvar "OSMODE" (mm:bitcode (getvar "OSMODE") 16384 -1))
				(setvar "OSMODE" (mm:bitcode (getvar "OSMODE") 16384 1))
			)
		)
		(T 
			(setq *SETVAR* (cons (cons (strcase a) (getvar a)) *SETVAR*))
			(setvar a x)
		)
	)
)

(defun cm:initialize ( / a i )
	(setq i (cm:echo-off))
	(if (and (null *SETVAR*) (cm:version)) (command-s "_.UNDO" "BE"))
	(cm:echo-set i)
	
	(if *DEBUG* 
		(setq *ATOMS* (atoms-family 0))
	)
	
	(foreach a '("USERS1" "USERS2" "USERS3" "USERS4" "USERS5")
		(cm:setvar a "")
	)
	
	(setq
		*DATE* (getvar "DATE")
		*TEMP* *ERROR*
		*ERROR* cm:error
	)
)

(defun cm:terminate ( / d i )
	(terpri)
	
	(foreach d *SETVAR*
		(cond 
			((= (car d) "DIMSTYLE") (command "_.-DIMSTYLE" "_R" (cdr d)))
			(T (setvar (car d) (cdr d)))
		)
	)
	
	(if *DEBUG* 
		(princ (strcat "\nTotal time elapsed: " (rtos (* (- (getvar "DATE") *DATE*) 60 60 24) 2 2) " s\n"))
	)
	
	(setq 
		*ATOMS* nil
		*DATE* nil
		*DEBUG* nil
		*SETVAR* nil
		*ERROR* *TEMP*
	)
	
	(setq i (cm:echo-off))
	(command-s "_.REDRAW")
	(command-s "_.REGEN")
	(if (cm:version) (command-s "_.UNDO" "E"))
	(cm:echo-set i)
	
	(princ)
)

;;; Turn command echo off and return the initial value

(defun cm:echo-off ( / i )
	(setq i (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)
	i
)

;;; Set command echo to value

(defun cm:echo-set ( i )
	(setvar "CMDECHO" i)
)

;;; Layer functions

(defun cm:layer-exists ( a )
	(tblsearch "LAYER" a)
)

(defun cm:layer-new ( a i aLinetype )
	(if (null (cm:layer-exists a)) (command-s "_.-LAYER" "_N" a ""))
	(cm:layer-color a i)
	(if aLinetype (cm:layer-linetype a aLinetype))
	
	a
)

(defun cm:layer-color ( a i )
	(command-s "_.-LAYER" "_C" i a "")
)

(defun cm:layer-linetype ( a aLinetype )
	(if (not (tblsearch "LTYPE" aLinetype))
		(command-s "_.-LINETYPE" "_L" aLinetype (findfile "acadiso.lin") "")
	)
	
	(command-s "_.-LAYER" "_L" aLinetype a "")
)

(defun cm:layer-make ( a )
	(if (null (cm:layer-exists a)) (command-s "_.-LAYER" "_M" a ""))
)

(defun cm:layer-activate ( a )
	(if (cm:layer-exists a)
		(command-s "_.-LAYER" "_T" a "_ON" a "")
		(cm:layer-make a)
	)
	
	(cm:setvar "CLAYER" a)
)

(defun cm:layer-active ( )
	(tblsearch "LAYER" (getvar "CLAYER"))
)

(defun cm:layer-active-linetype ( )
	(em:linetype (cm:layer-active))
)

(defun cm:layer-active-color ( )
	(em:color (cm:layer-active))
)

(defun cm:layer-change ( e a / l )
	(if (/= (em:layer (setq l (entget e))) a)
		(if (entmod (subst (cons '8 a) (assoc 8 l) l))
			(entupd e)
		)
	)
)

(princ)