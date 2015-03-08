(defun c:breakpoint ( / eObject eProperties eType pFirst pSecond pCenter)
	(em:ini)
	
	(setq eObject (entsel))
	(if eObject 
		(setq
			eProperties (entget (car eObject))
			eType (cdr (assoc 0 eProperties))
		)
	)
	
	(em:setvar "CMDECHO" 0)
	(em:setvar "OSMODE" (BitCode (getvar "OSMODE") 32 1))
	
	(OSnapOn)
	
	(cond 
		((null eType))
		((wcmatch eType "ARC,*LINE")
			(while (null (setq pFirst (_getpoint "\nSpecify break point: "))))
			
			(OSnapOff)
			
			(command "_.BREAK" eObject "_F" pFirst "@")
		)
		((wcmatch eType "CIRCLE,ELLIPSE")
			(setq 
				pFirst (_getpoint "\nSpecify first break point: ")
				pSecond (_getpoint "\nSpecify second break point: ")
				pCenter (cdr (assoc 10 eProperties))
				rRadius (cdr (assoc 40 eProperties))
			)
			
			(OSnapOff)
			
			(command "_.BREAK" eObject "_F" pFirst pSecond)
			(if (= eType "CIRCLE")
				(command "_.ARC" "_C" pCenter 
					(polar pCenter (angle pCenter pFirst) rRadius) 
					(polar pCenter (angle pCenter pSecond) rRadius)
				)
			)
		)
		(T (princ "\nObject can't be broken."))
	)
	
	(em:done)
)

(defun c:filletz ()
	(em:ini)
	
	(em:setvar "CMDECHO" 1)
	(em:setvar "FILLETRAD" 0)
	
	(command "_.FILLET" pause pause)
	
	(em:done)
)



