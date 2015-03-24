(defun c:breakpoint ( / eObject eProperties eType pFirst pSecond pCenter)
	(em:ini)
	
	(setq eObject (entsel))
	(if eObject 
		(setq
			eProperties (entget (car eObject))
			eType (cdr (assoc 0 eProperties))
		)
	)
	
	(em:setvar "OSMODE" (BitCode (getvar "OSMODE") 32 1))
	(em:setvar "OSNAP" 1)
	
	(cond 
		((null eType))
		((wcmatch eType "ARC,*LINE")
			(while (null (setq pFirst (im:get-point "\nSpecify break point: "))))
			
			(em:setvar "OSNAP" 0)
			
			(command "_.BREAK" eObject "_F" pFirst "@")
		)
		((wcmatch eType "CIRCLE,ELLIPSE")
			(setq 
				pFirst (im:get-point "\nSpecify first break point: ")
				pSecond (im:get-point "\nSpecify second break point: ")
				pCenter (cdr (assoc 10 eProperties))
				rRadius (cdr (assoc 40 eProperties))
			)
			
			(em:setvar "OSNAP" 0)
			
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





