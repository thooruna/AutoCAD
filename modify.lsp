;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun c:breakpoint ( / e aType p1 p2 p3 r s )
	(cm:initialize)
	
	(cond
		((setq e (car (setq s (entsel))))
			(cm:setvar "OSMODE" (mm:bitcode (getvar "OSMODE") 32 1))
			(cm:setvar "OSNAP" 1)
			
			(setq aType (em:type e)) ; Store type 
			
			(cond 
				((wcmatch aType "ARC,*LINE")
					(while (null (setq p1 (im:get-point "\nSpecify break point: "))))
					
					(cm:setvar "OSNAP" 0)
					
					(command "_.BREAK" s "_F" p1 "@") ; Entity e will not exist after this command
				)
				((wcmatch aType "CIRCLE,ELLIPSE")
					(setq 
						p1 (im:get-point "\nSpecify first break point: ")
						p2 (im:get-point "\nSpecify second break point: ")
						p3 (em:primary-point e) ; Center point of circle
						r (em:radius e) ; Radius of circle
					)
					
					(cm:setvar "OSNAP" 0)
					
					(command "_.BREAK" s "_F" p1 p2) ; Entity e will not exist after this command
					
					(if (= aType "CIRCLE")
						(command "_.ARC" "_C" p3 
							(polar p3 (angle p3 p1) r) 
							(polar p3 (angle p3 p2) r)
						)
					)
				)
				(T (princ "\nObject can't be broken."))
			)
		)
	)
	
	(cm:terminate)
)

(defun c:filletz ()
	(cm:initialize)
	
	(cm:setvar "CMDECHO" 1)
	(cm:setvar "FILLETRAD" 0)
	
	(command "_.FILLET" pause pause)
	
	(cm:terminate)
)





