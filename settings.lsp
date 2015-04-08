;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun c:snapang ( / r s x )
	(defun SetSnapang ( r )
		(princ (strcat "\nCurrent setting: SNAPANG = " (rtos (mm:radians->degrees (setvar "SNAPANG" r)))))
	)
	
	(cm:initialize)
	
	(setq r (getvar "SNAPANG"))
	
	(cond
		((= (getvar "SNAPANG") 0.0)
			(initget "O o")
			(setq x (getreal (strcat "\nEnter new value for SNAPANG or [Object] <" (rtos (mm:degrees->radians r)) ">: ")))
			
			(cond
				((= (sm:string-uppercase x) "O")
					(cond
						((wcmatch (em:type (car (setq s (nentsel)))) "*LINE,VERTEX,LEADER")
							(cm:setvar "APERTURE" (getvar "PICKBOX"))
							
							(if (setq r (angle (osnap (cadr s) "_NEA") (osnap (cadr s) "_MID")))
								(SetSnapang r)
							)
						)
						((not (null s)) (princ "\nInvalid object."))
					)
				)
				((not (null x))(SetSnapang x))
			)
		)
		(T (SetSnapang 0))
	)
	
	(cm:terminate)
)





