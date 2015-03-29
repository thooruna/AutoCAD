;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun c:snapang ( / )
	(cm:initialize)
	
	(if (= (getvar "SNAPANG") 0.0)
		(if (setq e (car (entsel "Please choose an object or [Enter]: ")))
			(if (= (em:type e) "LINE")
				(setvar "SNAPANG" (em:line-angle e))
				(princ "\nNot a line...")
			)
			(command "_SNAPANG" pause)
		)
		(setvar "SNAPANG" 0)
	)
	
	(cm:terminate)
)





