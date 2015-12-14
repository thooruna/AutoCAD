;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun xm:add-data ( x lData / lXData )
	(setq x (if x (em:edd x) (em:edd (entlast))))
	
	(regapp (car lData))
	
	(setq lXData
		(list
			(list -3
				(cons
					(car lData) ; App name
					(mapcar
						'(lambda (a)
							(cons
								1000 
								(if a a "")
							)
						)
						(cdr lData)
					) ; App data
				)
			)
		)
	)
	
	(entmod (append x lXData)) 
)

(defun xm:get-data ( e a / lData lXData )
	(if (null e) (setq e (car (entsel "\nSelect entity: "))))
	
	(if e
		(if (setq lData (entget e (list a)))
			(setq lXData (em:entity-extended-data lData))
		)
	)
	
	;(princ lXData)
	
	(if lXData (cdr (car lXData)))
)

