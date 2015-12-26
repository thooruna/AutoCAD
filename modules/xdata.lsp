;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun xm:set-data ( x aRegApp xSymbols / lXData )
	(setq x (if x (em:edd x) (em:edd (entlast))))
	
	(regapp aRegApp)
	
	(setq lXData
		(list
			(list -3
				(cons
					aRegApp
					(mapcar
						'(lambda (a)
							(cons
								(cond
									((= a "pWorldSpace|X") 1011)
									((= a "pWorldSpace|Y") 1021)
									((= a "pWorldSpace|Z") 1031)
									((= a "rDistance") 1041)
									((= a "rScaleFactor") 1042)
									(T 1000)
								)
								(cond
									((wcmatch a "pWorldSpace|X,pWorldSpace|Y,pWorldSpace|Z,rDistance,rScaleFactor") (vl-symbol-value (read  a)))
									(T (if (boundp (read a)) (sm:to-string (vl-symbol-value (read  a))) ""))
								)
							)
						)
						(lm:x->list xSymbols)
					)
				)
			)
		)
	)
	
	(entmod (append x lXData))
	
	(if lXData (cdr (car lXData)))
)

(defun xm:get-data ( e aRegApp xSymbols / d lData lXData )
	(if (null e) (setq e (car (entsel "\nSelect entity: "))))
	
	(if e
		(if (setq lData (entget e (list aRegApp)))
			(if (setq lXData (em:entity-extended-data lData))
				(foreach d (mapcar 'cons  (lm:x->list xSymbols) (mapcar 'cdr (cdr (car lXData))))
					(set 
						(read (car d))
						(cond
							((wcmatch (car d) "pWorldSpace|X,pWorldSpace|Y,pWorldSpace|Z,rDistance,rScaleFactor") (cdr d))
							(T (sm:to-string|empty->null (cdr d)))
						)
					)
				)
			)
		)
	)
	
	(if lXData (cdr (car lXData)))
)

