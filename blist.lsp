(defun blist ( s / ss h d lHandles lColumns lAttributes )
	(if (setq ss (ssget "_X" '((0 . "INSERT"))))
		(setq lHandles (bm:search ss s))
	)
	
	(defun PrintDivider ()
		(princ (sm:string-right-fill "\n" "=" (+ 8 (length lColumns) (apply '+ (mapcar 'cdr lColumns)))))
	)

	(defun PrintHeader ()
		(princ "\nHandle|")
	
		(foreach d lColumns
			(princ (sm:string-right-fill (car d) " " (cdr d)))
			(princ "|")
		)
	)
	
	(defun PrintData ()
		(foreach h lHandles
			(setq lAttributes (bm:insert-attributes (handent h)))
		
			(princ "\n")
			(princ h)
			(princ "   |")
		
			(foreach d lColumns
				(princ (sm:string-right-fill (cdr (assoc (car d) lAttributes)) " " (cdr (assoc (car d) lColumns))))
				(princ "|")
			)
		)
	)
		
	(setq lColumns (bm:all-attributes-length lHandles))
	
	(PrintDivider)
	(PrintHeader)
	(PrintDivider)
	(PrintData)
	(PrintDivider)
	
	(princ)
)

(defun c:blist ()
	(blist "TAG*")
)




