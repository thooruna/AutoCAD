(defun blocklist ( s a / d h lAttributes lColumns lHandles )
	(setq i 10) ;- Max length for handle string
	
	(defun PrintDivider ()
		(princ (sm:string-right-fill "\n" "=" (+ i 2 (length lColumns) (apply '+ (mapcar 'cdr lColumns)))))
	)

	(defun PrintHeader ()
		(princ "\n")
		(princ (sm:string-right-fill "Handle" " " i))
		(princ "|")
		
		(foreach d lColumns
			(princ (sm:string-right-fill (car d) " " (cdr d)))
			(princ "|")
		)
	)
	
	(defun PrintData ()
		(foreach h lHandles
			(setq lAttributes (bm:insert-attributes (handent h)))
		
			(princ "\n")
			(princ (sm:string-right-fill h " " i))
			(princ "|")
		
			(foreach d lColumns
				(princ (sm:string-right-fill (cdr (assoc (car d) lAttributes)) " " (cdr (assoc (car d) lColumns))))
				(princ "|")
			)
		)
	)
		
	(if s
		(cond
			((setq lHandles (bm:search s a))
				(setq lColumns (bm:all-attributes-length lHandles))
	
				(PrintDivider)
				(PrintHeader)
				(PrintDivider)
				(PrintData)
				(PrintDivider)
			
				(princ (strcat "\nTotal blocks found: " (itoa (length lHandles))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lHandles)))))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" a "\".")))
		)
		(princ "\nDrawing does not contain any blocks.")
	)
	
	(princ)
)

(defun c:blocklist ()
	(blocklist (im:select-all-blocks) "SYMBOL*")
)

(princ)


