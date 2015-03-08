(defun blocklist ( s a / d h lAttributes lHeader lHandles )
	(defun PrintDivider ()
		(princ (sm:string-right-fill "\n" "=" (+ i 2 (length lHeader) (apply '+ (mapcar 'cdr lHeader)))))
	)

	(defun PrintHeader ()
		(princ "\n")
		(princ (sm:string-right-fill "Handle" " " i))
		(princ "|")
		
		(foreach d lHeader
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
		
			(foreach d lHeader
				(princ (sm:string-right-fill (cdr (assoc (car d) lAttributes)) " " (cdr (assoc (car d) lHeader))))
				(princ "|")
			)
		)
	)
	
	(if s
		(cond
			((setq lHandles (bm:search s a))
				(setq lHeader (bm:insert-attribute-lengths lHandles))
				
				(if (< (setq i (bm:handle-lengths lHandles)) 6) ;- Max length for handle string
					(setq i 6)
				)

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
	)
	
	(princ)
)

(defun c:blocklist ()
	(blocklist (im:select-all-blocks) "*")
)

(princ)


