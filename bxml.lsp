(defun bxml ( a1 / a2 a3 b d c h s lHandles lColumns )
	(if (setq s (ssget "_X" '((0 . "INSERT"))))
		(cond
			((setq lHandles (bm:search s a1))
				(setq a2 "")
				
				(foreach h lHandles			
					(setq a2 (strcat a2 (xm:create-element "OBJECT" (xm:create-node (bm:insert-attributes (handent h))))))
				)

				(setq a2 (xm:create-element "ROOT" a2))

				(xm:create-file (setq a3 (strcat (fm:drawing-path) (fm:drawing-base) ".xml")) a2)
				
				(princ (strcat "\nTotal blocks found: " (itoa (length lHandles))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lHandles)))))
				(princ (strcat "\nXML file saved as : " a3))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" a1 "\".")))
		)
		(princ "\nDrawing does not contain any blocks.")
	)
	
	(princ)
)

(defun c:bxml ()
	(bxml "TAG*")
)
