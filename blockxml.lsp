(defun blockxml ( aFilter / aContent aFile b d c h s lHandles lColumns )
	(if (setq s (ssget "_X" '((0 . "INSERT"))))
		(cond
			((setq lHandles (bm:search s aFilter))
				(setq aContent "")
				
				(foreach h lHandles			
					(setq aContent (strcat aContent (xm:create-element "OBJECT" (xm:create-node (bm:insert-attributes (handent h))))))
				)

				(setq aContent (xm:create-element "ROOT" aContent))

				(xm:create-file (setq aFile (strcat (fm:drawing-path) (fm:drawing-base) ".xml")) aContent)
				
				(princ (strcat "\nTotal blocks found: " (itoa (length lHandles))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lHandles)))))
				(princ (strcat "\nXML file saved as : " aFile))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" aFilter "\".")))
		)
		(princ "\nDrawing does not contain any blocks.")
	)
	
	(princ)
)

(defun c:blockxml ()
	(blockxml "SYMBOL*")
)

(princ)