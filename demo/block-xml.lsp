;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun c:block-xml-update ( / aContent aFile aObject d e h )
	(cm:initialize)
	
	(if (findfile (setq aFile (strcat (fm:drawing-path) (fm:drawing-base) ".xml")))
		(if (setq aContent (fm:read-file aFile))
			(foreach aObject (xm:get-node-list "OBJECT" aContent)
				(if (setq h (cdr (assoc "HANDLE" (xm:get-attributes "OBJECT" aObject))))
					(if (setq e (handent h))
						(foreach d (xm:get-nodes "OBJECT" aObject)
							(bm:change-attribute-value e (car d) (cdr d))
							(princ (car d))
						)
					)
				)
			)
		)
	)
	
	(cm:terminate)
)

(defun block-xml-write ( aFilter /  aContent aFile b d c h s lHandles lColumns )
	(cm:debug T)
	(cm:initialize)
	
	(setq aFile (strcat (fm:drawing-path) (fm:drawing-base) ".xml"))
	
	(if (setq s (ssget "_X" '((0 . "INSERT"))))
		(cond
			((setq lHandles (bm:search-handles s aFilter))
				(setq aContent "")
				
				(foreach h lHandles
					(princ (setq aAttributes (bm:get-id (handent h))))
					(setq aContent (strcat aContent (xm:create-node "OBJECT" (xm:create-nodes (bm:get-attributes (handent h))) aAttributes)))
				)
				
				(setq aContent (xm:create-node "ROOT" aContent '("XMLNS:XSI" . "http://www.w3.org/2001/XMLSchema-instance")))
				
				(xm:write-file aFile aContent)
				
				(princ (strcat "\nTotal blocks found: " (itoa (length lHandles))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lHandles)))))
				(princ (strcat "\nXML file saved as : " aFile))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" aFilter "\".")))
		)
		(princ "\nDrawing does not contain any blocks.")
	)
	
	(cm:terminate)
)

(defun c:block-xml-write ( )
	(block-xml-create "SYMBOL*")
)

(princ)
