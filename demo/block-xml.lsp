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
						)
					)
				)
			)
		)
	)
	
	(cm:terminate)
)

(defun block-xml-write ( lBlocks aFilter /  aContent aFile e )
	(cm:initialize)
	
	(setq aContent "")
	
	(if lBlocks
		(cond
			((setq lBlocks (bm:search aFilter lBlocks))
				(foreach e lBlocks
					(setq aContent (strcat aContent (xm:create-node "OBJECT" (xm:create-nodes (bm:get-attributes e)) (bm:get-id e))))
				)
				
				(setq aContent (xm:create-node "ROOT" aContent '("xmlns:xsi" . "http://www.w3.org/2001/XMLSchema-instance")))
				
				(xm:write-file (setq aFile (strcat (fm:drawing-path) (fm:drawing-base) ".xml")) aContent)
				
				(princ (strcat "\nTotal blocks found: " (itoa (length lBlocks))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lBlocks)))))
				(princ (strcat "\nXML file saved as : " aFile))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" aFilter "\".")))
		)
	)
	
	(cm:terminate)
)

(defun c:block-xml-write ( )
	(block-xml-write (im:select-all-blocks) "SYMBOL*")
)

(princ)
