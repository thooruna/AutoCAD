;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-xml ( aFilter /  aContent aFile b d c h s lHandles lColumns )
	(if (setq s (ssget "_X" '((0 . "INSERT"))))
		(cond
			((setq lHandles (bm:search-handles s aFilter))
				(setq aContent "")
				
				(foreach h lHandles
					(princ (setq aAttributes (bm:get-id (handent h))))
					(setq aContent (strcat aContent (xm:create-node "OBJECT" (xm:create-nodes (bm:get-attributes (handent h))) aAttributes)))
				)
				
				(setq aContent (xm:create-node "ROOT" aContent '("xmlns:xsi" . "http://www.w3.org/2001/XMLSchema-instance")))
				
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

(defun c:block-xml ( )
	(block-xml "SYMBOL*")
)

(princ)
