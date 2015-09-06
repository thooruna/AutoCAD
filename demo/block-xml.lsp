;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-xml-report-location ()
	(strcat (findfile "demo") "\\reports")
)

(defun c:block-xml-update ( / aContent aFile aObject d e h )
	(cm:initialize)
	
	(if (findfile (setq aFile (strcat (fm:drawing-path) (fm:drawing-base) ".xml")))
		(if (setq aContent (fm:read-file aFile))
			(foreach aObject (xm:get-node-list "INSERT" aContent)
				(if (setq h (cdr (assoc "INSERT" (xm:get-attributes "INSERT" aObject))))
					(if (setq e (handent h))
						(foreach d (xm:get-nodes "INSERT" aObject)
							(bm:change-attribute-value e (car d) (cdr d))
						)
					)
				)
			)
		)
	)
	
	(cm:terminate)
)

(defun block-xml-write ( lBlocks aFilter / aContent aFile aHeader e )
	(cm:initialize)
	
	(setq 
		aHeader 
			(strcat
				(xm:add-version)
				(xm:add-stylesheet (strcat (block-xml-report-location) "\\html.xsl"))
				(xm:add-stylesheet (strcat (block-xml-report-location) "\\excel.xsl"))
			)
		aContent ""
	)
	
	(if lBlocks
		(cond
			((setq lBlocks (bm:search-blocks-with-attributes aFilter lBlocks))
				(foreach e lBlocks
					(setq aContent (strcat aContent (xm:create-node "INSERT" (xm:create-nodes (bm:get-attributes e)) (bm:get-id e))))
				)
				
				(setq aContent (xm:create-node "DRAWING" aContent (list (cons "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance") (cons "reports" (block-xml-report-location)))))
				
				(xm:write-file (setq aFile (strcat (fm:drawing-path) (fm:drawing-base) ".xml")) aHeader aContent)
				
				(princ (strcat "\nTotal blocks found: " (itoa (length lBlocks))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lBlocks)))))
				(princ (strcat "\nXML file saved as: " aFile))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" aFilter "\".")))
		)
	)
	
	(cm:terminate)
)

(defun c:block-xml-write ( )
	(block-xml-write (im:select-all-blocks) "*")
)

(princ)
