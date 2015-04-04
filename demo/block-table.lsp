;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-table ( s a aTitle / d e lData lEntities lHeader oTable)
	(setq lData '())
	
	(if s
		(cond
			((setq lEntities (bm:search-entities s a))
				(setq 
					lHeader (bm:get-attribute-lengths lEntities) 
					lData (tm:table-data-add-row lData (mapcar 'car lHeader)) ; First add the table header
				)
				
				;;; Add the table data rows
				(foreach e lEntities
					(setq lData (tm:table-data-add-row lData (bm:get-attributes e)))
				)
				
				;;; Sort the table data
				(setq lData (tm:table-data-sort lData 2)) ; Sort the table by column '2'
				(setq lData (tm:table-data-sort lData 1)) ; Sort the table by column '1'
				
				;;; Create the AutoCAD table
				(tm:table-init "Standard" "Courier New")
				
				(setq oTable (tm:table-create nil lData))
				(tm:table-set-title oTable aTitle)
				(tm:table-set-width oTable (mapcar 'cdr lHeader))
				(tm:table-show oTable)
				
				(princ (strcat "\nTotal blocks found: " (itoa (length lEntities))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lEntities)))))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" a "\".")))
		)
	)
	
	(princ)
)

(defun c:block-table ( / s )
	(block-table (im:select-all-blocks) "*" "BLOCK TABLE")
)

(defun c:btable ( / s )
	(block-table (im:select-all-blocks) "BALLOON" "PARTS LIST")
)

(defun c:ptable ( / s )
	(block-table (im:select-all-blocks) "SYMBOL*" "P & I D")
)

(princ)
