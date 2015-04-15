;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun table-insert ( aTitle lWidths lData / oTable )
	(cm:setvar "CMDECHO" 0)
	
	(if (null lWidths)
		(setq lWidths (tm:get-data-column-widths lData))
	)
	
	(tm:table-init "Standard" "Courier New")
	
	(setq oTable (tm:table-create nil lData)) ; Create the AutoCAD table
	(tm:table-set-title oTable aTitle)
	(tm:table-set-width oTable nil lWidths)
	(tm:table-show oTable)
)

(defun table-data ( aFilter lEntities / e lData )
	(cond
		((setq lEntities (bm:search aFilter lEntities))
			; Add header
			(setq lData (tm:data-row-add (bm:get-attribute-tags|all lEntities) nil))
			
			; Add data rows
			(foreach e lEntities (setq lData (tm:data-row-add (bm:get-attributes e) lData)))
			
			(princ (strcat "\nTotal blocks found: " (itoa (length lEntities))))
			(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lEntities)))))
		)
	)
	
	
	
	lData
)

(defun c:block-table ( / lData )
	(cm:initialize)
	
	(if (setq lData (table-data "*" (im:select-blocks)))
		(table-insert "BLOCK TABLE" nil lData)
	)
	
	(cm:terminate)
)

(defun c:btable ( / lData )
	(cm:initialize)
	
	(if (setq lData (table-data "BALLOON" (im:select-all-blocks)))
		(table-insert "PARTS LIST" nil lData)
	)
	
	(cm:terminate)
)

(defun c:ptable ( / i j lData )
	(defun ID ( j lData )
		(strcat 
			(tm:data-cell-value "LETTER" j lData) 
			" " 
			(tm:data-cell-value "NUMBER" j lData)
		)
	)
	
	(cm:initialize)
	
	(cond
		((setq lData (table-data "SYMBOL*" (im:select-all-blocks)))
			; Sort by column 'Number' and 'Letter'
			(setq lData (tm:data-column-sort "NUMBER,LETTER" lData))
			
			; Add a column named 'ID'
			(setq lData (tm:data-column-add "ID" 0 lData))
			
			; Update column 'ID'
			(setq i (lm:nth "ID" (car lData)))
			
			(repeat (1- (setq j (length lData)))
				(setq 
					j (1- j)
					lData (tm:data-cell-change (ID j lData) i j lData)
				)
			)
			
			; Remove column 'Number' and 'Letter'
			(setq lData (tm:data-column-delete "NUMBER,LETTER" lData))
			
			(table-insert "P & I D" nil lData)
		)
	)
	
	(cm:terminate)
)

(princ)
