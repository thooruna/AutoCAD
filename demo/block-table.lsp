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

(defun table-data ( xBlocks xColumns aFilter lEntities / e lData )
	(cond
		((setq lEntities (bm:search-blocks-with-attributes xBlocks lEntities))
			(if (null xColumns) (setq xColumns (bm:get-attribute-tags|all lEntities)))
			
			; Add rows
			(foreach e lEntities (setq lData (tm:data-row-add xColumns (bm:get-attributes e) aFilter lData)))
			
			(princ (strcat "\nTotal blocks found: " (itoa (length lEntities))))
			(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lEntities)))))
		)
	)
	
	lData
)

(defun c:block-table ( / lData )
	(cm:initialize)
	
	(if (setq lData (table-data "*" nil nil (im:select-blocks)))
		(table-insert "BLOCK TABLE" nil lData)
	)
	
	(cm:terminate)
)

(defun c:btable ( / lData )
	(cm:initialize)
	
	(if (setq lData (table-data "BALLOON" nil nil (im:select-all-blocks)))
		(table-insert "PARTS LIST" nil lData)
	)
	
	(cm:terminate)
)

(defun c:ptable ( / aFilter lData )
	(cm:debug T)
	(cm:initialize)
	
	(initget "1 2 3 4 5 6 7 8 9 0 *" 131)
	(setq aFilter (getkword "\nFilter group [1/2/3/4/5/6/7/8/9/0/*] <*>: "))
	(if (= aFilter "*") (setq aFilter nil))
	
	(cond
		(
			(setq lData 
				(table-data 
					"SYMBOL*"
					"NUMBER,LETTER,ID=LETTER|NUMBER,DESCRIPTION"
					(if aFilter (strcat "NUMBER=" aFilter "*"))
					(im:select-all-blocks)
				)
			)
			
			; Sort by column 'Number' and 'Letter'
			(setq lData (tm:data-column-sort "NUMBER,LETTER" lData))
			
			; Remove column 'Number' and 'Letter'
			(setq lData (tm:data-column-delete "NUMBER,LETTER" lData))
			
			(table-insert "P & I D" nil lData)
		)
	)
	
	(cm:terminate)
)

(princ)