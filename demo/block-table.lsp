;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun table-insert ( aTitle lWidths lData / oTable )
	(cm:setvar "CMDECHO" 0)
	
	(setq lWidths (if lWidths lWidths (tm:get-data-column-widths lData)))
	
	(tm:table-init nil nil)
	
	(setq oTable (tm:table-create nil lData)) ; Create the AutoCAD table
	
	(tm:table-set-title oTable aTitle)
	(tm:table-set-width oTable nil lWidths)
	
	oTable
)

(defun table-data ( xBlocks xColumns aFilter lEntities / e aAttribute aColumn aSpace aTemp aValue d lTemp )
	(cond
		((setq lEntities (bm:search-blocks-with-attributes|all xBlocks lEntities))
			(setq 
				xColumns (lm:x->list (if xColumns xColumns (bm:get-attribute-tags|all lEntities)))
				lData (if (> (length lData) 0) lData (list (mapcar 'sm:string-name xColumns)))
			)

			(foreach e lEntities 
				(setq 
					lTemp nil
					xValues (lm:x->list (bm:get-attributes e))
				)
				
				(foreach aColumn (mapcar 'sm:string-value xColumns) ; followed by data rows
					(setq aTemp nil)
					
					(foreach aAttribute (lm:string->list aColumn "|")
						(cond
							((= aAttribute "!N") (setq aTemp (em:name e)))
							((= aAttribute "!X") (setq aTemp (rtos (em:primary-point|X e) 2 0)))
							((= aAttribute "!Y") (setq aTemp (rtos (em:primary-point|Y e) 2 0)))
							((= aAttribute "!Z") (setq aTemp (rtos (em:primary-point|Z e) 2 0)))
							((wcmatch aAttribute "<*>") (setq aSpace (sm:string-substring|exclude aAttribute "<" ">")))
							(T (if (setq d (assoc (strcase aAttribute) xValues))
									(if (> (sm:string-length (setq aValue (if (/= (cdr d) "?") (cdr d) ""))) 0)
										(setq 
											aTemp (if aTemp (strcat aTemp (if aSpace aSpace " ") aValue) aValue)
											aSpace nil
										)
									)
								)
							)
						)
					)
					
					(setq lTemp (append lTemp (list (if aTemp aTemp ""))))
				)
				
				(if (if aFilter (wcmatch (nth (lm:nth (sm:string-name aFilter) (car lData)) lTemp) (sm:string-value aFilter)) T)
					(setq lData (append lData (list lTemp)))
				)
			)
			
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

(defun c:ptable ( / aFilter lData oTable )
	(cm:initialize)
	
	(initget "1 2 3 4 5 6 7 8 9 0 *" 131)
	(setq aFilter (getkword "\nFilter group [*/0/1/2/3/4/5/6/7/8/9] <*>: "))
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
			
			(setq oTable (table-insert "P & I D" nil lData))
			
			(tm:data-row-highlight oTable (lm:duplicates (mapcar 'car lData)) lData)
			(tm:table-show oTable)
			
		)
	)
	
	(cm:terminate)
)

(princ)