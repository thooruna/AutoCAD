;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun table-insert ( pInsert aTitle lWidths lData / oTable )
	(cm:setvar "CMDECHO" 0)
	
	(setq lWidths (if lWidths lWidths (tm:get-data-column-widths lData)))
	
	(tm:table-init nil nil)
	
	(setq oTable (tm:table-create pInsert lData)) ; Create the AutoCAD table
	
	(tm:table-set-title oTable aTitle)
	(tm:table-set-width oTable nil lWidths)
	
	oTable
)

(defun table-data ( xBlocks xDefinition aFilter lEntities / e aAttribute aCell aColumn aSpace aValue d lRow lTemp )
	(cond
		((setq lEntities (bm:search-blocks-with-attributes|all xBlocks lEntities))
			(setq 
				xDefinition (lm:x->list (if xDefinition xDefinition (bm:get-attribute-tags|all lEntities))) 
				lData (if (> (length lData) 0) lData (list (mapcar 'sm:string-name xDefinition))) ; Header row
				lTemp (lm:wcmatch (mapcar 'sm:string-name xDefinition) "!*") ; Temporary columns
			)
			
			(foreach e lEntities 
				(setq 
					lRow nil
					xValues (lm:x->list (bm:get-attributes e))
				)
				
				(foreach aColumn (mapcar 'sm:string-value xDefinition) ; followed by data rows
					(if (= (substr aColumn 1 1) "!") (setq aColumn (substr aColumn 2)))
					
					(setq aCell nil)
					
					(foreach aAttribute (lm:string->list aColumn "|")
						(cond
							((= aAttribute ":N") (setq aCell (em:name e)))
							((= aAttribute ":X") (setq aCell (rtos (em:primary-point|X e) 2 0)))
							((= aAttribute ":Y") (setq aCell (rtos (em:primary-point|Y e) 2 0)))
							((= aAttribute ":Z") (setq aCell (rtos (em:primary-point|Z e) 2 0)))
							((wcmatch aAttribute "<*>") (setq aSpace (sm:string-substring|exclude aAttribute "<" ">")))
							(T (if (setq d (assoc (strcase aAttribute) xValues))
									(if (> (sm:string-length (setq aValue (if (/= (cdr d) "?") (cdr d) ""))) 0)
										(setq 
											aCell (if aCell (strcat aCell (if aSpace aSpace " ") aValue) aValue)
											aSpace nil
										)
									)
								)
							)
						)
					)
					
					(setq lRow (append lRow (list (if aCell aCell ""))))
				)
				
				(if (if aFilter (wcmatch (nth (lm:nth (sm:string-name aFilter) (car lData)) lRow) (sm:string-value aFilter)) T)
					(setq lData (append lData (list lRow)))
				)
			)
			
			; Sort by the temporary columns or first column only
			(setq lData (tm:data-column-sort (if lTemp lTemp (car (car lData))) lData))
			
			; Remove temporary columns
			(setq lData (tm:data-column-delete lTemp lData))
			
			(princ (strcat "\nTotal blocks found: " (itoa (length lEntities))))
			(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lEntities)))))
		)
	)
	
	lData
)

(defun block-table ( pInsert aTitle aSymbol aDefinition aFilter aDuplicates / lData oTable a )
	(cond
		((setq lData (table-data aSymbol aDefinition aFilter (im:select-all-blocks)))
			(setq oTable (table-insert pInsert aTitle nil lData))
			
			(if aDuplicates 
				(if (setq a (lm:nth aDuplicates (car lData)))
					(tm:data-row-highlight oTable (lm:duplicates (mapcar '(lambda (x) (nth a x)) lData)) lData)
				)
			)
			
			(tm:table-show oTable)
			(xm:set-data oTable (list "THOORUNA" "v1.0" "BLOCK-TABLE" aTitle aSymbol aDefinition aFilter aDuplicates))
		)
	)
)

(defun c:ptable ( / aDefinition aDuplicates aFilter aSymbol aTitle )
	(cm:debug T)
	(cm:initialize)
	
	(initget "1 2 3 4 5 6 7 8 9 0 *" 129)
	(setq aFilter (getkword "\nFilter group [*/0/1/2/3/4/5/6/7/8/9] <*>: "))
	(if (wcmatch aFilter ",`*") (setq aFilter nil))
	
	(setq
		aTitle (strcat "P & I D" (if aFilter (strcat " - Group " aFilter) ""))
		aSymbol "SYMBOL*"
		aDefinition "!NUMBER,!LETTER,ID=LETTER|NUMBER,DESCRIPTION"
		aFilter (if aFilter (strcat "!NUMBER=" aFilter "*"))
		aDuplicates "ID"
	)
	
	(block-table nil aTitle aSymbol aDefinition aFilter aDuplicates)
	
	(cm:terminate)
)

(defun c:btable ( / aDefinition aDuplicates aFilter aSymbol aTitle )
	(cm:debug T)
	(cm:initialize)
	
	(setq
		aTitle "PARTS LIST"
		aSymbol "BALLOON"
		aDefinition nil
		aFilter nil
		aDuplicates "ID"
	)
	
	(block-table nil aTitle aSymbol aDefinition aFilter aDuplicates)
	
	(cm:terminate)
)

(defun c:block-table ( / aDefinition aDuplicates aFilter aSymbol aTitle )
	(cm:debug T)
	(cm:initialize)
	
	(setq
		aTitle "BLOCK TABLE"
		aSymbol "*"
		aDefinition nil
		aFilter nil
		aDuplicates nil
	)
	
	(block-table nil aTitle aSymbol aDefinition aFilter aDuplicates)
	
	(cm:terminate)
)

(defun c:block-table-update ( / aDefinition aDuplicates aFunction aFilter aSymbol aTitle aVersion e l)
	(cm:debug T)
	(cm:initialize)
	
	(foreach e (im:select-all-tables)
		(cond 
			((setq l (xm:get-data e "THOORUNA"))
				(setq 
					aVersion (cdr (nth 0 l))
					aFunction (cdr (nth 1 l))
				)
				(cond 
					((= aFunction "BLOCK-TABLE")
						(setq
							aTitle  (cdr (nth 2 l))
							aSymbol (cdr (nth 3 l))
							aDefinition (cdr (nth 4 l))
							aFilter (cdr (nth 5 l))
							aDuplicates (cdr (nth 6 l))
						)
						
						(if (= aFilter "") (setq aFilter nil))
						(if (= aDefinition "") (setq aDefinition nil))
						(if (= aDuplicates "") (setq aDuplicates nil))
						
						(block-table (em:primary-point e) aTitle aSymbol aDefinition aFilter aDuplicates)
						(entdel e)
					)
				)
			)
		)
	)
	
	(cm:terminate)
)

(princ)