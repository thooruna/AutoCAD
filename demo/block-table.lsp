;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-table|definition ( r a )
	(cond
		((= r 0) 
			(cond
				((= a "PTABLE") 
					(setq 
						aSymbol "SYMBOL*"
						aDefinition "!NUMBER,!LETTER,ID=LETTER|NUMBER,DESCRIPTION"
						aFilter (if aFilter aFilter nil)
						aSort "!NUMBER,!LETTER"
						aDuplicate "ID"
					)
				)
				((= a "BTABLE")
					(setq
						aSymbol "BALLOON"
						aDefinition nil
						aFilter nil
						aSort "ID"
						aDuplicate "ID"
					)
				)
				((= a "BLOCK-TABLE")
					(setq
						aSymbol (if aSymbol aSymbol "*")
						aDefinition nil
						aFilter nil
						aSort nil
						aDuplicate nil
					)
				)
			)
		)
	)
)

(defun block-table|regapp ( r )
	(cond
		((= r 0) "THOORUNA_BLOCK-TABLE_0")
	)
)

(defun block-table|xdata ( r )
	(cond
		((= r 0) "rScaleFactor,aTitle,aWidth,aFunction,aSymbol,aDefinition,aFilter,aSort,aDuplicate")
	)
)

(defun block-table|data ( xBlocks xDefinition aFilter lEntities / e aAttribute aCell aColumn aSpace aValue d lRow )
	(cond
		((setq lEntities (bm:search-blocks-with-attributes|all xBlocks lEntities))
			(setq 
				xDefinition (lm:x->list (if xDefinition xDefinition (bm:get-attribute-tags|all lEntities))) 
				lData (if (> (length lData) 0) lData (list (mapcar 'sm:string-name xDefinition))) ; Header row
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
			
			; Sort by specified columns or first column only
			(setq lData (tm:data-column-sort (if aSort aSort (car (car lData))) lData))
			
			; Remove temporary columns
			(setq lData (tm:data-column-delete (lm:wcmatch (mapcar 'sm:string-name xDefinition) "!*") lData))
			
			(princ (strcat "\nTotal blocks found: " (itoa (length lEntities))))
			(princ (strcat "\nUnique blocks: " (itoa (length (lm:unique lEntities)))))
			(princ (strcat "\nFiltered blocks: " (itoa (length lData))))
		)
	)
	
	lData
)

(defun block-table ( pInsert rScaleFactor aTitle aWidth aFunction aSymbol aDefinition aFilter aSort aDuplicate / lData oTable a )
	(cm:setvar "CMDECHO" 0)
	
	(cond
		((setq lData (block-table|data aSymbol aDefinition aFilter (im:select-all-blocks)))
			(tm:table-init nil nil)
			
			(setq oTable (tm:table-create pInsert lData)) ; Create the AutoCAD table
			
			(tm:table-set-title oTable aTitle)
			(tm:table-set-width oTable (if aWidth 1 nil) (if aWidth (lm:string->list|numbers aWidth) (tm:get-data-column-widths lData)))
			
			(if aDuplicate 
				(if (setq a (lm:nth aDuplicate (car lData)))
					(tm:data-row-highlight oTable (lm:duplicates (mapcar '(lambda (x) (nth a x)) lData)) lData)
				)
			)
			
			(tm:table-show oTable)
			
			(command "_.SCALE" (vlax-vla-object->ename oTable) "" pInsert rScaleFactor)
			(xm:set-data oTable (block-table|regapp 0) (block-table|xdata 0))
		)
	)
)

(defun c:ptable ( / aDefinition aDuplicate aFunction aFilter aSort aSymbol aTitle aWidth )
	(cm:initialize)
	
	(initget "1 2 3 4 5 6 7 8 9 0 *" 129)
	(setq aFilter (getkword "\nFilter group [*/0/1/2/3/4/5/6/7/8/9] <*>: "))
	(if (wcmatch aFilter ",`*") (setq aFilter nil))
	
	(setq
		aTitle (strcat "P & I D" (if aFilter (strcat " - Group " aFilter) ""))
		aWidth nil
		aFunction "PTABLE"
	)
	
	(block-table|definition 0 aFunction)
	(block-table nil 1 aTitle aWidth aFunction aSymbol aDefinition (if aFilter (strcat "!NUMBER=" aFilter "*")) aSort aDuplicate)
	
	(cm:terminate)
)

(defun c:btable ( / aDefinition aDuplicate aFunction aFilter aSort aSymbol aTitle aWidth )
	(cm:initialize)
	
	(setq
		aTitle "PARTS LIST"
		aWidth nil
		aFunction "BTABLE"
	)
	
	(block-table|definition 0 aFunction)
	(block-table nil 1 aTitle aWidth aFunction aSymbol aDefinition aFilter aSort aDuplicate)
	
	(cm:terminate)
)

(defun c:block-table ( / aDefinition aDuplicate aFunction aFilter aSort aSymbol aTitle aWidth )
	(cm:initialize)
	
	(setq
		aTitle "BLOCK TABLE"
		aWidth nil
		aFunction "BLOCK-TABLE"
	)
	
	(block-table|definition 0 aFunction)
	(block-table nil 1 aTitle aWidth aFunction aSymbol aDefinition aFilter aSort aDuplicate)
	
	(cm:terminate)
)

(defun c:block-table-update ( / aDefinition aDuplicate aFunction aFilter aSort aSymbol aTitle aWidth e l rScaleFactor )
	(cm:initialize)
	
	(foreach e (im:select-all-tables|current-tab)
		(cond 
			((setq l (xm:get-data e (block-table|regapp 0) (block-table|xdata 0)))
				(princ (if aTitle (strcat "\nUpdating table: " aTitle) "\nUpdating table..."))
				
				(block-table|definition 0 aFunction)
				(block-table (em:primary-point e) rScaleFactor aTitle aWidth aFunction aSymbol aDefinition aFilter aSort aDuplicate)
				(entdel e)
			)
			(T (princ "\nTable does not contain extended entity data."))
		)
	)
	
	(cm:terminate)
)

(princ)