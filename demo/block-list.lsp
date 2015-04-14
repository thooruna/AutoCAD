;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-list ( lBlocks aFilter / d e i lAttributes lHeader )
	(defun PrintDivider ()
		(princ (sm:string-right-fill "\n" "=" (+ i 3 (length lHeader) (apply '+ (mapcar 'cdr lHeader)))))
	)

	(defun PrintHeader ()
		(princ "\n|")
		(princ (sm:string-right-fill "HANDLE" " " i))
		(princ "|")
		
		(foreach d lHeader
			(princ (sm:string-right-fill (car d) " " (cdr d)))
			(princ "|")
		)
	)
	
	(defun PrintData ()
		(foreach e lBlocks
			(setq lAttributes (bm:get-attributes e))
		
			(princ "\n|")
			(princ (sm:string-right-fill (em:handle e) " " i))
			(princ "|")
			
			(foreach d lHeader
				(princ (sm:string-right-fill (cdr (assoc (car d) lAttributes)) " " (cdr (assoc (car d) lHeader))))
				(princ "|")
			)
		)
	)
	
	(if lBlocks
		(cond
			((setq lBlocks (bm:search aFilter lBlocks))
				(setq lHeader (bm:get-attribute-lengths lBlocks))
				
				(setq i (max (bm:handle-lengths lBlocks) (strlen "Handle"))) ; Max length for handle string
				
				(PrintDivider)
				
				(princ (strcat "\nBlock name(s): " aFilter))
				
				(PrintDivider)
				(PrintHeader)
				(PrintDivider)
				(PrintData)
				(PrintDivider)
			
				(princ (strcat "\nTotal blocks found: " (itoa (length lBlocks))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lBlocks)))))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" aFilter "\".")))
		)
	)
	
	(princ)
)

(defun c:block-list ( / aFilter )
	(foreach aFilter (bm:list)
		(block-list (im:select-all-blocks) aFilter)
	)
)

(defun c:blist ( )
	(block-list (im:select-all-blocks) "BALLOON")
)

(defun c:plist ( )
	(block-list (im:select-all-blocks) "SYMBOL*")
)

(princ)


