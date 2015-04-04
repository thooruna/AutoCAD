;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-list ( s a / d e i lAttributes lEntities lHeader )
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
		(foreach e lEntities
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
	
	(if s
		(cond
			((setq lEntities (bm:search-entities s a))
				(setq lHeader (bm:get-attribute-lengths lEntities))
				
				(setq i (max (bm:handle-lengths lEntities) (strlen "Handle"))) ; Max length for handle string
				
				(PrintDivider)
				
				(princ (strcat "\nBlock name(s): " a))
				
				(PrintDivider)
				(PrintHeader)
				(PrintDivider)
				(PrintData)
				(PrintDivider)
			
				(princ (strcat "\nTotal blocks found: " (itoa (length lEntities))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lEntities)))))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" a "\".")))
		)
	)
	
	(princ)
)

(defun c:block-list ( / a )
	(foreach a (bm:list)
		(block-list (im:select-all-blocks) a)
	)
)

(defun c:blist ( / s )
	(block-list (im:select-all-blocks) "BALLOON")
)

(defun c:plist ( / s )
	(block-list (im:select-all-blocks) "SYMBOL*")
)

(princ)


