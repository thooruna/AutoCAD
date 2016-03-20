;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun screen-list ( lBlocks xFilter / d e i lAttributes lHeader )
	(defun PrintDivider ()
		(princ (sm:string-fill|right "\n" "=" (+ i 3 (length lHeader) (apply '+ (mapcar 'cdr lHeader)))))
	)

	(defun PrintHeader ()
		(princ "\n|")
		(princ (sm:string-fill|right "HANDLE" " " i))
		(princ "|")
		
		(foreach d lHeader
			(princ (sm:string-fill|right (car d) " " (cdr d)))
			(princ "|")
		)
	)
	
	(defun PrintData ()
		(foreach e lBlocks
			(setq lAttributes (bm:get-attributes e))
		
			(princ "\n|")
			(princ (sm:string-fill|right (em:handle e) " " i))
			(princ "|")
			
			(foreach d lHeader
				(princ (sm:string-fill|right (cdr (assoc (car d) lAttributes)) " " (cdr (assoc (car d) lHeader))))
				(princ "|")
			)
		)
	)
	
	(if lBlocks
		(cond
			((setq lBlocks (bm:search-blocks-with-attributes xFilter lBlocks))
				(setq lHeader (bm:get-attribute-lengths lBlocks))
				
				(setq i (max (bm:handle-lengths lBlocks) (strlen "Handle"))) ; Max length for handle string
				
				(PrintDivider)
				
				(princ (strcat "\nBlock name(s): " (lm:x->string xFilter)))
				
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

(defun xml-list ( s a )
	(block-xml-write s a)
)

(defun block-list ( s a )
	(setq x (im:get-keyword "\nOutput to " nil '("Screen" "Xml")))
	
	(cond 
		((= x "Screen") (screen-list s a))
		((= x "Xml") (xml-list s a))
	)
)

(defun c:block-list ( / aFilter )
	(block-list (im:select-all-blocks) (bm:list))
)

(defun c:blist ( )
	(block-list (im:select-all-blocks) "BALLOON")
)

(defun c:plist ( )
	(block-list (im:select-all-blocks) "SYMBOL*")
)

(princ)


