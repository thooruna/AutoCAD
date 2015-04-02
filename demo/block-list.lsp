;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-list ( s a / d h i lAttributes lHeader lHandles )
	(defun PrintDivider ()
		(princ (sm:string-right-fill "\n" "=" (+ i 2 (length lHeader) (apply '+ (mapcar 'cdr lHeader)))))
	)

	(defun PrintHeader ()
		(princ "\n")
		(princ (sm:string-right-fill "Handle" " " i))
		(princ "|")
		
		(foreach d lHeader
			(princ (sm:string-right-fill (car d) " " (cdr d)))
			(princ "|")
		)
	)
	
	(defun PrintData ()
		(foreach h lHandles
			(setq lAttributes (bm:get-attributes (handent h)))
		
			(princ "\n")
			(princ (sm:string-right-fill h " " i))
			(princ "|")
		
			(foreach d lHeader
				(princ (sm:string-right-fill (cdr (assoc (car d) lAttributes)) " " (cdr (assoc (car d) lHeader))))
				(princ "|")
			)
		)
	)
	
	(if s
		(cond
			((setq lHandles (bm:search s a))
				(setq lHeader (bm:get-attribute-lengths lHandles))
				
				(setq i (max (bm:handle-lengths lHandles) (strlen "Handle"))) ; Max length for handle string
				
				(PrintDivider)
				(PrintHeader)
				(PrintDivider)
				(PrintData)
				(PrintDivider)
			
				(princ (strcat "\nTotal blocks found: " (itoa (length lHandles))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lHandles)))))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" a "\".")))
		)
	)
	
	(princ)
)

(defun c:block-list ()
	(block-list (im:select-all-blocks) "*")
)

(defun c:blist ( / s )
	(block-list (im:select-all-blocks) "BALLOON")
)

(defun c:plist ( / s )
	(block-list (im:select-all-blocks) "SYMBOL*")
)

(princ)


