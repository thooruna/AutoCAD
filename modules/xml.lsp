;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun xm:string-escape ( a / d )
	(setq a (sm:to-string a))
	
	(foreach d '(("'" . "&apos;") ("\"" . "&quot;") ("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;"))
		(setq a (sm:string-substitute (cdr d) (car d) a))
	)
	
	a
)

(defun xm:string-clean ( a / d )
	(setq a (sm:to-string a))
	
	(foreach d '(("#" . "_") ("?" . "_") ("\n" . "") ("\t" . ""))
		(setq a (sm:string-substitute (cdr d) (car d) a))
	)
	
	a
	;(strcase a T)
)

(defun xm:create-nodes ( x / a d )
	(setq a "")
	
	(if (= (type x) 'LIST)
		(if (listp (cdr x)) 
			(foreach d x (setq a (strcat a (xm:create-nodes d))))
			(setq a (strcat "\n\t<" (xm:string-clean (car x)) ">" (xm:string-escape (cdr x)) "</" (xm:string-clean (car x)) ">"))
		)
	)
	
	a
)

(defun xm:create-node ( x a l / d )
	(cond 
		((= (type x) 'STR)
			;(setq x (strcase x T))
			(strcat "\n<" x (xm:create-attributes l) ">" a "\n</" x ">")
		)
	)
)

(defun xm:create-attributes ( x / a d )
	(setq a "")
	
	(if (= (type x) 'LIST)
		(if (listp (cdr x)) 
			(foreach d x (setq a (strcat a (xm:create-attributes d))))
			(setq a (strcat " " (xm:string-clean (car x)) "=\"" (xm:string-escape (cdr x)) "\""))
		)
	)
	
	a
)

(defun xm:write-file ( aFile aContent / f )
	(setq f (open aFile "w")) 
	
	(cond
		(f 
			(write-line "<?xml version=\"1.0\"?>" f)
			(write-line aContent f)
			(close f)
		)
	)
)

;;; Note: returns all nodes (works only on nodes without attributes)

(defun xm:get-nodes ( aName aContent / aTag aValue l )
	(if (setq aContent (xm:get-node-string aName aContent))
		(foreach aTag (lm:unique (lm:str->lst|exclude (sm:string-substitute "<" "</" aContent) "<" ">"))
			(if (setq aValue (sm:string-substring|exclude aContent (strcat "<" aTag ">") (strcat "</" aTag ">")))
				(setq l (cons (cons aTag aValue) l))
			)
		)
	)
	
	l
)

(defun xm:get-node-string ( aName aContent )
	(if (setq aContent (sm:string-substring|include aContent (strcat "<" aName) (strcat "</" aName ">")))
		(setq aContent (sm:string-substring|exclude aContent ">" (strcat "</" aName ">")))
	)
)

(defun xm:get-node-list ( aName aContent )
	(lm:str->lst|include aContent (strcat "<" aName) (strcat "</" aName ">"))
)

(defun xm:get-attributes ( aName aContent / aTag aValue l )
	(if (setq aContent (xm:get-attribute-string aName aContent))
		(foreach aTag (lm:str->lst|exclude aContent " " "=")
			(if (setq aValue (sm:string-substring|exclude aContent (strcat aTag "=\"") "\""))
				(setq l (cons (cons aTag aValue) l))
			)
		)
	)
	
	l
)

(defun xm:get-attribute-string ( aName aContent )
	(setq aContent (sm:string-substring|exclude aContent (strcat "<" aName) ">"))
)

(princ)
