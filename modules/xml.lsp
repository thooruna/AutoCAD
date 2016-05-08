;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun xml:string-escape ( a / d )
	(setq a (sm:to-string a))
	
	(foreach d '(("'" . "&apos;") ("\"" . "&quot;") ("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("²" . "&#178;") ("³" . "&#179;"))
		(setq a (sm:string-substitute (cdr d) (car d) a))
	)
	
	a
)

(defun xml:string-clean ( a / d )
	(setq a (sm:to-string a))
	
	(foreach d '(("#" . "Z") ("?" . "Z") ("\n" . "") ("\t" . ""))
		(setq a (sm:string-substitute (cdr d) (car d) a))
	)
	
	a
	;(strcase a T)
)

(defun xml:add-version ()
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
)

(defun xml:add-stylesheet ( a )
	(strcat "\n<?xml-stylesheet type=\"text/xsl\" href=\"" a "\"?>")
)

(defun xml:create-nodes ( x / a d )
	(setq a "")
	
	(if (= (type x) 'LIST)
		(if (listp (cdr x)) 
			(foreach d x (setq a (strcat a (xml:create-nodes d))))
			(setq a (strcat "\n\t<" (xml:string-clean (car x)) ">" (xml:string-escape (cdr x)) "</" (xml:string-clean (car x)) ">"))
		)
	)
	
	a
)

(defun xml:create-node ( x a l / d )
	(cond 
		((= (type x) 'STR)
			;(setq x (strcase x T))
			(strcat "\n<" x (xml:create-attributes l) ">" a "\n</" x ">")
		)
	)
)

(defun xml:create-attributes ( x / a d )
	(setq a "")
	
	(if (= (type x) 'LIST)
		(if (listp (cdr x)) 
			(foreach d x (setq a (strcat a (xml:create-attributes d))))
			(setq a (strcat " " (xml:string-clean (car x)) "=\"" (xml:string-escape (cdr x)) "\""))
		)
	)
	
	a
)

(defun xml:write-file ( aFile aHeader aContent / f )
	(setq f (open aFile "w")) 
	
	(cond
		(f 
			(write-line aHeader f)
			(write-line aContent f)
			(close f)
		)
	)
)

;;; Note: returns all nodes (works only on nodes without attributes)

(defun xml:get-nodes ( aName aContent / aTag aValue l )
	(if (setq aContent (xml:get-node-string aName aContent))
		(foreach aTag (lm:unique (lm:string->list|exclude (sm:string-substitute "<" "</" aContent) "<" ">"))
			(if (setq aValue (sm:string-substring|exclude aContent (strcat "<" aTag ">") (strcat "</" aTag ">")))
				(setq l (cons (cons aTag aValue) l))
			)
		)
	)
	
	l
)

(defun xml:get-node-string ( aName aContent )
	(if (setq aContent (sm:string-substring|include aContent (strcat "<" aName) (strcat "</" aName ">")))
		(setq aContent (sm:string-substring|exclude aContent ">" (strcat "</" aName ">")))
	)
)

(defun xml:get-node-list ( aName aContent )
	(lm:string->list|include aContent (strcat "<" aName) (strcat "</" aName ">"))
)

(defun xml:get-attributes ( aName aContent / aTag aValue l )
	(if (setq aContent (xml:get-attribute-string aName aContent))
		(foreach aTag (lm:string->list|exclude aContent " " "=")
			(if (setq aValue (sm:string-substring|exclude aContent (strcat aTag "=\"") "\""))
				(setq l (cons (cons aTag aValue) l))
			)
		)
	)
	
	l
)

(defun xml:get-attribute-string ( aName aContent )
	(setq aContent (sm:string-substring|exclude aContent (strcat "<" aName) ">"))
)

(princ)
