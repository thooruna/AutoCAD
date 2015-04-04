;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun xm:string-escape ( a / d )
	(setq a (sm:to-string a))
	
	(foreach d '(("'" . "&apos;") ("\"" . "&quot;") ("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;"))
		(setq a (sm:string-subst (cdr d) (car d) a))
	)
	
	a
)

(defun xm:string-clean ( a / d )
	(setq a (sm:to-string a))
	
	(foreach d '(("#" . "_") ("?" . "_"))
		(setq a (sm:string-subst (cdr d) (car d) a))
	)
	
	(strcase a T)
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
			(setq x (strcase x T))
			(strcat "\n<" x (xm:create-attributes l) ">" a "\n</" x ">")
		)
	)
)

(defun xm:create-file ( aFile aContent / f )
	(setq f (open aFile "w")) 
	
	(cond
		(f 
			(write-line "<?xml version=\"1.0\"?>" f)
			(write-line aContent f)
			(close f)
		)
	)
)

(princ)
