(defun xm:string-escape ( a / d )
	(foreach d '(("'" . "&apos;") ("\"" . "&quot;") ("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;"))
		(setq a (sm:string-subst (cdr d) (car d) a))
	)
	
	a
)

(defun xm:string-clean ( a / d )
	(foreach d '(("#" . "_") ("?" . "_"))
		(setq a (sm:string-subst (cdr d) (car d) a))
	)
	
	(strcase a T)
)

(defun xm:create-node ( x / a d )
	(setq a "")
	
	(if (= (type x) 'LIST)
		(if (listp (cdr x)) 
			(foreach d x (setq a (strcat a (xm:create-node d))))
			(setq a (strcat "<" (xm:string-clean (car x)) ">" (xm:string-escape (cdr x)) "</" (xm:string-clean (car x)) ">"))
		)
	)
	
	a
)

(defun xm:create-element ( x a )
	(cond 
		((= (type x) 'STR)
			(setq x (strcase x T))
			(strcat "<" x ">" a "</" x ">")
		)
	)
)

(defun xm:create-file ( a1 a2 / f )
	(setq f (open a1 "w")) 
	
	(cond
		(f 
			(write-line "<?xml version=\"1.0\"?>" f)
			(write-line a2 f)
			(close f)
		)
	)
)
