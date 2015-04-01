;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun bm:find ( aBase / aPath )
	(setq aPath (findfile "symbols"))
	
	(cond
		((tblsearch "BLOCK" aBase) aBase)
		((findfile (strcat aBase ".dwg")))
		((findfile (strcat aPath "\\" aBase ".dwg")))
		(T 
			(princ (strcat "\nUnable to find: " aBase ".dwg"))
			nil
		)
	)
)

(defun bm:load ( xBase / a l )
	(setq l (mapcar 'bm:find (lm:x->list xBase)))
	
	(foreach a l
		(cond
			((tblsearch "BLOCK" a))
			(a
				(princ "\n")
				(command "_.-INSERT" a)
				(command) ;- Break out of insert command
			)
		)
	)
	
	(apply 'and l)
)

(defun bm:insert ( a p )
	(command "_.-INSERT" a p 1 1 0.0)
)

(defun bm:insert-symbol ( a p )
	(command "_.-INSERT" a p (getvar "DIMSCALE") 0.0)
)

(defun bm:insert-symbol-leader ( a p l )
	(apply 'command (append '("_.LEADER") (reverse l) (list "_A" "" "_B" a p (getvar "DIMSCALE") 0.0)))
	(command "_.REDRAW")
)

(defun bm:insert-symbol-extension-line ( a p l )
	(apply 'command (append '("_.PLINE") (reverse l) '("")))
	(bm:insert-symbol a p)
	(command "_.REDRAW")
)

(defun bm:handle-lengths ( l )
	(apply 'max (mapcar 'strlen l))
)

(defun bm:insertion-point ( x )
	(em:primary-point x)
)

(defun bm:insert-attribute-values ( e )
	(mapcar 'cdr (bm:insert-attributes e))
)

(defun bm:insert-attribute-tags ( e )
	(mapcar 'car (bm:insert-attributes e))
)

(defun bm:insert-attributes ( e )
	(if (= (em:type (setq e (entnext e))) "ATTRIB")
		(cons (cons (strcase (em:name e)) (em:value e)) (bm:insert-attributes e))
	)
)

(defun bm:insert-attributes-filter ( e l1 / l2 )
	(if (= (em:type (setq e (entnext e))) "ATTRIB")
		(if (member (em:name e) l1)
			(cons (cons (strcase (em:name e)) (em:value e)) (bm:insert-attributes-filter e l1))
			(bm:insert-attributes-filter e l1)
		)
	)
)

(defun bm:insert-attribute-lengths ( lHandles / h e a lAttributes )
	(setq lAttributes '())
	
	(foreach h lHandles
		(setq e (handent h))
		(foreach a (bm:insert-attribute-tags e)
			(if (not (assoc a lAttributes))
				(setq lAttributes (cons (cons a (max (strlen a) (bm:insert-attribute-max-length lHandles a))) lAttributes))
			)
		)
	)
	
	(reverse lAttributes)
)

(defun bm:insert-attribute-max ( l a )
	(cond
		(l (apply 'max (mapcar '(lambda ( x ) (atoi (cdr (assoc a (bm:insert-attributes (handent x)))))) l)))
		(T 0)
	)
)

(defun bm:insert-attribute-max-length ( l a )
	(cond
		(l (apply 'max (mapcar '(lambda ( x ) (sm:string-length (cdr (assoc a (bm:insert-attributes (handent x)))))) l)))
		(T 0)
	)
)

(defun bm:insert-has-attributes ( x )
	(= (em:entities-follow x) 1)
)

(defun bm:search ( s x / e i lHandles )
	(setq lHandles '())
	
	(defun SearchNested ( e )
		(setq e (em:entity-name-reference (tblsearch "BLOCK" (em:name e))))
		(while e
			(SearchCurrent e)
			(setq e (entnext e))
		)
	)
	
	(defun SearchCurrent ( e )
		(if (= (em:type e) "INSERT") 
			(if (bm:insert-has-attributes e)
				(if (wcmatch (em:name e) x)
					(setq lHandles (append lHandles (list (em:handle e))))
				)
				(SearchNested e)
			)
		)
	)
	
	(if (lm:is-list x)
		(setq x (lm:lst->str x ","))
	)
	
	(if s
		(repeat (setq i (sslength s))
			(setq e (ssname s (setq i (1- i))))
			(SearchCurrent e)
		)
	)
	
	lHandles
)

(defun bm:get-attribute-value ( e aTag )
	(if (= (em:type (setq e (entnext e))) "ATTRIB")
		(if (= (strcase (em:name e)) (strcase aTag))
			(em:value e)
			(bm:get-attribute-value e aTag)
		)
	)
)

(defun bm:change-attribute-value ( e aTag aValue / l )
	(if (= (em:type (setq l (entget (setq e (entnext e))))) "ATTRIB")
		(if (= (strcase (em:name l)) (strcase aTag))
			(if (entmod (subst (cons 1 aValue) (assoc 1 l) l))
				(progn
					(entupd e)
					aValue
				)
			)
			(bm:change-attribute-value e aTag aValue)
		)
	)
)

(defun bm:change-attribute-tag ( e aTag aValue / l )
	(if (= (em:type (setq l (entget (setq e (entnext e))))) "ATTRIB")
		(if (= (strcase (em:name l)) (strcase aTag))
			(if (entmod (subst (cons 2 aValue) (assoc 2 l) l))
				(progn
					(entupd e)
					aValue
				)
			)
			(bm:change-attribute-tag e aTag aValue)
		)
	)
)

(defun bm:swap-attribute-value ( e aTag1 aTag2 / aValue1 aValue2 )
	(setq 
		aValue1 (bm:get-attribute-value e aTag1)
		aValue2 (bm:get-attribute-value e aTag2)
	)
	
	(if (and aValue1 aValue2)
		(progn
			(bm:change-attribute-value e aTag1 aValue2)
			(bm:change-attribute-value e aTag2 aValue1)
		)
	)
)

(defun bm:shift-left-atrribute-values ( e l / i lTags lValues )
	(setq 
		lTags (mapcar 'car (reverse l))
		lValues (cons  ""  (reverse (cdr (mapcar  'cdr  l))))
	)
	
	(repeat (setq i (length l))
		(setq i (1- i))
		(bm:change-attribute-value e (nth i lTags) (nth i lValues))
	)
)

(defun bm:replace-block ( e a / l )
	(if (= (em:type (setq l (entget e))) "INSERT")
		(if (entmod (subst (cons 2 a) (assoc 2 l) l))
			(entupd e)
		)
	)
)


(princ)


