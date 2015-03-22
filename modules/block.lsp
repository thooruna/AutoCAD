(defun bm:edd ( x ) ;- This function returns the entity's definition data.
	(if (= (type x) 'ENAME) 
		(entget x) ;- Get the entity's definition data.
		x ;- This already is the entity's definition data.
	)
)

(defun bm:entity-name-reference ( e ) 
	(cdr (assoc -2 e)) ;- APP: entity name reference (fixed)
)

(defun bm:entity-name ( e ) 
	(cdr (assoc -1 e)) ;- APP: entity name. The name changes each time a drawing is opened. It is never saved (fixed)
)

(defun bm:type ( x )
	(cdr (assoc 0 (bm:edd x))) ;- Text string indicating the entity type (fixed)
)

(defun bm:value ( x ) 
	(cdr (assoc 1 (bm:edd x))) ;- Primary text value for an entity
)

(defun bm:name ( x / o a )
	(cond
		((and (= (bm:type x) "INSERT") (member (strcase "vlax-ename->vla-object") (atoms-family 1)))
			(setq o (vlax-ename->vla-object x))
			(if (vlax-property-available-p o 'effectivename) ;- Get the effective block name if available
				(setq a (vlax-get-property o "effectivename"))
			)
			(vlax-release-object o)
		)
		(T (setq a (cdr (assoc 2 (bm:edd x)))))
	)
	
	a
)

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
	(setq l (mapcar 'bm:find (lm:to-list xBase)))
	
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
	(apply 'command (append '("_.LEADER") (reverse l) '("_A" "" "_B" a p (getvar "DIMSCALE") 0.0)))
	(command "_.REDRAW")
)

(defun bm:insert-symbol-extension-line ( a p l )
	(apply 'command (append '("_.PLINE") (reverse l) '("")))
	(bm:insert-symbol a p)
	(command "_.REDRAW")
)

(defun bm:handle ( x )
	(cdr (assoc 5 (bm:edd x))) ;- Entity handle; text string of up to 16 hexadecimal digits (fixed)
)

(defun bm:handle-lengths ( l )
	(apply 'max (mapcar 'strlen l))
)

(defun bm:layer ( x )
	(cdr (assoc 8 (bm:edd x))) ;- Layer name (fixed)
)

(defun bm:insertion-point ( x )
	(cdr (assoc 10 (bm:edd x))) ;- Primary point; this is the start point of a line or text entity, center of a circle, and so on
)

(defun bm:insert-attribute-values ( e )
	(mapcar 'cdr (bm:insert-attributes e))
)

(defun bm:insert-attribute-tags ( e )
	(mapcar 'car (bm:insert-attributes e))
)

(defun bm:insert-attributes ( e )
	(if (= (bm:type (setq e (entnext e))) "ATTRIB")
		(cons (cons (strcase (bm:name e)) (bm:value e)) (bm:insert-attributes e))
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
	(= (cdr (assoc 66 (bm:edd x))) 1)
)

(defun bm:search ( s x / e i lHandles )
	(setq lHandles '())
	
	(defun SearchNested ( e )
		(setq e (bm:entity-name-reference (tblsearch "BLOCK" (bm:name e))))
		(while e
			(SearchCurrent e)
			(setq e (entnext e))
		)
	)

	(defun SearchCurrent ( e )
		(if (= (bm:type e) "INSERT") 
			(if (bm:insert-has-attributes e)
				(if (wcmatch (bm:name e) x)
					(setq lHandles (append lHandles (list (bm:handle e))))
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
	(if (= (bm:type (setq e (entnext e))) "ATTRIB")
		(if (= (strcase (bm:name e)) (strcase aTag))
			(bm:value e)
			(bm:get-attribute-value e aTag)
		)
	)
)

(defun bm:change-attribute-value ( e aTag aValue / l )
	(if (= (bm:type (setq l (entget (setq e (entnext e))))) "ATTRIB")
		(if (= (strcase (bm:name l)) (strcase aTag))
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
	(if (= (bm:type (setq l (entget (setq e (entnext e))))) "ATTRIB")
		(if (= (strcase (bm:name l)) (strcase aTag))
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

(defun bm:replace-block ( e a / l )
	(if (= (bm:type (setq l (entget e))) "INSERT")
		(if (entmod (subst (cons 2 a) (assoc 2 l) l))
			(entupd e)
		)
	)
)


(princ)


