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

(defun bm:insert-attribute-lengths ( l / h e a i d1 d2 lAttributes )
	(setq lAttributes '())
	
	(foreach h l
		(setq e (handent h))
		(foreach a (bm:insert-attribute-tags e)
			(if (not (assoc a lAttributes))
				(setq lAttributes (append lAttributes (list (cons a (strlen a)))))
			)
		)
	)
	
	(foreach h l
		(setq e (handent h))
		(foreach d1 (bm:insert-attributes e)
			(setq d2 (assoc (car d1) lAttributes))
			(if (> (setq i (strlen (cdr d1))) (cdr d2))
				(setq lAttributes (subst (cons (car d1) i) d2 lAttributes))
			)
		)
	)
	
	lAttributes
)

(defun bm:insert-attributes ( e )
	(if (= (bm:type (setq e (entnext e))) "ATTRIB")
		(cons (cons (bm:name e) (bm:value e)) (bm:insert-attributes e))
	)
)

(defun bm:insert-has-attributes ( x )
	(= (cdr (assoc 66 (bm:edd x))) 1)
)

(defun bm:search ( s a / e i lHandles )
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
				(if (wcmatch (bm:name e) a)
					(setq lHandles (append lHandles (list (bm:handle e))))
				)
				(SearchNested e)
			)
		)
	)

	(repeat (setq i (sslength s))
		(setq e (ssname s (setq i (1- i))))
		(SearchCurrent e)
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


