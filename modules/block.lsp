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

(defun bm:name ( x / b n )
	(cond
		((= (type x) 'ENAME)
			(cond 
				((member (strcase "vlax-ename->vla-object") (atoms-family 1))
					(setq b (vlax-ename->vla-object x))
					(if (vlax-property-available-p b 'effectivename) ;- Get the effective block name if available
						(setq n (vlax-get-property b "effectivename"))
					)
					(vlax-release-object b)
				)
				(T 
					(setq n (cdr (assoc 2 (bm:edd (x)))))
				)
			)
		)
		((= (type x) 'LIST)
			(setq n (cdr (assoc 2 x)))
		)
	)
	
	n
)

(defun bm:handle ( x )
	(cdr (assoc 5 (bm:edd x))) ;- Entity handle; text string of up to 16 hexadecimal digits (fixed)
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

(defun bm:insert-attributes ( e / l lAttributes )
	(setq lAttributes '())
	
	(while (and (setq l (entget (setq e (entnext e)))) (/= (bm:type l) "SEQEND"))
		(setq lAttributes (append lAttributes (list (cons (bm:name l) (bm:value l)))))
	)
	
	lAttributes
)

(defun bm:insert-has-attributes ( x )
	(= (cdr (assoc 66 (bm:edd x))) 1)
)

(defun bm:all-attributes-length ( l / h e a i d1 d2 lAttributes )
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

(princ)

