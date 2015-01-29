(defun bt:edd ( e ) ;- This function returns the entity's definition data.
	(if (= (type e) 'ENAME) 
		(entget e) ;- Get the entity's definition data.
		e ;- This already is the entity's definition data.
	)
)

(defun bt:entity-name-reference ( e ) 
	(cdr (assoc -2 e)) ;- APP: entity name reference (fixed)
)

(defun bt:entity-name ( e ) 
	(cdr (assoc -1 e)) ;- APP: entity name. The name changes each time a drawing is opened. It is never saved (fixed)
)

(defun bt:type ( e )
	(cdr (assoc 0 (bt:edd e))) ;- Text string indicating the entity type (fixed)
)

(defun bt:value ( e ) 
	(cdr (assoc 1 (bt:edd e))) ;- Primary text value for an entity
)

(defun bt:name ( e / b n )
	(cond
		((= (type e) 'ENAME)
			(setq b (vlax-ename->vla-object e))
			(if (vlax-property-available-p b 'effectivename) ;- Get the effective block name if available
				(setq n (vlax-get-property b "effectivename"))
			)
			(vlax-release-object b)
		)
		((= (type e) 'LIST)
			(setq n (cdr (assoc 2 e)))
		)
	)
	
	n
)

(defun bt:handle ( e )
	(cdr (assoc 5 (bt:edd e))) ;- Entity handle; text string of up to 16 hexadecimal digits (fixed)
)

(defun bt:layer ( e )
	(cdr (assoc 8 (bt:edd e))) ;- Layer name (fixed)
)

(defun bt:insertion-point ( e )
	(cdr (assoc 10 (bt:edd e))) ;- Primary point; this is the start point of a line or text entity, center of a circle, and so on
)

(defun bt:insert-attribute-values ( e )
	(mapcar 'cdr (bt:insert-attributes e))
)

(defun bt:insert-attribute-tags ( e )
	(mapcar 'car (bt:insert-attributes e))
)

(defun bt:insert-attributes ( e / l lAttributes )
	(setq lAttributes '())
	
	(while (and (setq l (entget (setq e (entnext e)))) (/= (bt:type l) "SEQEND"))
		(setq lAttributes (append lAttributes (list (cons (bt:name l) (bt:value l)))))
	)
	
	lAttributes
)

(defun bt:insert-has-attributes ( e )
	(= (cdr (assoc 66 (bt:edd e))) 1)
)

(defun bt:search ( s / e i lHandles)
	(setq lHandles '())
	
	(defun SearchNested ( e )
		(setq e (bt:entity-name-reference (tblsearch "BLOCK" (bt:name e))))
		(while e
			(SearchCurrent e)
			(setq e (entnext e))		
		)
	)

	(defun SearchCurrent ( e )
		(if (= (bt:type e) "INSERT")
			(if (bt:insert-has-attributes e)
				(setq lHandles (append lHandles (list (bt:handle e))))
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

(defun c:blist ( / l h e a )
	(setq l (bt:search (setq s (ssget "_X" '((0 . "INSERT"))))))

	(foreach h l
		(setq e (handent h))
		(princ "\n")
		(princ h)
		(princ " ")
		(princ (bt:name e))
		(princ " ")
		(foreach a (bt:insert-attribute-values e)
			(princ a)
			(princ " ")
		)
	)
)

(c:blist)


