(defun bm:edd ( e ) ;- This function returns the entity's definition data.
	(if (= (type e) 'ENAME) 
		(entget e) ;- Get the entity's definition data.
		e ;- This already is the entity's definition data.
	)
)

(defun bm:entity-name-reference ( e ) 
	(cdr (assoc -2 e)) ;- APP: entity name reference (fixed)
)

(defun bm:entity-name ( e ) 
	(cdr (assoc -1 e)) ;- APP: entity name. The name changes each time a drawing is opened. It is never saved (fixed)
)

(defun bm:type ( e )
	(cdr (assoc 0 (bm:edd e))) ;- Text string indicating the entity type (fixed)
)

(defun bm:value ( e ) 
	(cdr (assoc 1 (bm:edd e))) ;- Primary text value for an entity
)

(defun bm:name ( e / b n )
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

(defun bm:handle ( e )
	(cdr (assoc 5 (bm:edd e))) ;- Entity handle; text string of up to 16 hexadecimal digits (fixed)
)

(defun bm:layer ( e )
	(cdr (assoc 8 (bm:edd e))) ;- Layer name (fixed)
)

(defun bm:insertion-point ( e )
	(cdr (assoc 10 (bm:edd e))) ;- Primary point; this is the start point of a line or text entity, center of a circle, and so on
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

(defun bm:insert-has-attributes ( e )
	(= (cdr (assoc 66 (bm:edd e))) 1)
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

(defun bm:search ( ss s / e i lHandles)
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
				(if (wcmatch (bm:name e) s)
					(setq lHandles (append lHandles (list (bm:handle e))))
				)
				(SearchNested e)
			)
		)
	)

	(repeat (setq i (sslength ss))
		(setq e (ssname ss (setq i (1- i))))
		(SearchCurrent e)
	)
	
	lHandles
)

(defun c:blist ( s / ss h d lHandles lColumns lAttributes )
	(if (setq ss (ssget "_X" '((0 . "INSERT"))))
		(setq lHandles (bm:search ss s))
	)
	
	(defun PrintDivider ()
		(princ (sm:string-right-fill "\n" "=" (+ 8 (length lColumns) (apply '+ (mapcar 'cdr lColumns)))))
	)

	(defun PrintHeader ()
		(princ "\nHandle|")
	
		(foreach d lColumns
			(princ (sm:string-right-fill (car d) " " (cdr d)))
			(princ "|")
		)
	)
	
	(defun PrintData ()
		(foreach h lHandles
			(setq lAttributes (bm:insert-attributes (handent h)))
		
			(princ "\n")
			(princ h)
			(princ "   |")
		
			(foreach d lColumns
				(princ (sm:string-right-fill (cdr (assoc (car d) lAttributes)) " " (cdr (assoc (car d) lColumns))))
				(princ "|")
			)
		)
	)
		
	(setq lColumns (bm:all-attributes-length lHandles))
	
	(PrintDivider)
	(PrintHeader)
	(PrintDivider)
	(PrintData)
	(PrintDivider)
	
	(princ)
)


