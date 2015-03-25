(defun em:edd ( x ) ;- This function returns the entity's definition data.
	(if (= (type x) 'ENAME) 
		(entget x) ;- Get the entity's definition data.
		x ;- This already is the entity's definition data.
	)
)

(defun em:entity-name-reference ( e ) 
	(cdr (assoc -2 e)) ;- APP: entity name reference (fixed)
)

(defun em:entity-name ( e ) 
	(cdr (assoc -1 e)) ;- APP: entity name. The name changes each time a drawing is opened. It is never saved (fixed)
)

(defun em:type ( x )
	(cdr (assoc 0 (em:edd x))) ;- Text string indicating the entity type (fixed)
)

(defun em:value ( x ) 
	(cdr (assoc 1 (em:edd x))) ;- Primary text value for an entity
)

(defun em:name ( x / o a )
	(cond
		((and (= (em:type x) "INSERT") (member (strcase "vlax-ename->vla-object") (atoms-family 1)))
			(setq o (vlax-ename->vla-object x))
			(if (vlax-property-available-p o 'effectivename) ;- Get the effective block name if available
				(setq a (vlax-get-property o "effectivename"))
			)
			(vlax-release-object o)
		)
		(T (setq a (cdr (assoc 2 (em:edd x)))))
	)
	
	a
)

