;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

;;; Source for DXF codes: http://www.autodesk.com/techpubs/autocad/acad2000/dxf/group_codes_in_numerical_order_dxf_01.htm

;;; Return entity definition data

(defun em:edd ( x )
	(if (= (type x) 'ENAME) 
		(entget x) ; Get the entity's definition data.
		x ; This already is the entity's definition data.
	)
)

;;; APP: entity name reference (fixed)

(defun em:entity-name-reference ( e ) 
	(cdr (assoc -2 e))
)

;;; APP: entity name. The name changes each time a drawing is opened. It is never saved (fixed)

(defun em:entity-name ( e ) 
	(cdr (assoc -1 e)) 
)

;;; Text string indicating the entity type (fixed)

(defun em:type ( x )
	(cdr (assoc 0 (em:edd x)))
)

;;; Primary text value for an entity

(defun em:value ( x ) 
	(cdr (assoc 1 (em:edd x))) 
)

;;; Name (attribute tag, block name, and so on)

(defun em:name ( x / o a )
	(cond
		((and (= (em:type x) "INSERT") (member (strcase "vlax-ename->vla-object") (atoms-family 1)))
			(setq o (vlax-ename->vla-object x))
			(if (vlax-property-available-p o 'effectivename) ; Get the effective block name if available
				(setq a (vlax-get-property o "effectivename"))
			)
			(vlax-release-object o)
		)
		(T (setq a (cdr (assoc 2 (em:edd x)))))
	)
	
	a
)


;;; Entity handle; text string of up to 16 hexadecimal digits (fixed)

(defun em:handle ( x )
	(cdr (assoc 5 (em:edd x)))
)

;;; Layer name (fixed)

(defun em:layer ( x )
	(cdr (assoc 8 (em:edd x))) 
)

;;; Primary point. This is the start point of a line or text entity, center of a circle, and so on. 
;;; DXF: X value of the primary point (followed by Y and Z value codes 20 and 30)
;;; APP: 3D point (list of three reals)

(defun em:primary-point ( x )
	(cdr (assoc 10 (em:edd x)))
)

;;; Other points. 
;;; DXF: X value of other points (followed by Y value codes 21-28 and Z value codes 31-38)
;;; APP: 3D point (list of three reals)

(defun em:other-point ( x )
	(cdr (assoc 11 (em:edd x)))
)

;;; "Entities follow" flag (fixed)

(defun em:entities-follow ( x )
	(cdr (assoc 66 (em:edd x)))
)

;;; Line angle between the primairy point and the other point 

(defun em:line-angle ( x )
	(if (= (em:type x) "LINE")
		(angle (em:primary-point x) (em:other-point x))
		0
	)
) 