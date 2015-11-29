;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper (unless stated otherwise)
;;; Copyright © 2015

;;; Source: http://mdouglas.blogs.com/in_the_dynamic_interface/2005/07/dynamic_lisp_co.html

(defun dm:set-property ( oProps sProp Val / i oSBReferenceProperty sPName iFound)
	(setq 
		i (vlax-safearray-get-l-bound oProps 1)
		iFound 0
	)
	(while (and (<= i (vlax-safearray-get-u-bound oProps 1)) (= iFound 0))
		(setq oSBReferenceProperty (vlax-safearray-get-element oProps i))
		(setq sPName (vla-get-PropertyName oSBReferenceProperty))
		(if (= (strcase sPName) sProp)
			(progn
				(print (strcat "Old value of " sPName "="))
				(princ (vlax-variant-value (vla-get-value oSBReferenceProperty)))
				(vla-put-value oSBReferenceProperty 
					(vlax-make-variant Val
						(vlax-variant-type (vla-get-value oSBReferenceProperty))
					)
				)
				(print "New value=")
				(princ (vlax-variant-value (vla-get-value oSBReferenceProperty)))
				(setq iFound 1)
			)
		)
	
		(setq i (1+ i))
	)
	(princ)
)

(defun dm:modify-property ( lstProp / ss index cnt oBkRef oProps i j oSBReferenceProperty )
	(vl-load-com)
	(setq 
		ss (ssget "_L")
		index 0
		cnt (sslength ss)
	)
	
	(while (< index cnt)
		(setq e (ssname ss index))
		(setq oBkRef (vlax-ename->vla-object e))
		
		(setq oProps (vlax-variant-value (vla-GetDynamicBlockProperties oBkRef)))
		
		(setq i (vlax-safearray-get-l-bound oProps 1))
		(while (<= i (vlax-safearray-get-u-bound oProps 1))
			(setq oSBReferenceProperty (vlax-safearray-get-element oProps i))
			(print (strcat (vla-get-PropertyName oSBReferenceProperty) "="))
			(princ (vlax-variant-value (vla-get-value oSBReferenceProperty)))
			(setq i (1+ i))
		)
		
		(setq j 0)
		(while (< j (length lstProp))
			(setq sProp (strcase (nth j lstProp)))
			(dm:set-property oProps sProp (nth (+ 1 j) lstProp))
			(setq j (+ 2 j))
		) ;while < j (length lstProp)
		
		(setq index (1+ index))
	) ;while < index cnt
	
	(princ)
)

(princ)