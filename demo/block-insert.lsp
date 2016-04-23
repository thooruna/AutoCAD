;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-insert ( xBlocks rRadius bLeader / a l p )
	(cm:setvar "ATTDIA" 0)
	(cm:setvar "ATTREQ" 0)
	(cm:setvar "AUTOSNAP" 63)
	(cm:setvar "CLAYER" "0")
	(cm:setvar "POLARMODE" 0)
	(cm:setvar "POLARANG" (mm:degrees->radians 15))
	
	(if (bm:load xBlocks)
		(cond
			((setq l (im:get-points-with-keywords "replace" xBlocks))
				(setq
					a (getvar "USERS1")
					p (car l)
					l (append (list (polar (car l) (angle (car l) (cadr l)) (* rRadius (getvar "DIMSCALE")))) (cdr l))
				)
				
				(cm:setvar "AUTOSNAP" 0)
				(cm:setvar "OSNAP" 0)
				
				(if (> (length l) 1)
					(if bLeader
						(bm:insert-symbol-leader a p l)
						(bm:insert-symbol-extension-line a p l)
					)
					(bm:insert-symbol a p)
				)
			)
			(T (block-replace xBlocks))
		)
		(princ "\nUnable to find symbols.")
	)
	
	(if a (entlast))
)

(defun c:block-insert-update ( / l )
	(cm:initialize)
	
	(foreach e (im:select-all-blocks|nested&filter "BALLOON,SYMBOL*")
		(setq l (entget e))
		
		;;; Update symbol negative scale factors to uniform positive scale factors.
		(cond 
			((or (< (em:scale|X l) 0) (not (= (em:scale|X l) (em:scale|Y l) (em:scale|Z l))))
				(setq
					l (subst (cons '41 (abs (em:scale|X l))) (assoc 41 l) l)
					l (subst (cons '42 (em:scale|X l)) (assoc 42 l) l)
					l (subst (cons '43 (em:scale|X l)) (assoc 43 l) l)
				)
				(entmod l)
				(entupd e)
			)
		)
		
		;;; Update symbol layer.
		(cm:layer-change e bm:layer-symbol)
	)
	
	(cm:terminate)
)

(defun c:bsymbol ( / aBlock aTag e x )
	(cm:initialize)
	
	(defun SetLeaderStyle ( aBlock )
		(if (null (tblsearch "DIMSTYLE" aBlock))
			(command "_.DIMSCALE" pause)
		)
		
		(cm:setvar "DIMSTYLE" aBlock)
		(cm:setvar "DIMASZ" 1)
		(cm:setvar "DIMLDRBLK" "_DOT")
		
		(command "_.-DIMSTYLE" "_S" aBlock "_Y")
	)
	
	(SetLeaderStyle "Balloon")
	
	(setq 
		aBlock "BALLOON"
		aTag "ID"
		x (1+ (bm:get-attribute-max (bm:search-blocks-with-attributes aBlock (im:select-all-blocks)) aTag))
	)
	
	(if (setq e (block-insert aBlock 5 T))
		(if (= (cm:getvar "ATTREQ") 1)
			(if (setq x (im:get-number "ID" x))
				(bm:change-attribute-value e aTag (sm:string-fill|left x "0" 2))
			)
		)
	)
	
	(cm:terminate)
)

(defun c:psymbol ( / aTag e x xBlocks )
	(cm:initialize)
	
	(setq 
		xBlocks "SYMBOL-1,SYMBOL-2"
		aTag "NUMBER"
		x (1+ (bm:get-attribute-max (bm:search-blocks-with-attributes xBlocks (im:select-all-blocks)) aTag))
	)
	
	(if (setq e (block-insert xBlocks 5 nil))
		(if (= (cm:getvar "ATTREQ") 1)
			(if (setq x (im:get-number "Number" x))
				(bm:change-attribute-value e aTag (sm:string-fill|left x "0" 3))
			)
		)
	)
	
	(cm:terminate)
)

(princ)