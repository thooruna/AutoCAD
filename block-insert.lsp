;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-insert ( xBlock aTag rRadius bLeader / a l p )
	(cm:initialize)
	(cm:setvar "ATTDIA" 0)
	(cm:setvar "ATTREQ" 0)
	(cm:setvar "AUTOSNAP" 63)
	(cm:setvar "POLARMODE" 0)
	(cm:setvar "POLARANG" (dtr 15))
	
	(if (bm:load xBlock)
		(cond
			((setq l (im:get-points xBlock))
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
		)
	)
	
	(cm:terminate)
	
	(if a (entlast))
)

(defun c:bsymbol ( / aBlock aTag l p x )
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
		x (1+ (bm:insert-attribute-max (bm:search (im:select-all-blocks) aBlock) aTag))
	)
	
	(if (setq e (block-insert aBlock aTag 5 T))
		(if (setq x (im:get-number "ID" x))
			(bm:change-attribute-value e aTag (sm:string-left-fill x "0" 2))
		)
	)
	
	(princ)
)

(defun c:psymbol ( / a aTag e l lBlock p x )
	(setq 
		lBlock '("SYMBOL-1" "SYMBOL-2")
		aTag "NUMBER"
		x (1+ (bm:insert-attribute-max (bm:search (im:select-all-blocks) lBlock) aTag))
	)
	
	(if (setq e (block-insert lBlock aTag 5 nil))
		(if (setq x (im:get-number "Number" x))
			(bm:change-attribute-value e aTag (sm:string-left-fill x "0" 4))
		)
	)
	
	(princ)
)
