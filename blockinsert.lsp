(defun blockinsert ( xBlock aTag rRadius bLeader / a l p )
	(em:ini)
	(em:setvar "ATTDIA" 0)
	(em:setvar "ATTREQ" 0)
	(em:setvar "AUTOSNAP" 63)
	(em:setvar "POLARMODE" 0)
	(em:setvar "POLARANG" (dtr 15))
	
	(if (bm:load xBlock)
		(cond
			((setq l (im:get-points xBlock))
				(setq
					a (getvar "USERS1")
					p (car l)
					l (append (list (polar (car l) (angle (car l) (cadr l)) (* rRadius (getvar "DIMSCALE")))) (cdr l))
				)
				
				(em:setvar "AUTOSNAP" 0)
				(em:setvar "OSNAP" 0)
				
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
	
	(em:done)
	
	(if a (entlast))
)

(defun c:bsymbol ( / aBlock aTag l p x )
	(defun SetLeaderStyle ( aBlock )
		(if (null (tblsearch "DIMSTYLE" aBlock))
			(command "_.DIMSCALE" pause)
		)
		
		(em:setvar "DIMSTYLE" aBlock)
		(em:setvar "DIMASZ" 1)
		(em:setvar "DIMLDRBLK" "_DOT")
		
		(command "_.-DIMSTYLE" "_S" aBlock "_Y")
	)
	
	(SetLeaderStyle "Balloon")
	
	(setq 
		aBlock "BALLOON"
		aTag "ID"
		x (1+ (bm:insert-attribute-max (bm:search (im:select-all-blocks) aBlock) aTag))
	)
	
	(if (setq e (blockinsert aBlock aTag 5 T))
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
	
	(if (setq e (blockinsert lBlock aTag 5 nil))
		(if (setq x (im:get-number "Number" x))
			(bm:change-attribute-value e aTag (sm:string-left-fill x "0" 4))
		)
	)
	
	(princ)
)
