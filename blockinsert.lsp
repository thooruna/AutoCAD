(defun c:balloon ( / aBlock aTag l p rRadius x )
	(defun SetLeaderStyle ( aBlock )
		(if (null (tblsearch "DIMSTYLE" aBlock))
			(command "_.DIMSCALE" pause)
		)
		
		(em:setvar "DIMSTYLE" aBlock)
		(em:setvar "DIMASZ" 1)
		(em:setvar "DIMLDRBLK" "_DOT")
		
		(command "_.-DIMSTYLE" "_S" aBlock "_Y")
	)
	
	(em:debug T)
	(em:ini)
	(em:setvar "ATTDIA" 0)
	(em:setvar "ATTREQ" 0)
	(em:setvar "AUTOSNAP" 63)
	(em:setvar "POLARMODE" 0)
	(em:setvar "POLARANG" (dtr 15))
	
	(setq 
		aBlock "BALLOON"
		aTag "ID"
		rRadius 5
	)
	
	(SetLeaderStyle "Balloon")
	
	(if (bm:load aBlock)
		(cond
			((setq l (im:get-points aBlock))
				(setq 
					p (car l)
					l (append (list (polar (car l) (angle (car l) (cadr l)) (* rRadius (getvar "DIMSCALE")))) (cdr l))
					x (1+ (bm:insert-attribute-max (bm:search (im:select-all-blocks) aBlock) aTag))
				)
				
				(em:setvar "AUTOSNAP" 0)
				(em:setvar "OSNAP" 0)
				
				(if (> (length l) 1)
					(bm:insert-symbol-leader aBlock p l)
					(bm:insert-symbol aBlock p)
				)
				
				(if (setq x (im:get-number "Number" x))
					(bm:change-attribute-value (entlast) aTag (sm:string-left-fill x "0" 2))
				)
			)
		)
	)
	
	(em:done)
)

(defun c:psymbol ( / a aTag l lBlock p rRadius x )
	(em:debug T)
	(em:ini)
	(em:setvar "ATTDIA" 0)
	(em:setvar "ATTREQ" 0)
	(em:setvar "AUTOSNAP" 63)
	(em:setvar "POLARMODE" 0)
	(em:setvar "POLARANG" (dtr 15))
	
	(setq 
		lBlock '("SYMBOL-1" "SYMBOL-2")
		aTag "NUMBER"
		rRadius 5
	)
	
	(if (bm:load lBlock)
		(cond
			((setq l (im:get-points lBlock))
				(setq
					a (getvar "USERS1")
					p (car l)
					l (append (list (polar (car l) (angle (car l) (cadr l)) (* rRadius (getvar "DIMSCALE")))) (cdr l))
					x (1+ (bm:insert-attribute-max (bm:search (im:select-all-blocks) lBlock) aTag))
				)
				
				(em:setvar "AUTOSNAP" 0)
				(em:setvar "OSNAP" 0)
				
				(if (> (length l) 1)
					(bm:insert-symbol-leader a p l)
					(bm:insert-symbol a p)
				)
				
				(if (setq x (im:get-number "Number" x))
					(bm:change-attribute-value (entlast) aTag (sm:string-left-fill x "0" 4))
				)
			)
		)
	)
	
	(em:done)
)
