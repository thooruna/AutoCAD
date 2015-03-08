(defun c:balloon ( / )
	(defun SetLeaderStyle ( a )
		(if (tblsearch "DIMSTYLE" a) (command "_.-DIMSTYLE" "_R" a))
		
		(em:setvar "DIMASZ" 1)
		(em:setvar "DIMLDRBLK" "_DOT")
		
		(if (tblsearch "DIMSTYLE" a)
			(command "_.-DIMSTYLE" "_S" a "_Y")
			(command "_.-DIMSTYLE" "_S" a)
		)
	)
	
	(em:debug T)
	(em:ini)
	
	(em:setvar "ATTDIA" 0)
	(em:setvar "ATTREQ" 0)
	(em:setvar "AUTOSNAP" 63)
	(em:setvar "POLARMODE" 0)
	(em:setvar "POLARANG" (dtr 15))
	
	(setq
		aTag "ID"
		rRadius 5
	)
	
	
	(if (setq aBlock (bm:load "BALLOON"))
		(cond
			((setq l (im:get-points aBlock))
				(SetLeaderStyle "Balloon")
				
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
				
				(if (setq x (im:get-number "Text" x))
					(bm:change-attribute-value (entlast) aTag (sm:string-left-fill x "0" 2))
				)
			)
		)
	)
	
	(em:done)
)
