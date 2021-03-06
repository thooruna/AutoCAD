;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright � 2015

(defun block-insert-symbol|dialog ( aFolder / a p )
	(if (setq a (getfiled "Select a block: " (strcat (findfile aFolder) "\\") "dwg" 8))
		(if (bm:load a)
			(if (setq p (im:get-point nil))
				(bm:insert-symbol (fm:base a) p)
			)
		)
	)
)

(defun block-insert-symbol|single ( a p )
	(if (bm:load a)
		(if (setq p (im:get-point nil))
			(bm:insert-symbol (fm:base a) p)
		)
	)
)

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

(defun c:block-insert-update ( / e l )
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

(defun c:bsymbol ( / aBlock aTag e i )
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
		i (1+ (bm:get-attribute-max (bm:search-blocks-with-attributes aBlock (im:select-all-blocks)) aTag))
	)
	
	(if (setq e (block-insert aBlock 5 T))
		(if (= (cm:getvar "ATTREQ") 1)
			(if (setq i (im:get-number "ID" i))
				(bm:change-attribute-value e aTag (sm:string-fill|left (itoa i) "0" 2))
			)
		)
	)
	
	(cm:terminate)
)

(defun c:psymbol ( / aTag e i xBlocks )
	(cm:initialize)
	
	(setq 
		xBlocks "SYMBOL-1,SYMBOL-2"
		aTag "NUMBER"
		i (1+ (bm:get-attribute-max (bm:search-blocks-with-attributes xBlocks (im:select-all-blocks)) aTag))
	)
	
	(if (setq e (block-insert xBlocks 5 nil))
		(if (= (cm:getvar "ATTREQ") 1)
			(if (setq i (im:get-number "Number" i))
				(bm:change-attribute-value e aTag (sm:string-fill|left (itoa i) "0" 3))
			)
		)
	)
	
	(cm:terminate)
)

(princ)