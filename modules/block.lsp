;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright � 2015

(setq
	bm:layer-symbol "0"
	bm:layer-extension-line "0"
	bm:custom-folders "custom\\blocks,custom\\symbols,demo\\blocks,demo\\symbols"
)

(defun bm:preload ( a )
	(command "_.-INSERT" a)
	(command) ;- Break out of insert command
	(fm:base a)
)

(defun bm:redefine ( a / aBase)
	(command "_.INSERT" (strcat (setq aBase (fm:base a)) "=" a)) 
	(command) ;;; Break out of insert command
	(command-s "_.ATTSYNC" "N" aBase)
	aBase
)

(defun bm:list ( / a l )
	(setq a (tblnext "BLOCK" T))
	
	(while a
		(setq 
			l (cons (em:name a) l)
			a (tblnext "BLOCK")
		)
	)
	
	l
)

(defun bm:exists|drawing ( a / aBase )
	(if (tblsearch "BLOCK" (setq aBase (fm:base a)))
		aBase
	)
)

(defun bm:exists|file ( a / aPath l )
	(if (/= (fm:extension a) ".dwg") 
		(setq a (strcat a ".dwg"))
	)
	
	(cond
		((findfile a))
		((car
			(vl-remove
				nil
				(mapcar 
					'findfile 
					(mapcar 
						'(lambda (x) 
							(strcat x "\\" a)
						)
						(vl-remove
							nil 
							(mapcar 
								'findfile 
								(lm:x->list bm:custom-folders)
							)
						)
					)
				)
			)
		))
		(T 
			(princ (strcat "\nUnable to find: " a))
			(princ "\n")
			nil
		)
	)
)

;;; Use bm:load before any insert functions.

(defun bm:load ( x / a l )
	(foreach a (setq l (lm:x->list x))
		(if (not (bm:exists|drawing a))
			(if (setq a (bm:exists|file a))
				(bm:preload a)
			)
		)
	)
	
	(apply 'and (mapcar 'bm:exists|drawing l))
)

(defun bm:is-uniformly-scaled ( aBase / l )
	(if (setq l (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))))
		(= (vlax-get (vla-item l aBase) 'BlockScaling) 1)
	)
)

(defun bm:insert ( aBase p )
	(if (bm:is-uniformly-scaled aBase)
		(command-s "_.-INSERT" aBase p 1 0.0)
		(command-s "_.-INSERT" aBase p 1 1 0.0)
	)
)

(defun bm:insert-symbol ( aBase p )
	(cm:layer-activate bm:layer-symbol)
	(if (bm:is-uniformly-scaled aBase)
		(command-s "_.-INSERT" aBase p (getvar "DIMSCALE") 0.0)
		(command-s "_.-INSERT" aBase p (getvar "DIMSCALE") (getvar "DIMSCALE") 0.0)
	)
)

(defun bm:insert-symbol-leader ( aBase p l )
	(cm:layer-activate bm:layer-symbol)
	(apply 'command (append '("_.LEADER") (reverse l) (list "_A" "" "_B" aBase p (getvar "DIMSCALE") 0.0)))
	(command "_.REDRAW")
	(entlast)
)

(defun bm:insert-symbol-extension-line ( aBase p l / e )
	
	(defun DrawExtensionLine ( l )
		(cm:layer-activate bm:layer-extension-line)
		(apply 'command (append '("_.PLINE") (reverse l) '("")))
		(entlast)
	)
	
	(setq e (DrawExtensionLine l))
	(bm:insert-symbol aBase p)
	(if (equal p (car l)) (command "_.TRIM" "" (list e (polar p (angle (car l) (cadr l)) 0.001)) ""))
	(command "_.REDRAW")
	e
)

(defun bm:handle-lengths ( lEntities )
	(apply 'max (mapcar 'strlen (mapcar 'em:handle lEntities)))
)

(defun bm:insertion-point ( x )
	(em:primary-point x)
)

(defun bm:get-attribute-value ( e xTags )
	(if (lm:is-list xTags) (setq xTags (lm:x->string xTags)))
	
	(if (= (em:type (setq e (entnext e))) "ATTRIB")
		(if (sm:string-match (em:name e) xTags)
			(em:value e)
			(bm:get-attribute-value e xTags)
		)
	)
)

(defun bm:get-attribute-values ( e )
	(mapcar 'cdr (bm:get-attributes e))
)

(defun bm:get-attribute-tags ( e )
	(mapcar 'car (bm:get-attributes e))
)

(defun bm:get-attribute-tags|all ( lEntities / e aTag lAttributes )
	(foreach e lEntities
		(foreach aTag (bm:get-attribute-tags e)
			(if (not (member aTag lAttributes))
				(setq lAttributes (cons aTag lAttributes))
			)
		)
	)
	
	(reverse lAttributes)
)

(defun bm:get-attributes ( e )
	(if (= (em:type (setq e (entnext e))) "ATTRIB")
		(cons (cons (strcase (em:name e)) (em:value e)) (bm:get-attributes e))
	)
)

(defun bm:get-attributes|include ( e xTags )
	(if (lm:is-list xTags) (setq xTags (lm:x->string xTags)))
	
	(if (= (em:type (setq e (entnext e))) "ATTRIB")
		(if (sm:string-match (em:name e) xTags)
			(cons (cons (strcase (em:name e)) (em:value e)) (bm:get-attributes|include e xTags))
			(bm:get-attributes|include e xTags)
		)
	)
)

(defun bm:get-attributes|exclude ( e xTags )
	(if (lm:is-list xTags) (setq xTags (lm:x->string xTags)))
	
	(if (= (em:type (setq e (entnext e))) "ATTRIB")
		(if (null (sm:string-match (em:name e) xTags))
			(cons (cons (strcase (em:name e)) (em:value e)) (bm:get-attributes|exclude e xTags))
			(bm:get-attributes|exclude e xTags)
		)
	)
)

(defun bm:get-attribute-lengths ( lEntities / e aTag lAttributes )
	(foreach e lEntities
		(foreach aTag (bm:get-attribute-tags e)
			(if (not (assoc aTag lAttributes))
				(setq lAttributes (cons (cons aTag (max (strlen aTag) (bm:get-attribute-max-length lEntities aTag))) lAttributes))
			)
		)
	)
	
	(reverse lAttributes)
)

(defun bm:get-attribute-max ( lEntities xAttributes / iMax )
	(cond
		(lEntities (apply 'max (mapcar '(lambda ( x ) (atoi (bm:get-attribute-value x xAttributes))) lEntities)))
		(T 0)
	)
)

(defun bm:get-attribute-max-length ( lEntities xAttributes )
	(cond
		(lEntities (apply 'max (mapcar '(lambda ( x ) (sm:string-length (bm:get-attribute-value x xAttributes))) lEntities)))
		(T 0)
	)
)

(defun bm:get-id ( e )
	(list 
		(cons "HANDLE" (em:handle e)) 
		(cons "NAME" (em:name e)) 
		(cons "X" (em:primary-point|X e)) 
		(cons "Y" (em:primary-point|Y e)) 
		(cons "Z" (em:primary-point|Z e))
	)
)

(defun bm:has-attributes ( x )
	(= (em:entities-follow x) 1)
)

(defun bm:search-blocks ( xFilter x / e l )
	(defun SearchNested ( e )
		(while e
			(SearchCurrent e)
			(setq e (entnext e))
		)
	)
	
	(defun SearchCurrent ( e )
		(if (= (em:type e) "INSERT") 
			(if (wcmatch (strcase (em:name e)) (strcase xFilter)) (setq l (cons e l))
				(SearchNested (em:entity-name-reference (tblsearch "BLOCK" (em:name e))))
			)
		)
	)
	
	(setq xFilter (strcase (lm:x->string xFilter)))
	
	(foreach e (lm:x->list x)
		(SearchCurrent e)
	)
	
	l
)

(defun bm:search-blocks-with-attributes|all ( xFilter x / e l )
	(defun SearchNested ( e )
		(while e
			(SearchCurrent e)
			(setq e (entnext e))
		)
	)
	
	(defun SearchCurrent ( e )
		(if (= (em:type e) "INSERT") 
			(if (bm:has-attributes e)
				(if (wcmatch (strcase (em:name e)) (strcase xFilter)) (setq l (cons e l)))
				(SearchNested (em:entity-name-reference (tblsearch "BLOCK" (em:name e))))
			)
		)
	)
	
	(setq xFilter (strcase (lm:x->string xFilter)))
	
	(foreach e (lm:x->list x)
		(SearchCurrent e)
	)
	
	l
)

(defun bm:search-blocks-with-attributes|two-levels ( xFilter x / e l )
	(defun SearchNested ( e )
		(while e
			(if (and (= (em:type e) "INSERT")  (bm:has-attributes e)) (SearchCurrent e))
			(setq e (entnext e))
		)
	)
	
	(defun SearchCurrent ( e )
		(if (wcmatch (strcase (em:name e)) (strcase xFilter)) (setq l (cons e l)))
	)
	
	(setq xFilter (strcase (lm:x->string xFilter)))
	
	(foreach e (lm:x->list x)
		(if (bm:has-attributes e)
			(SearchCurrent e)
			(SearchNested (em:entity-name-reference (tblsearch "BLOCK" (em:name e))))
		)
	)
	
	l
)

(defun bm:search-blocks-with-attributes ( xFilter x / e l )
	(setq xFilter (strcase (lm:x->string xFilter)))
	
	(foreach e (lm:x->list x)
		(if (bm:has-attributes e)
			(if (wcmatch (strcase (em:name e)) (strcase xFilter)) (setq l (cons e l)))
		)
	)
	
	l
)

(defun bm:change-attribute-value ( e xTags aValue / l )
	(if (lm:is-list xTags) (setq xTags (lm:x->string xTags)))
	
	(if (= (em:type (setq l (entget (setq e (entnext e))))) "ATTRIB")
		(if (sm:string-match (em:name l) xTags)
			(if (entmod (subst (cons 1 aValue) (assoc 1 l) l))
				(progn
					(entupd e)
					aValue
				)
			)
			(bm:change-attribute-value e xTags aValue)
		)
	)
)

(defun bm:change-attribute-tag ( e xTags aValue / l )
	(if (lm:is-list xTags) (setq xTags (lm:x->string xTags)))
	
	(if (= (em:type (setq l (entget (setq e (entnext e))))) "ATTRIB")
		(if (= (strcase (em:name l)) (strcase xTags))
			(if (entmod (subst (cons 2 aValue) (assoc 2 l) l))
				(progn
					(entupd e)
					aValue
				)
			)
			(bm:change-attribute-tag e xTags aValue)
		)
	)
)

(defun bm:change-attributes ( e lAttributes / dAttribute )
	(foreach dAttribute lAttributes
		(bm:change-attribute-value e (car dAttribute) (cdr dAttribute))
	)
)

(defun bm:swap-attribute-value ( e aTag1 aTag2 / aValue1 aValue2 )
	(setq 
		aValue1 (bm:get-attribute-value e aTag1)
		aValue2 (bm:get-attribute-value e aTag2)
	)
	
	(if (and aValue1 aValue2)
		(progn
			(bm:change-attribute-value e aTag1 aValue2)
			(bm:change-attribute-value e aTag2 aValue1)
		)
	)
)

(defun bm:shift-atrribute-values|left ( e l / i lTags lValues )
	(setq 
		lTags (mapcar 'car (reverse l))
		lValues (cons "" (reverse (cdr (mapcar 'cdr  l))))
	)
	
	(repeat (setq i (length l))
		(setq i (1- i))
		(bm:change-attribute-value e (nth i lTags) (nth i lValues))
	)
)

(defun bm:replace-block ( e a / l )
	(if (= (em:type (setq l (entget e))) "INSERT")
		(if (entmod (subst (cons 2 a) (assoc 2 l) l))
			(entupd e)
		)
	)
)

(defun bm:move-block ( e p / l )
	(if (= (em:type (setq l (entget e))) "INSERT")
		(if (entmod (subst (cons 10 (mapcar '+ (em:primary-point l) p)) (assoc 10 l) l))
			(entupd e)
		)
	)
)

(princ)


