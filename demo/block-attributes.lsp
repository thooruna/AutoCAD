;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun update-titleblocks ( eSource / eDestination lAttributes lEntities )
	(if (setq lAttributes (bm:get-attributes|exclude eSource "UNITS,SHEET"))
		(if (setq lEntities (im:select-blocks-filter (em:name eSource)))
			(foreach eDestination lEntities
				(bm:change-attributes eDestination lAttributes)
			)
		)
	)
)

(defun c:shift-revisions ( / e aTag aNumber l )
	(cm:initialize)
	
	(setq aNumber '("1" "2" "3" "4" "5" "6"))
	
	(if (setq e (im:select-block))
		(if (wcmatch (strcase (em:name e)) "*TITLE*")
			(if (bm:has-attributes e)
				(progn
					(foreach aTag '("REVNO" "REVDESC" "REVBY" "REVDATE")
						(if (setq l (bm:get-attributes|include e (mapcar '(lambda (x) (strcat aTag x)) aNumber)))
							(bm:shift-left-atrribute-values e l)
						)
					)
					(update-titleblocks e)
				)
				(princ "\nThat block has no editable attributes.")
			)
			(princ "\nThat block is not a titleblock.")
		)
	)
	
	(cm:terminate)
)

(defun c:edit-titleblock ( / e )
	(cm:initialize)
	
	(if (setq e (im:select-block))
		(if (wcmatch (strcase (em:name e)) "*TITLE*")
			(if (bm:has-attributes e)
				(progn
					(command-s "_.EATTEDIT" (ssadd e))
					(update-titleblocks e)
				)
				(princ "\nThat block has no editable attributes.")
			)
			(princ "\nThat block is not a titleblock.")
		)
	)
	
	(cm:terminate)
)

