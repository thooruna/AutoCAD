;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun c:shift-revisions ( / a e i l )
	(cm:initialize)
	
	(setq i '("1" "2" "3" "4" "5" "6"))
	
	(if (setq e (im:select-block))
		(if (wcmatch (strcase (em:name e)) "*TITLE*")
			(if (bm:insert-has-attributes e)
				(foreach a '("REVNO" "REVDESC" "REVBY" "REVDATE")
					(if (setq l (bm:insert-attributes-filter e (mapcar '(lambda (x) (strcat a x)) i)))
						(bm:shift-left-atrribute-values e l)
					)
				)
			)
		)
	)
	
	(cm:terminate)
)

