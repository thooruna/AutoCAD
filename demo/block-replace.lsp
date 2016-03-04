;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun block-replace ( xBlocks / a e lEntities )
	(cm:initialize)
	
	(if (bm:load xBlocks)
		(if (setq lEntities (im:select-blocks))
			(if (setq a (im:get-keyword "\nSelect block" xBlocks))
				(if (member a (lm:x->list xBlocks))
					(foreach e lEntities
						(if (member (em:name e) (lm:x->list xBlocks))
							(bm:replace-block e a)
							(princ (strcat "\nUnable to replace blocks named: " (strcase (em:name e))))
						)
					)
					(princ (strcat "\nUnable to replace blocks with: " (strcase a)))
				)
			)
		)
		(princ "\nUnable to find symbols.")
	)
	
	(cm:terminate)
)

(defun c:preplace ( )
	(block-replace "SYMBOL-1,SYMBOL-2")
)