(defun LoadModules ( / a b c )
	(setq a (findfile "modules"))
	
	(if a
		(foreach b '("string" "block")
			(if (setq c (findfile (strcat a "\\" b ".lsp")))
				(load c)
				(princ (strcat "\nUnable to load: " b ".lsp"))
			)
		)
	)
)

(LoadModules)

(load "blist.lsp")