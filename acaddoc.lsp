(defun LoadModules ( / a1 a2 a3 )
	(setq a1 (findfile "modules"))
	
	(if a1
		(foreach a2 '("block" "file" "list" "string" "xml")
			(if (setq a3 (findfile (strcat a1 "\\" a2 ".lsp")))
				(load a3)
				(princ (strcat "\nUnable to load: " a2 ".lsp"))
			)
		)
	)
)

(LoadModules)

(load "blist.lsp")
(load "bxml.lsp")
