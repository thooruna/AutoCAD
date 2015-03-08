(defun LoadModules ( / aBase aFile aPath )
	(setq aPath (findfile "modules"))
	
	(if aPath
		(foreach aBase '("block" "common" "error" "file" "input" "list" "modify" "string" "table" "xml")
			(princ (strcat "\nLoading: " aBase ".lsp"))
			(if (setq aFile (findfile (strcat aPath "\\" aBase ".lsp")))
				(if (load aFile (strcat "\rUnable to load: "  aBase ".lsp"))
					(princ (strcat "\rLisp file loaded successfully. Module: " (strcase aBase)))
				)
				(princ (strcat "\rUnable to find: " aBase ".lsp"))
			)
		)
	)
)

(defun c:tt ( )
	(load "acaddoc.lsp")
)

(LoadModules)

(load "blockinsert.lsp")
(load "blocklist.lsp")
(load "blocktable.lsp")
(load "blockxml.lsp")

(princ)