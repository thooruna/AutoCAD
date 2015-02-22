(defun LoadModules ( / );aPath aBase aFile )
	(setq aPath (findfile "modules"))
	
	(if aPath
		(foreach aBase '("block" "error" "file" "input" "list" "string" "table" "xml")
			(if (setq aFile (findfile (strcat aPath "\\" aBase ".lsp")))
				(load aFile (strcat "\nUnable to load: " aFile))
				(princ (strcat "\nUnable to find: " aBase ".lsp"))
			)
		)
	)
)

(LoadModules)

(load "blocklist.lsp")
(load "blockxml.lsp")
(load "blocktable.lsp")
