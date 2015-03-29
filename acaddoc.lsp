;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun LoadModules ( / aBase aFile aPath )
	(if (setq aPath (findfile "modules"))
		(foreach aBase '("block" "common" "entity" "file" "input" "list" "math" "string" "table" "xml")
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

(load "block-attributes.lsp")
(load "block-insert.lsp")
(load "block-list.lsp")
(load "block-table.lsp")
(load "block-xml.lsp")
(load "modify.lsp")
(load "settings.lsp")

(princ)