;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun c:ll ( ) (load "acaddoc.lsp"))

(defun LoadFile ( aFile )
	(princ (strcat "\nLoading: " aFile))
	(if (setq aFile (findfile aFile))
		(if (load aFile (strcat "\rUnable to load: " aFile))
			(princ (strcat "\rLisp file loaded successfully. File: " aFile))
		)
		(princ (strcat "\rUnable to find: " aFile))
	)
)

(defun LoadModules ( l / aBase aPath )
	(if (setq aPath (findfile "modules"))
		(foreach aBase l (LoadFile (strcat aPath "\\" aBase ".lsp")))
		(princ (strcat "\nUnable to find modules folder."))
	)
)

(LoadModules '("block" "common" "dynamic" "entity" "file" "input" "list" "math" "string" "table" "xdata" "xml"))

(LoadFile "demo\\block-attributes.lsp")
(LoadFile "demo\\block-insert.lsp")
(LoadFile "demo\\block-list.lsp")
(LoadFile "demo\\block-replace.lsp")
(LoadFile "demo\\block-table.lsp")
(LoadFile "demo\\block-xml.lsp")

(LoadFile "demo\\lasertext.lsp")

(LoadFile "modify.lsp")
(LoadFile "settings.lsp")

;;; Change the drawing visual style. The '2D Wireframe' visual style causes issues on 2D drawings with block tables. Mainly in the first drawing.
;;;
;;; See the following link for similar problems:
;;; https://forums.autodesk.com/t5/autocad-2013-2014-2015-2016/hide-command-ram-error-autocad-2016-any-fix-coming/td-p/5782607

(command "_.VSCURRENT"  "W")
(command "_.VIEWRES" "_Y" 2000)

(princ)