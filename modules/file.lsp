;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun fm:findfile ( a )
	(if (setq a (findfile a)) a "")
)

(defun fm:base ( a )
	(vl-filename-base a)
)

(defun fm:extension ( a )
	(if (setq a (strcase (vl-filename-extension a) T)) a "")
)

(defun fm:path ( a )
	(vl-filename-directory a)
)

(defun fm:drawing ( / )
	(getvar "DWGNAME")
)

(defun fm:drawing-base ( / )
	(vl-filename-base (getvar "DWGNAME"))
)

(defun fm:drawing-extension ( / )
	(vl-filename-extension (getvar "DWGNAME"))
)

(defun fm:drawing-path ( / )
	(getvar "DWGPREFIX")
)

(defun fm:drawing-full ( / )
	(strcat (getvar "DWGPREFIX") (getvar "DWGNAME"))
)

(defun fm:read-file ( aFile / aContents aLine f )
	(setq aContents "")
		
		
	(if (findfile aFile)
		(if (setq f (open aFile "r"))
			(while (setq aLine (read-line f))
				(setq aContents (strcat aContents "\n" aLine))
			)
		)
	)
	
	aContents
)

;(defun fm:open-text-file ( a m )
;	(open a m) ;- m for mode: "w" (write), "r" (read) or "a" (append)
;)

;(defun fm:write-line ( a f )
;	(write-line a f)
;)

;(defun fm:close-text-file ( f )
;	(close f)
;)

(princ)


