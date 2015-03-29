;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun fm:drawing ()
	(getvar "DWGNAME")
)

(defun fm:drawing-base ()
	(vl-filename-base (getvar "DWGNAME"))
)

(defun fm:drawing-full ()
	(strcat (getvar "DWGPREFIX") (getvar "DWGNAME"))
)

(defun fm:drawing-path ()
	(getvar "DWGPREFIX")
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


