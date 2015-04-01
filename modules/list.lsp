;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun lm:x->list ( x )
	(if (= (type x) 'STR)
		(list x)
		x
	)
)

(defun lm:is-list ( x )
	(= (type x) 'LIST)
)

;;-------------------=={ List Difference }==------------------;;
;;                                                            ;;
;;  Returns items appearing exclusively in one list but not   ;;
;;  another, i.e. the relative complement: l1 \ l2            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  l1,l2 - lists for which to return the difference          ;;
;;------------------------------------------------------------;;
;;  Returns:  List of items appearing exclusively in list l1  ;;
;;------------------------------------------------------------;;

(defun lm:diff ( l1 l2 ) ;- LM:ListDifference renamed to lm:diff
	(vl-remove-if '(lambda ( x ) (member x l2)) l1)
)

;; Unique  -  Lee Mac
;; Returns a list with duplicate elements removed.

(defun lm:unique ( l ) ;- LM:unique remand to lm:unique
	(if l (cons (car l) (lm:unique (vl-remove (car l) (cdr l)))))
)

;; String to List  -  Lee Mac
;; Separates a string using a given delimiter
;; str - [str] String to process
;; del - [str] Delimiter by which to separate the string
;; Returns: [lst] List of strings
 
(defun lm:str->lst ( str del / pos ) ;- LM:str->lst remand to lm:str->lst
	(if (setq pos (vl-string-search del str))
		(cons (substr str 1 pos) (lm:str->lst (substr str (+ pos 1 (strlen del))) del))
		(list str)
	)
)

;; List to String  -  Lee Mac
;; Concatenates each string in a supplied list, separated by a given delimiter
;; lst - [lst] List of strings to concatenate
;; del - [str] Delimiter string to separate each item

(defun lm:lst->str ( lst del ) ;- LM:lst->str renamed to lm:lst->str
	(if (cdr lst)
		(strcat (car lst) del (lm:lst->str (cdr lst) del))
		(car lst)
	)
)

(princ)
