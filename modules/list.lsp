;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper (unless stated otherwise)
;;; Copyright © 2015

(defun lm:x->list ( x / i l )
	(cond
		((= (type x) 'PICKSET)
			(repeat (setq i (sslength x))
				(setq l (cons (ssname x (setq i (1- i))) l))
			)
		)
		((= (type x) 'STR)
			(lm:string->list x ",")
		)
		(x)
	)
)

(defun lm:x->string ( x / i l )
	(cond
		((= (type x) 'LIST)
			(lm:list->string x ",")
		)
		(x)
	)
)

(defun lm:is-list ( x )
	(= (type x) 'LIST)
)

(defun lm:add-numbering ( l / i )
	(mapcar '(lambda (x) (cons x (setq i (if i (1+ i) 0)))) l)
)

;;; Multiple Asssoc

(defun lm:assoc ( x l1 / d l2 )
	(while (setq d (assoc x l1))
		(setq 
			l1 (cdr (member d l1))
			l2 (cons d l2)
		)
	)
	
	(reverse l2)
)

(defun lm:assoc|values ( x l )
	(mapcar 'cdr (lm:assoc x l))
)

(defun lm:nth ( x l1 / i l2 )
	(if (setq l2 (member x (reverse l1)))
		(1- (length l2))
	)
)

;;; Insert Nth  -  Lee Mac
;;; Inserts an item at the nth position in a list.
;;; x - [any] Item to be inserted
;;; n - [int] Zero-based index at which to insert item
;;; l - [lst] List in which item is to be inserted

(defun lm:insert-nth ( x n l ) ; LM:insertnth renamed to lm:insert-nth
	(cond
		((null l) nil)
		((< 0  n) (cons (car l) (lm:insert-nth x (1- n) (cdr l))))
		((cons x l))
	)
)

;;;----------------------=={ Remove Nth }==--------------------;;;
;;;                                                            ;;;
;;;  Removes the item at the nth index in a supplied list      ;;;
;;;------------------------------------------------------------;;;
;;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;;
;;;------------------------------------------------------------;;;
;;;  Arguments:                                                ;;;
;;;  n - index of item to remove (zero based)                  ;;;
;;;  l - list from which item is to be removed                 ;;;
;;;------------------------------------------------------------;;;
;;;  Returns:  List with item at index n removed               ;;;
;;;------------------------------------------------------------;;;

(defun lm:remove-nth ( n l ) ; LM:RemoveNth renamed to lm:remove-nth
	(if (and l (< 0 n))
		(cons (car l) (lm:remove-nth (1- n) (cdr l)))
		(cdr l)
	)
)

;;;---------------------=={ Subst Nth }==----------------------;;;
;;;                                                            ;;;
;;;  Substitutes an item at the nth position in a list.        ;;;
;;;------------------------------------------------------------;;;
;;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;;
;;;------------------------------------------------------------;;;
;;;  Arguments:                                                ;;;
;;;  a - item to substitute                                    ;;;
;;;  n - position in list to make the substitution             ;;;
;;;  l - list in which to make the substitution                ;;;
;;;------------------------------------------------------------;;;
;;;  Returns:  Resultant list following the substitution       ;;;
;;;------------------------------------------------------------;;;

(defun lm:substitute-nth ( a n l ) ; LM:SubstNth renamed to lm:substitute-nth
	(if l
		(if (zerop n)
			(cons a (cdr l))
			(cons (car l) (lm:substitute-nth a (1- n) (cdr l)))
		)
	)
)

;;;-------------------=={ List Difference }==------------------;;;
;;;                                                            ;;;
;;;  Returns items appearing exclusively in one list but not   ;;;
;;;  another, i.e. the relative complement: l1 \ l2            ;;;
;;;------------------------------------------------------------;;;
;;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;;
;;;------------------------------------------------------------;;;
;;;  Arguments:                                                ;;;
;;;  l1,l2 - lists for which to return the difference          ;;;
;;;------------------------------------------------------------;;;
;;;  Returns:  List of items appearing exclusively in list l1  ;;;
;;;------------------------------------------------------------;;;

(defun lm:difference ( l1 l2 ) ;- LM:ListDifference renamed to lm:difference
	(vl-remove-if '(lambda ( x ) (member x l2)) l1)
)

;;; Unique  -  Lee Mac
;;; Returns a list with duplicate elements removed.

(defun lm:unique ( l ) ; LM:unique renamed to lm:unique
	(if l (cons (car l) (lm:unique (vl-remove (car l) (cdr l)))))
)

;;; List Duplicates  -  Lee Mac
;;; Returns a list of items appearing more than once in a supplied list

(defun lm:duplicates ( l ) ; LM:ListDupes renamed to lm:duplicates
	(if l
		(if (member (car l) (cdr l))
			(cons (car l) (lm:duplicates (vl-remove (car l) (cdr l))))
			(lm:duplicates (vl-remove (car l) (cdr l)))
		)
	)
)

;;; List with wcmatch - Wilfred Stapper
;;; Returns a list of matching items from a supplied list

(defun lm:wcmatch ( l a )
	(vl-remove-if '(lambda (x) (not (wcmatch x a))) l)
)

;;; List with wcmatch - Wilfred Stapper
;;; Returns a list without the matching items from supplied list

(defun lm:wcmatch|remove ( l a )
	(vl-remove-if '(lambda (x) (wcmatch x a)) l)
)

;;; String to List  -  Lee Mac
;;; Separates a string using a given delimiter
;;; str - [str] String to process
;;; del - [str] Delimiter by which to separate the string
;;; Returns: [lst] List of strings
 
(defun lm:string->list ( str del / pos ) ; LM:str->lst renamed to lm:string->list
	(if (setq pos (vl-string-search del str))
		(cons (substr str 1 pos) (lm:string->list (substr str (+ pos 1 (strlen del))) del))
		(list str)
	)
)

;;; String to List -  Wilfred Stapper
;;; Separates a string using the given start and end pattern
;;; aString - String to process
;;; aStart - Start pattern
;;; aEnd - End pattern
;;; Returns a list of strings, excluding the pattern
 
(defun lm:string->list|exclude ( aString aStart aEnd / iStart iEnd ) 
	(if (setq iStart (vl-string-search aStart aString))
		(if (setq iEnd (vl-string-search aEnd aString (+ iStart (strlen aStart))))
			(cons 
				(substr aString (+ 1 iStart (strlen aStart)) (- iEnd iStart (strlen aStart)))
				(lm:string->list|exclude (substr aString (1+ iEnd)) aStart aEnd)
			)
			(list aString)
		)
	)
)

;;; String to List - Wilfred Stapper
;;; Separates a string using the given start and end pattern
;;; aString - String to process
;;; aStart - Start pattern
;;; aEnd - End pattern
;;; Returns a list of strings, including the pattern
 
(defun lm:string->list|include ( aString aStart aEnd / iStart iEnd ) 
	(if (setq iStart (vl-string-search aStart aString))
		(if (setq iEnd (vl-string-search aEnd aString (+ iStart (strlen aStart))))
			(cons 
				(substr aString (1+ iStart) (- (+ iEnd (strlen aEnd)) iStart)) 
				(lm:string->list|include (substr aString (1+ iEnd)) aStart aEnd)
			)
			(list aString)
		)
	)
)

;;; Parse Numbers - Lee Mac
;;; Parses a list of numerical values from a supplied string.

(defun lm:string->list|numbers ( a )
	((lambda ( l )
		(read
			(strcat "("
				(vl-list->string
					(mapcar
						'(lambda ( a b c )
							(if (or (< 47 b 58)
									(and (= 45 b) (< 47 c 58) (not (< 47 a 58)))
									(and (= 46 b) (< 47 a 58) (< 47 c 58))
								)
									b 32
							)
                            )
                            (cons nil l) l (append (cdr l) '(()))
					)
				)
				")"
			)
		))
		
		(if a (vl-string->list a))
	)
)

;;; Parse Characters
;;; Parses a list of characters from a supplied string.

(defun lm:string->list|characters ( a )
	(mapcar 'chr (vl-string->list a))
)

;;; List to String - Lee Mac
;;; Concatenates each string in a supplied list, separated by a given delimiter
;;; lst - [lst] List of strings to concatenate
;;; del - [str] Delimiter string to separate each item

(defun lm:list->string ( lst del ) ; LM:lst->str renamed to lm:list->string
	(if (cdr lst)
		(strcat (car lst) del (lm:list->string (cdr lst) del))
		(car lst)
	)
)

(princ)
