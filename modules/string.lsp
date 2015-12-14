;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper
;;; Copyright © 2015

(defun sm:to-string ( x )
	(cond 
		((= (type x) 'INT) (itoa x))
		((= (type x) 'REAL) (rtos x))
		((= (type x) nil) "")
		(T x)
	)
)

;; Escape wildcard characters in a string except for the comma

(defun sm:string-escape ( a )
	(vl-list->string
		(apply 'append
			(mapcar
				'(lambda ( x )
					(if (member x '(35 42 45 46 63 64 91 93 126))
						(list 96 x)
						(list x)
					)
				)
				(vl-string->list a)
			)
		)
	)
)

;;; Simplified wcmatch command (case insensitive, escaped and no wildcards except for the comma)

(defun sm:string-match ( a1 a2 )
	(wcmatch (strcase a1) (strcase (sm:string-escape a2)))
)

(defun sm:string-lowercase ( x )
	(cond
		((= (type x) 'STRING) (strcase x T))
		(T x)
	)
)

(defun sm:string-uppercase ( x )
	(cond
		((= (type x) 'STRING) (strcase x))
		(T x)
	)
)

(defun sm:string-length ( x )
	(strlen (sm:to-string x))
)

(defun sm:string-left-fill ( aString aFill i )
	(if (null aString) 
		(setq aString "")
	)
	
	(if (or (null aFill) (= (strlen aFill) 0)) 
		(setq aFill " ") ; Default
	)
	
	(while (<= (strlen aString) (- i 1))
		(setq aString (strcat aFill aString))
	)
	
	aString
)

(defun sm:string-right-fill ( aString aFill i )
	(if (null aString) 
		(setq aString "")
	)
	
	(if (or (null aFill) (= (strlen aFill) 0)) 
		(setq aFill " ") ; Default
	)
	
	(while (<= (strlen aString) (- i 1))
		(setq aString (strcat aString aFill))
	)
	
	aString
)

;;; Substitutes one string for another, within a string (replacing all occurences)
;;; aNew - The string to be substituted for pattern
;;; aPattern - A string containing the pattern to be replaced
;;; aString - The string to be searched for pattern

(defun sm:string-substitute ( aNew aPattern aString / i )
	(setq i 0)
	
	(while (setq i (vl-string-search aPattern aString i))
		(setq aString (vl-string-subst aNew aPattern aString i)
			i (+ i (strlen aNew))
		)
	)
	
	aString
)

;;; Find a substring with start and end pattern, excluding the pattern
;;; aString - The string to be searched
;;; aStart - A string containing the start pattern
;;; aEnd - A string containing the end pattern

(defun sm:string-substring|exclude ( aString aStart aEnd / iStart iEnd ) 
	(if (setq iStart (vl-string-search aStart aString))
		(if (setq iEnd (vl-string-search aEnd aString (+ iStart (strlen aStart))))
			(substr aString (+ 1 iStart (strlen aStart)) (- iEnd iStart (strlen aStart)))
		)
	)
)

;;; Find a substring with start and end pattern, including the pattern
;;; aString - The string to be searched
;;; aStart - A string containing the start pattern
;;; aEnd - A string containing the end pattern

(defun sm:string-substring|include ( aString aStart aEnd / iStart iEnd ) 
	(if (setq iStart (vl-string-search aStart aString))
		(if (setq iEnd (vl-string-search aEnd aString (+ iStart (strlen aStart))))
			(substr aString (1+ iStart) (- (+ iEnd (strlen aEnd)) iStart))
		)
	)
)

(defun sm:string-remove|characters ( aString aCharacters )
	(mapcar 'vl-list->string
		(list (vl-remove-if '(lambda ( x ) (member x (vl-string->list aCharacters))) (vl-string->list aString)))
	)
)

(defun sm:string-name ( aString / aPattern iEnd )
	(setq aPattern "=")
	
	(if (setq iEnd (vl-string-search aPattern aString))
		(substr aString 1 iEnd)
		aString
	)
)

(defun sm:string-value ( aString / aPattern iStart )
	(setq aPattern "=")
	
	(if (setq iStart (vl-string-search aPattern aString))
		(substr aString (+ iStart 2))
		aString
	)
)

(defun sm:is-character ( a )
	(or 
		(sm:is-character|digit a)
		(sm:is-character|ucase a)
		(sm:is-character|lcase a)
	)
)

(defun sm:is-character|digit ( a )
	(and 
		(= (type a) 'STR)
		(and (>= (ascii a) 48) (<= (ascii a)  57))
	)
)

(defun sm:is-character|ucase ( a )
	(and 
		(= (type a) 'STR)
		(and (>= (ascii a) 65) (<= (ascii a) 90))
	)
)

(defun sm:is-character|lcase ( a )
	(and
		(= (type a) 'STR)
		(and (>= (ascii a) 97) (<= (ascii a) 122))
	)
)

(princ)