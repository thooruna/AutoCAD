(defun lm:unique ( l )
	(if l (cons (car l) (lm:unique (vl-remove (car l) (cdr l)))))
)

;(defun lm:merge-dotted-pair ( l a1 a2 )
;   (strcat (assoc a1 l) (assoc a2 l))
;)


(princ)
