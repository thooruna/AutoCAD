(defun lm:unique ( l )
	(if l (cons (car l) (lm:unique (vl-remove (car l) (cdr l)))))
)

(defun lm:diff ( l1 l2 )
	(vl-remove-if '(lambda ( x ) (member x l2)) l1)
)

(princ)
