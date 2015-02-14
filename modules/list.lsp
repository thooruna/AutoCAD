;; Unique  -  Lee Mac
;; Returns a list with duplicate elements removed.

(defun lm:unique ( l )
    (if l (cons (car l) (lm:unique (vl-remove (car l) (cdr l)))))
)