(defun im:get-insertion-point ( / p )
	(setq p (getpoint "\nSpecify insertion point: "))
	
	(if (null p) 
		(setq p '(0 0 0))
		p
	)
)

(defun im:select-all-blocks ( )
	(ssget "_X" '((0 . "INSERT")))
)

(defun im:select-blocks ( )
	(ssget '((0 . "INSERT")))
)

(princ)