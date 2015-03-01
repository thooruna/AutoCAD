(defun im:get-insertion-point ( / p )
	(setq p (getpoint "\nSpecify insertion point: "))
	
	(if (null p) 
		(setq p '(0 0 0))
		p
	)
)

(defun im:select-all-blocks ( )
	(if (null (setq s (ssget "_X" '((0 . "INSERT")))))
		(princ "\nDrawing does not contain any blocks.")
	)
	
	s
)

(defun im:select-blocks ( / s )
	(if (null (setq s (ssget '((0 . "INSERT")))))
		(princ "\nNo blocks were selected.")
	)
	
	s
)

(princ)