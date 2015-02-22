(defun blocktable ( s a / d h lData lHeader lHandles oTable)
	(setq lData '())
	
	(if s
		(cond
			((setq lHandles (bm:search s a))
				(setq 
					lHeader (bm:all-attributes-length lHandles) 
					lData (tm:table-data-add lData (mapcar 'car lHeader)) ;- First add the table header
				)
					
				(foreach h lHandles
					(setq lData (tm:table-data-add lData (bm:insert-attributes (handent h))))
				)
					
				(setq lData (tm:table-data-sort lData 1)) ;- Sort the table by column '1'
				
				;-	Create the AutoCAD table
				(tm:table-init "Standard" "Courier New")
					
				(setq oTable (tm:table-create nil lData))
				(tm:table-set-title oTable "BLOCK TABLE")
				(tm:table-set-width oTable (mapcar 'cdr lHeader))
				(tm:table-show oTable)
					
				(princ (strcat "\nTotal blocks found: " (itoa (length lHandles))))
				(princ (strcat "\nTotal unique blocks found: " (itoa (length (lm:unique lHandles)))))
			)
			(T (princ (strcat "\nNo blocks found with filter \"" a "\".")))
		)
		(princ "\nDrawing does not contain any blocks.")
	)
	
	(princ)
)

(defun c:blocktable ( / s )
	(blocktable (im:select-all-blocks) "SYMBOL*")
)

(princ)
