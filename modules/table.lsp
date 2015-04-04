(setq
	tm:table-cell-horizontal-margin 2.5
	tm:table-cell-vertical-margin 0.5
	tm:table-cell-text-height 2.5
	tm:table-title-text-height 5
)

(defun tm:table-data-add-row ( lData lRow / a d lTemp )
	(setq lTemp '())
		
	(cond
		((= (length lData) 0)
			(setq lTemp lRow) ;- First row is the header row,
		)
		(T
			(foreach a (car lData) ;- followed by data rows
				(if (setq d (assoc a lRow))	
					(setq lTemp (append lTemp (list (cdr d))))
					(setq lTemp (append lTemp (list "")))
				)
			)
		)
	)
	
	(append lData (list lTemp))
)

(defun tm:table-data-sort ( l i / lTemp )
	(if (null i) (setq i 0))
	
	(setq 
		lTemp (mapcar '(lambda (x) (list (nth i x) x)) (cdr l)) ;- Remove the header/Change the data format: add sort column first and nest the row data
		lTemp (vl-sort lTemp '(lambda (a b) (< (car a) (car b)))) ;- Sort the data
		lTemp (mapcar 'cadr lTemp) ;- Change the data format back
	)
	
	(append (list (car l)) lTemp) ;- Add the header back
)

(defun tm:table-init ( a1 a2 / acad doc dicts dictObj myTableStyle)
	(command "._STYLE" a1 a2 0 1 0 "" "")
	
	(vl-load-com)
	
	(setq acad (vlax-get-acad-object)) ;- Get the AutoCAD application
	(setq doc (vla-get-ActiveDocument acad)) ;- Get the current document
	(setq dicts (vla-get-Dictionaries doc)) ;- Get the Dictionaries collection
	(setq dictObj (vla-Item dicts "acad_tablestyle")) ;- Get the TableStyle dictionary
	(setq myTableStyle (vla-AddObject dictObj a1 "AcDbTableStyle")) ;- Create a custom table style
	
	(vla-put-Name myTableStyle a1) ;- Set the name for the style
	(vla-put-Description myTableStyle a1) ;- Set the description for the style
	(vla-put-BitFlags myTableStyle 1)  ;- Sets the bit flag value for the style
	(vla-put-FlowDirection myTableStyle acTableTopToBottom) ;- Sets the direction of the table, top to bottom or bottom to top
	(vla-put-HeaderSuppressed myTableStyle :vlax-false) ;- Sets the supression of the table header
	(vla-put-HorzCellMargin myTableStyle tm:table-cell-horizontal-margin) ;- Sets the horizontal margin for the table cells
	(vla-put-TitleSuppressed myTableStyle :vlax-false) ;- Sets the supression of the table title
	(vla-put-VertCellMargin myTableStyle tm:table-cell-vertical-margin) ;- Sets the vertical margin for the table cells
	(vla-SetAlignment myTableStyle (+ acTitleRow acHeaderRow) acMiddleCenter) ;- Set the alignment for the Header, and Title row
	(vla-SetAlignment myTableStyle acDataRow acMiddleLeft) ;- Set the alignment for the Data rows
	(vla-SetTextHeight myTableStyle acTitleRow tm:table-title-text-height) ;- Set the text height for the Title rows
	(vla-SetTextHeight myTableStyle (+ acDataRow acHeaderRow) tm:table-cell-text-height) ;- Set the text height for the Header and Data rows
	(vla-SetTextStyle myTableStyle (+ acDataRow acHeaderRow acTitleRow) a1) ;- Set the text style
	
	(setvar "CTABLESTYLE" a1) ;- Set the current table style
)

(defun tm:table-create ( pInsert lTable / pt iRows iColumns oTable nRows nCols row cell lRow )
	(defun ActiveSpace ( / *AcadDoc* )
		(if (or (eq acmodelspace (vla-get-activespace (cond (*AcadDoc*) ((setq *AcadDoc* (vla-get-activedocument (vlax-get-acad-object))))))) 
				(eq :vlax-true (vla-get-mspace *AcadDoc*)))
				(vla-get-modelspace *AcadDoc*)
				(vla-get-paperspace *AcadDoc*)
		)
	)

	(setq 
		pt (vlax-make-safearray vlax-vbDouble '(0 . 2))	;- Insertion point for the table 	
		iRows (+ (length lTable) 1)
		;iColumns (length (cadr (nth 0 lTable)))
		iColumns (length (nth 0 lTable))
	) 
	
	(if (null pInsert) 
		(setq pInsert (im:get-insertion-point))
	)
	
	(vlax-safearray-fill pt pInsert) 
	
	(setq oTable (vla-AddTable (ActiveSpace) pt iRows iColumns 5 250)) 
	(vla-put-RegenerateTableSuppressed oTable :vlax-true)
	(vla-put-RepeatTopLabels oTable :vlax-true)
	(vla-put-Layer oTable "0")
	
	(setq 
		;- Rows and columns zero based 
		nRows (- (vla-get-rows oTable) 1)
		nCols (- (vla-get-columns oTable) 1)
		
		;- Rows and columns after row 0, column 0 
		row 1  
		cell 0
	) 
	
	;(princ lTable)
	
	;- Loop through row cells
	(while (<= row nRows)
		;(setq lRow (cadr (nth (- row 1) lTable)))
		(setq lRow (nth (- row 1) lTable))
		(while (<= cell nCols) 
			(vla-SetText oTable row cell (nth cell lRow))
			;Set the cell alignment
			;(if (or (and (and (or (= cell 0) (= cell 2)) (> row 1)) (= iColumns 4)) (IsNumber (nth cell lRow)))
			;	(vla-SetCellAlignment oTable row cell acMiddleRight) ;- Only used by ptable
			;	(vla-SetCellAlignment oTable row cell acMiddleLeft)
			;)
			(setq cell (1+ cell)) 
		);while 
		(setq row (1+ row)) 
		(setq cell 0)  
	);while
	
	(vla-ReComputeTableblock oTable :vlax-true)
	
	oTable
)

(defun tm:table-set-title ( oTable a)
	(vla-SetText oTable 0 0 a)
)

(defun tm:table-set-width ( oTable l / d i )
	(setq i 0)
	(foreach d l
		(vla-SetColumnWidth oTable i (+ (* d 2.66) (* tm:table-cell-horizontal-margin 2)))
		(setq i (1+ i))
	)
)

(defun tm:table-show ( oTable )
	(vla-put-RegenerateTableSuppressed oTable :vlax-false)
)

(princ)

