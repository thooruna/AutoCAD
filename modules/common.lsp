(defun dtr (rAngle)
	(* pi (/ rAngle 180.0))
)

(defun rtd (rAngle)
	(/ (* rAngle 180.0) pi)
)

(defun BitCode (value bit on)
	; Written by Lee Ambrosius on: 6/6/04
	; Command toggles the bit of the value (typically used for OSMODE)
	; Value of 'on' determines if bit is to be on (1), off (-1), or toggled (0 or nil)
	(cond
		( (or(= on "1") (= on 1))
			; Turn bit on if not already on
			(if (zerop (logand bit value))
				(+ value bit)
				value
			)
		)
		( (or (= on "-1") (= on -1))
			; Turn bit off if not already off
			(if (not (zerop (logand bit value)))
				(- value bit)
				value
			)
		)
		(T
			; Toggle bit
			(if (zerop (logand bit value))
				(+ value bit)
				(- value bit)
			)
		)
	)
)

(defun IsCharacter (sChar)
	(and 
		(= (strlen sChar) 1)
		(or 
			(and (>= (ascii sChar) 65) (<= (ascii sChar) 90))
			(and (>= (ascii sChar) 97) (<= (ascii sChar) 122))
		)
	)
)

(defun _getpoint (sMessage / p)
	(if (null (setq p (getpoint sMessage))) (exit))
	p
)

