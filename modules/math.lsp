;;; Download the latest version from http://github.com/thooruna/AutoCAD/
;;; Author: Wilfred Stapper (unless stated otherwise)
;;; Copyright © 2015

(defun mm:degrees->radians ( r )
	(* pi (/ r 180.0))
)

(defun mm:radians->degrees ( r )
	(/ (* r 180.0) pi)
)

(defun mm:is-number ( a / l )
	(if (= (length (setq l (lm:string->list|numbers a))) 1)
		(= (car l) (atof a))
	)
)

;;; Written by Lee Ambrosius on: 6/6/04
;;; Command toggles the bit of the value (typically used for OSMODE)
;;; Value of 'on' determines if bit is to be on (1), off (-1), or toggled (0 or nil)

(defun mm:bitcode ( value bit on )
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

(princ)