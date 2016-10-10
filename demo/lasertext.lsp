(defun lasertext ( p aString / aBlock aChar aPrefix r )
	(cm:setvar "OSMODE" 0)
	
	(foreach aChar (lm:string->list|characters aString)
		(cond 
			((sm:is-character|digit aChar) (setq aPrefix "_digit_"))
			((sm:is-character|ucase aChar) (setq aPrefix "_char_ucase_"))
			((sm:is-character|lcase aChar) (setq aPrefix "_char_lcase_"))
			(T (setq aPrefix nil))
		)
		
		(if aPrefix
			(if (setq aBlock (bm:exists|drawing (strcat aPrefix aChar)))
				(progn
					(bm:insert aBlock p)
					(command "_.EXPLODE" (entlast))
				)
			)
		)
		
		(cond
			((wcmatch aChar "0,2,3,5,6,7,8,9") (setq r 6.5))
			((wcmatch aChar "1") (setq r 3.5))
			((wcmatch aChar "4") (setq r 7))
			((wcmatch aChar "A,M,O,Q") (setq r 9.5))
			((wcmatch aChar "B,E,P,S") (setq r 7.5))
			((wcmatch aChar "C,G,R,V,X,Y") (setq r 9))
			((wcmatch aChar "D,H,N,T,U,Z") (setq r 8))
			((wcmatch aChar "F") (setq r 6.5))
			((wcmatch aChar "I") (setq r 1.5))
			((wcmatch aChar "J") (setq r 5.5))
			((wcmatch aChar "K") (setq r 8.5))
			((wcmatch aChar "L") (setq r 6))
			((wcmatch aChar "W") (setq r 13))
			((wcmatch aChar "a,b,c,d,e,g,p,q,v,y,z") (setq r 6.5))
			((wcmatch aChar "f") (setq r 4.5))
			((wcmatch aChar "h,k,n,s,u") (setq r 6))
			((wcmatch aChar "i,l") (setq r 1))
			((wcmatch aChar "m,w") (setq r 10))
			((wcmatch aChar "j") (setq r 3))
			((wcmatch aChar "o,x") (setq r 7))
			((wcmatch aChar "r") (setq r 4))
			((wcmatch aChar "t") (setq r 3.5))
			
			(T (setq r 4)) ; Other characters
		)
		
		(setq p (polar p 0 (+ r 2)))
	)
)

(defun c:lasertext ( / a p )
	(cm:initialize)
	
	(if (bm:load "LASERFONT")
		(if (setq p (im:get-insertion-point))
			(if (setq a (getstring T "\nSpecify text: "))
				(lasertext p a)
			)
		)
	)
	
	(cm:terminate)
)

