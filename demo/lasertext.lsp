(defun lasertext ( p aString / aBlock aChar aPrefix i )
	(cm:setvar "OSMODE" 0)
	
	(foreach aChar (lm:string->list|characters aString)
		(cond 
			((sm:is-character|digit aChar) (setq aPrefix "_digit_"))
			((sm:is-character|ucase aChar) (setq aPrefix "_char_ucase_"))
			((sm:is-character|lcase aChar) (setq aPrefix "_char_lcase_"))
			(T (setq aPrefix nil))
		)
		
		(if aPrefix
			(if (setq aBlock (bm:find (strcat aPrefix aChar)))
				(progn
					(bm:insert aBlock p)
					(command "_.EXPLODE" (entlast))
				)
			)
		)
		
		(cond
			((wcmatch aChar "0,2,3,5,6,7,8,9") (setq i 6.5))
			((wcmatch aChar "1") (setq i 3.5))
			((wcmatch aChar "4") (setq i 7))
			((wcmatch aChar "A,M,O,Q") (setq i 9.5))
			((wcmatch aChar "B,E,P,S") (setq i 7.5))
			((wcmatch aChar "C,G,R,V,X,Y") (setq i 9))
			((wcmatch aChar "D,H,N,T,U,Z") (setq i 8))
			((wcmatch aChar "I") (setq i 1.5))
			((wcmatch aChar "J") (setq i 5.5))
			((wcmatch aChar "K") (setq i 8.5))
			((wcmatch aChar "L") (setq i 6))
			((wcmatch aChar "W") (setq i 13))
			((wcmatch aChar "a,b,c,d,e,g,p,q,v,y,z") (setq i 6.5))
			((wcmatch aChar "f") (setq i 4.5))
			((wcmatch aChar "h,k,n,s,u") (setq i 6))
			((wcmatch aChar "i,l") (setq i 1))
			((wcmatch aChar "m,w") (setq i 10))
			((wcmatch aChar "j") (setq i 3))
			((wcmatch aChar "o,x") (setq i 7))
			((wcmatch aChar "r") (setq i 4))
			((wcmatch aChar "t") (setq i 3.5))
			
			(T (setq i 4)) ; Other characters
		)
		
		(setq p (polar p 0 (+ i 2)))
	)
)

(defun c:lasertext ( / a p )
	(cm:debug nil)
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

