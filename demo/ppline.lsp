(defun c:ppline ( / a l )
	
	(defun LayerActivate ( a / l )
		(if (setq l (assoc a lLayers))
			(cm:layer-activate (cm:layer-new (cadr l) (caddr l) (cadddr l)))
		)
	)
	
	(defun LayerActive ( / a )
		(if (setq a (assoc (em:name (cm:layer-active)) (mapcar 'cddr  (mapcar 'reverse lLayers)))) 
			(cadr a)
			(caar lLayers)
		)
	)
	
	(setq 
		lLayers '(
			("Processflow" "PROCESS FLOW" 2 "Continuous")
			("vegOil" "VEG OIL" 32 "Continuous")
			("Caustic" "CAUSTIC" 6 "ACAD_ISO05W100")
			("freshWater" "FRESH WATER" 5 "HIDDEN")
			("compressedAir" "AIR" 4 "BORDER2")
			("Nitrogen" "NITROGEN" 41 "Phantom")
			("sYstemwater" "LP_PROCESS_WATER" 94 "ACAD_ISO04W100")
			("Diesel" "DIESEL" 44 "ACAD_ISO06W100")
			("naturalGas" "NATURAL GAS" 1 "ACAD_ISO06W100")
			("Thermalfluid" "THERMAL OIL" 181 "ACAD_ISO06W100")
			("Hydraulic" "HYDRAULIC" 2 "Amzigzag")
			("Steam" "STEAM" 91 "ACAD_ISO06W100")
		)
	)
	
	(LayerActivate (im:get-keyword "Linetype: " (LayerActive) (mapcar 'car lLayers)))
	
	(command-s "_.PLINE")
)

(princ)