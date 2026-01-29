;;;=============================================================================
;;; GESTIONE PUNTI CAD  
;;; Ultimo aggiornamento: 14/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per operazioni sui punti cad
;;; Dipendenze esterne: -
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1.  FUNZIONI FONDAMENTALI
;;; 2.  OPERAZIONI SU LISTE DI PUNTI
;;; 3.  CALCOLI GEOMETRICI AVANZATI
;;; 4.  PROIEZIONE PUNTI E PERPENDICOLARI
;;; 5.  SELEZIONE E FILTRAGGIO PUNTI
;;; 6.  UTILITA' VISIVE E ZOOM
;;; 7.  TRASFORMAZIONI GEOMETRICHE
;;;=============================================================================
;;; 1. FUNZIONI FONDAMENTALI
;;;=============================================================================
;;; CALC-DISTANCE
;;; Calcola la distanza euclidea tra due punti (2D o 3D)
;;; Argomenti:
;;;   pt1 - Primo punto come lista (x y) o (x y z)
;;;   pt2 - Secondo punto come lista (x y) o (x y z)
;;; Ritorna: Distanza come numero reale, o nil se input non valido
;;; Esempi:
;;;   (calc-distance '(0 0) '(3 4)) => 5.0
;;;   (calc-distance '(0 0 0) '(1 1 1)) => 1.73205
(defun calc-distance (pt1 pt2 / dx dy dz)
  (if (and pt1 pt2)
    (progn
      (setq dx (- (car pt1) (car pt2))
	    dy (- (cadr pt1) (cadr pt2))
	    dz (if (and (caddr pt1) (caddr pt2))
		 (- (caddr pt1) (caddr pt2))
		 0.0
	       )
      )
      (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
    )
    nil
  )
)
;;; CALC-MIDPOINT
;;; Calcola il punto medio tra due punti (2D o 3D)
;;; Argomenti:
;;;   pt1 - Primo punto
;;;   pt2 - Secondo punto
;;; Ritorna: Punto medio come lista, o nil se input non valido
;;; Esempio:
;;;   (calc-midpoint '(0 0) '(10 10)) => (5.0 5.0)
(defun calc-midpoint (pt1 pt2 / x y z)
  (if (and pt1 pt2)
    (progn
      (setq x (/ (+ (car pt1) (car pt2)) 2.0)
	    y (/ (+ (cadr pt1) (cadr pt2)) 2.0)
	    z (if (and (caddr pt1) (caddr pt2))
		(/ (+ (caddr pt1) (caddr pt2)) 2.0)
		nil
	      )
      )
      (if z
	(list x y z)
	(list x y)
      )
    )
    nil
  )
)
;;; GET-X-COORDS
;;; Estrae le coordinate X da una lista di punti
;;; Argomenti:
;;;   pt-list - Lista di punti
;;; Ritorna: Lista di coordinate X
;;; Esempio:
;;;   (get-x-coords '((1 2) (3 4) (5 6))) => (1 3 5)
(defun get-x-coords (pt-list)
  (mapcar 'car pt-list)
)
;;; GET-MAX-X
;;; Trova la coordinata X massima in una lista di punti
;;; Argomenti:
;;;   pt-list - Lista di punti
;;; Ritorna: Valore X massimo come reale, o nil se lista vuota
;;; Esempio:
;;;   (get-max-x '((1 2) (5 3) (3 4))) => 5
(defun get-max-x (pt-list / xmax p)
  (setq xmax nil)
  (foreach p pt-list
    (if	(or (null xmax) (> (car p) xmax))
      (setq xmax (car p))
    )
  )
  xmax
)
;;;=============================================================================
;;; 2. OPERAZIONI SU LISTE DI PUNTI
;;;=============================================================================
;;; CALC-CENTROID
;;; Calcola il centro aritmetico (baricentro) di una lista di punti
;;; Argomenti:
;;;   pt-list - Lista di punti (2D)
;;; Ritorna: Punto baricentro come lista (x y), o nil se lista vuota
;;; Esempio:
;;;   (calc-centroid '((0 0) (10 0) (5 10))) => (5.0 3.33333)
(defun calc-centroid (pt-list / sx sy n)
  (if (and pt-list (> (length pt-list) 0))
    (progn
      (setq sx 0.0
	    sy 0.0
	    n  0
      )
      (foreach pt pt-list
	(setq sx (+ sx (car pt))
	      sy (+ sy (cadr pt))
	      n	 (1+ n)
	)
      )
      (list (/ sx n) (/ sy n))
    )
    nil
  )
)
;;; CALC-CUMULATIVE-DISTANCE
;;; Calcola distanza cumulativa dall'inizio lista fino a punto specifico
;;; Argomenti:
;;;   target-pt - Punto target da raggiungere (deve essere nella lista)
;;;   pt-list   - Lista di punti consecutivi
;;; Ritorna: Distanza cumulativa come reale, o nil se punto non trovato
;;; Esempio:
;;;   (calc-cumulative-distance '(5 0) '((0 0) (5 0) (10 0))) => 5.0
(defun calc-cumulative-distance
       (target-pt pt-list / prev accum found tol cur)
  (if (and target-pt pt-list)
    (progn
      (setq tol	  1e-6
	    prev  (car pt-list)
	    accum 0.0
	    found nil
      )
      (if (<= (calc-distance prev target-pt) tol)
	(setq found T)
	(foreach cur (cdr pt-list)
	  (if (not found)
	    (progn
	      (setq accum (+ accum (calc-distance prev cur))
		    prev  cur
	      )
	      (if (<= (calc-distance cur target-pt) tol)
		(setq found T)
	      )
	    )
	  )
	)
      )
      (if found
	accum
	nil
      )
    )
    nil
  )
)
;;; FIND-NEAREST-POINT
;;; Trova il punto più vicino in una lista rispetto a un punto di riferimento
;;; Argomenti:
;;;   ref-pt  - Punto di riferimento
;;;   pt-list - Lista di punti candidati
;;; Ritorna: Punto più vicino dalla lista, o nil se lista vuota
;;; Esempio:
;;;   (find-nearest-point '(5 5) '((0 0) (10 10) (6 6))) => (6 6)
(defun find-nearest-point (ref-pt pt-list / best min-dist cur cur-dist)
  (if (and ref-pt pt-list)
    (progn
      (setq best nil
	    min-dist 1e99
      )
      (foreach cur pt-list
	(setq cur-dist (calc-distance ref-pt cur))
	(if (< cur-dist min-dist)
	  (setq	min-dist cur-dist
		best	 cur
	  )
	)
      )
      best
    )
    nil
  )
)
;;; FIND-NEAREST-PAIR
;;; Trova il punto più vicino e il suo vicino in una lista
;;; Argomenti:
;;;   ref-pt  - Punto di riferimento
;;;   pt-list - Lista di punti consecutivi
;;; Ritorna: Lista di due punti (più vicino e suo vicino più prossimo)
;;; Esempio:
;;;   (find-nearest-pair '(5 1) '((0 0) (5 0) (10 0))) => ((5 0) (10 0))
(defun find-nearest-pair (ref-pt pt-list / p1 idx len p2)
  (if (and ref-pt pt-list (> (length pt-list) 1))
    (progn
      (setq p1	(find-nearest-point ref-pt pt-list)
	    idx	0
	    len	(length pt-list)
      )
      (while (and (< idx len) (/= (nth idx pt-list) p1))
	(setq idx (1+ idx))
      )
      (cond
	((= idx 0)
	 (setq p2 (nth 1 pt-list))
	)
	((>= idx (1- len))
	 (setq p2 (nth (1- idx) pt-list))
	)
	(t
	 (setq
	   p2 (if (< (calc-distance ref-pt (nth (1+ idx) pt-list))
		     (calc-distance ref-pt (nth (1- idx) pt-list))
		  )
		(nth (1+ idx) pt-list)
		(nth (1- idx) pt-list)
	      )
	 )
	)
      )
      (list p1 p2)
    )
    nil
  )
)
;;; IS-POINT-NEAR-LIST
;;; Verifica se un punto ù vicino a qualsiasi punto in una lista (entro tolleranza)
;;; Argomenti:
;;;   test-pt - Punto da testare
;;;   pt-list - Lista di punti da confrontare
;;;   tol     - Tolleranza opzionale (default 0.001)
;;; Ritorna: T se punto ù vicino a uno nella lista, nil altrimenti
(defun is-point-near-list (test-pt pt-list / tol result)
  (setq	tol 0.001
	result nil
  )
  (foreach pt pt-list
    (if	(< (calc-distance test-pt pt) tol)
      (setq result T)
    )
  )
  result
)
;;;=============================================================================
;;; 3. CALCOLI GEOMETRICI AVANZATI
;;;=============================================================================
;;; CALC-PERIMETER
;;; Calcola il perimetro di un poligono chiuso da lista di punti
;;; Argomenti:
;;;   pt-list - Lista di punti consecutivi che formano un poligono
;;; Ritorna: Lunghezza totale del perimetro
;;; Esempio:
;;;   (calc-perimeter '((0 0) (10 0) (10 10) (0 10))) => 40.0
(defun calc-perimeter (pt-list / perimeter i)
  (setq	perimeter 0.0
	i 0
  )
  (repeat (1- (length pt-list))
    (setq perimeter (+ perimeter
		       (calc-distance
			 (nth i pt-list)
			 (nth (1+ i) pt-list)
		       )
		    )
	  i	    (1+ i)
    )
  )
  (+ perimeter (calc-distance (last pt-list) (car pt-list)))
)
;;; CREATE-RECTANGLE-POINTS
;;; Crea lista di punti angolari rettangolo da due angoli opposti
;;; Argomenti:
;;;   corner1 - Primo angolo (x1 y1)
;;;   corner2 - Angolo opposto (x2 y2)
;;; Ritorna: Lista di 5 punti (4 angoli + punto chiusura)
;;; Esempio:
;;;   (create-rectangle-points '(0 0) '(10 5)) 
;;;   => ((0 0) (10 0) (10 5) (0 5) (0 0))
(defun create-rectangle-points (corner1 corner2)
  (list
    corner1
    (list (car corner2) (cadr corner1))
    corner2
    (list (car corner1) (cadr corner2))
    corner1
  )
)
;;; CALC-PERPENDICULAR-ANGLE
;;; Calcola direzione perpendicolare basata su tre punti e punto selezione
;;; Argomenti:
;;;   select-pt - Punto che indica direzione perpendicolare desiderata
;;;   pt-triple - Lista di tre punti consecutivi (A B C)
;;; Ritorna: Angolo in radianti per direzione perpendicolare
(defun calc-perpendicular-angle
       (select-pt pt-triple / ang ang1 ang2 d1 d2 r)
  (if (and select-pt pt-triple (>= (length pt-triple) 3))
    (progn
      (setq ang	 (angle (nth 1 pt-triple) (nth 2 pt-triple))
	    ang1 (+ ang (* 0.5 pi))
	    ang2 (- ang (* 0.5 pi))
	    r	 (/ (calc-distance select-pt (nth 1 pt-triple)) 20.0)
	    d1	 (calc-distance (nth 1 pt-triple) (polar select-pt ang1 r))
	    d2	 (calc-distance (nth 1 pt-triple) (polar select-pt ang2 r))
      )
      (if (> d1 d2)
	ang1
	ang2
      )
    )
    nil
  )
)
;;;=============================================================================
;;; 4. PROIEZIONE PUNTI E PERPENDICOLARI
;;;=============================================================================
;;; PROJECT-POINT-TO-SEGMENT
;;; Proietta un punto perpendicolarmente su un segmento di linea
;;; Argomenti:
;;;   seg-pt1 - Primo estremo del segmento (x y [z])
;;;   seg-pt2 - Secondo estremo del segmento (x y [z])
;;;   test-pt - Punto da proiettare (x y [z])
;;;   constrain - Se T, proiezione deve cadere tra gli estremi
;;; Ritorna: Punto proiettato (x y [z]), o nil se fuori segmento (con constrain)
;;; Esempio:
;;;   (project-point-to-segment '(0 0) '(10 0) '(5 5) T) => (5.0 0.0)
;;;   (project-point-to-segment '(0 0) '(10 0) '(15 5) T) => nil (fuori)
;;;   (project-point-to-segment '(0 0) '(10 0) '(15 5) nil) => (15.0 0.0)
(defun project-point-to-segment	(seg-pt1 seg-pt2 test-pt constrain
				 /	 x1	 x2	 x3
				 y1	 y2	 y3	 z1
				 z2	 z3	 t-param denom
				 dx	 dy	 proj-pt
				)
  (if (and seg-pt1 seg-pt2 test-pt)
    (progn
      (setq x1 (car seg-pt1)
	    y1 (cadr seg-pt1)
	    z1 (caddr seg-pt1)
	    x2 (car seg-pt2)
	    y2 (cadr seg-pt2)
	    z2 (caddr seg-pt2)
	    x3 (car test-pt)
	    y3 (cadr test-pt)
	    z3 (caddr test-pt)
      )
      (setq dx (- x2 x1)
	    dy (- y2 y1)
      )
      (setq denom (+ (* dx dx) (* dy dy)))
      (if (< denom 1e-10)
	seg-pt1
	(progn
	  (setq	t-param	(/ (+ (* dx (- x3 x1))
			      (* dy (- y3 y1))
			   )
			   denom
			)
	  )
	  (if (and constrain (or (< t-param 0.0) (> t-param 1.0)))
	    nil
	    (progn
	      (setq proj-pt (list (+ x1 (* t-param dx))
				  (+ y1 (* t-param dy))
			    )
	      )
	      (if (and z1 z2)
		(setq
		  proj-pt (append proj-pt
				  (list (+ z1 (* t-param (- z2 z1))))
			  )
		)
		(if z1
		  (setq proj-pt (append proj-pt (list z1)))
		)
	      )
	      proj-pt
	    )
	  )
	)
      )
    )
    nil
  )
)
;;; DISTANCE-POINT-TO-SEGMENT
;;; Calcola la distanza minima da un punto a un segmento
;;; Argomenti:
;;;   seg-pt1 - Primo estremo del segmento
;;;   seg-pt2 - Secondo estremo del segmento
;;;   test-pt - Punto di test
;;; Ritorna: Distanza minima (numero reale)
(defun distance-point-to-segment (seg-pt1 seg-pt2 test-pt / proj-pt)
  (setq proj-pt (project-point-to-segment seg-pt1 seg-pt2 test-pt T))
  (if proj-pt
    (distance test-pt proj-pt)
    (min (distance test-pt seg-pt1)
	 (distance test-pt seg-pt2)
    )
  )
)
;;; IS-POINT-ON-SEGMENT
;;; Verifica se un punto giace su un segmento (con tolleranza)
;;; Argomenti:
;;;   seg-pt1 - Primo estremo del segmento
;;;   seg-pt2 - Secondo estremo del segmento
;;;   test-pt - Punto da verificare
;;;   tolerance - Tolleranza (default 1e-6)
;;; Ritorna: T se il punto è sul segmento, nil altrimenti
(defun is-point-on-segment
       (seg-pt1 seg-pt2 test-pt tolerance / proj-pt dist)
  (if (not tolerance)
    (setq tolerance 1e-6)
  )
  (setq proj-pt (project-point-to-segment seg-pt1 seg-pt2 test-pt T))
  (if proj-pt
    (progn
      (setq dist (distance test-pt proj-pt))
      (< dist tolerance)
    )
    nil
  )
)
;;; GET-PROJECTION-PARAMETER
;;; Ottiene il parametro t della proiezione (0=pt1, 1=pt2)
;;; Argomenti:
;;;   seg-pt1 - Primo estremo del segmento
;;;   seg-pt2 - Secondo estremo del segmento
;;;   test-pt - Punto da proiettare
;;; Ritorna: Valore t (numero reale), o nil se segmento degenere
(defun get-projection-parameter	(seg-pt1       seg-pt2	     test-pt
				 /	x1     x2     x3     y1
				 y2	y3     dx     dy     denom
				)
  (if (and seg-pt1 seg-pt2 test-pt)
    (progn
      (setq x1	  (car seg-pt1)
	    y1	  (cadr seg-pt1)
	    x2	  (car seg-pt2)
	    y2	  (cadr seg-pt2)
	    x3	  (car test-pt)
	    y3	  (cadr test-pt)
	    dx	  (- x2 x1)
	    dy	  (- y2 y1)
	    denom (+ (* dx dx) (* dy dy))
      )
      (if (< denom 1e-10)
	nil
	(/ (+ (* dx (- x3 x1)) (* dy (- y3 y1))) denom)
      )
    )
    nil
  )
)
;;;=============================================================================
;;; 5. SELEZIONE E FILTRAGGIO PUNTI
;;;=============================================================================
;;; SELECT-POINTS-INTERACTIVE
;;; Raccoglie punti interattivamente con osnap abilitato (END e INT)
;;; Ritorna: Lista di punti selezionati in ordine
;;; Uso: (setq miei-punti (select-points-interactive))
(defun select-points-interactive (/ pt pt-list aperture-old)
  (setq	pt-list	'()
	aperture-old
	 (getvar "aperture")
  )
  (setvar "aperture" 20)
  (command "_.osnap" "_end,_int")
  (while (setq
	   pt (getpoint "\nSeleziona prossimo punto (Invio per finire): ")
	 )
    (setq pt-list (cons pt pt-list))
  )
  (command "_.osnap" "_none")
  (setvar "aperture" aperture-old)
  (reverse pt-list)
)
;;; FILTER-POINTS-ABOVE-Y
;;; Filtra punti sopra una coordinata Y specificata
;;; Argomenti:
;;;   pt-list - Lista di punti da filtrare
;;;   min-y   - Soglia Y minima
;;; Ritorna: Lista di punti con Y > min-y
(defun filter-points-above-y (pt-list min-y / result)
  (setq result '())
  (foreach pt pt-list
    (if	(> (cadr pt) min-y)
      (setq result (cons pt result))
    )
  )
  (reverse result)
)
;;; CHECK-POINT-ORIENTATION
;;; Verifica orientamento relativo tra due punti
;;; Argomenti:
;;;   pt1 - Primo punto
;;;   pt2 - Secondo punto
;;; Ritorna: T se pt1 è "dopo" pt2 (destra o giù), nil altrimenti
;;; Nota: Stampa messaggi di avviso sulla linea di comando
(defun check-point-orientation (pt1 pt2 / result)
  (setq result nil)
  (if (and pt1 pt2)
    (cond
      ((> (car pt1) (car pt2))
       (princ "\nAttenzione: Orientamento a destra!\n")
       (setq result T)
      )
      ((= (abs (- (car pt1) (car pt2))) 0.0)
       (if (< (cadr pt1) (cadr pt2))
	 (progn
	   (princ "\nAttenzione: Orientamento in basso!\n")
	   (setq result T)
	 )
       )
      )
    )
  )
  result
)
;;;=============================================================================
;;; 6. UTILITA' VISIVE E ZOOM
;;;=============================================================================
;;; ZOOM-TO-POINTS
;;; Zooma la vista per inquadrare tutti i punti con margine
;;; Argomenti:
;;;   pt-list - Lista di punti da inquadrare
;;;   padding - Distanza margine opzionale (default 3.0)
;;; Effetti collaterali: Esegue comando ZOOM di AutoCAD
(defun zoom-to-points
       (point-list / min-x max-x min-y max-y x-list y-list padding)
  (if point-list
    (progn
      (setq x-list  (mapcar 'car point-list)
	    y-list  (mapcar 'cadr point-list)
	    min-x   (apply 'min x-list)
	    max-x   (apply 'max x-list)
	    min-y   (apply 'min y-list)
	    max-y   (apply 'max y-list)
	    padding 3.0
      )
      (setq min-x (- min-x padding)
	    min-y (- min-y padding)
	    max-x (+ max-x padding)
	    max-y (+ max-y padding)
      )
      (command "_.zoom"
	       "_w"
	       (list min-x min-y)
	       (list max-x max-y)
      )
    )
  )
)
;;;=============================================================================
;;; 7. TRASFORMAZIONI GEOMETRICHE
;;;=============================================================================
;;; OFFSET-POLYLINE
;;; Crea versione offset di una polilinea definita da punti
;;; Argomenti:
;;;   pt-list - Lista di punti consecutivi che definiscono polilinea
;;; Ritorna: Lista di punti offset
;;; Nota: Funzione richiede all'utente primo punto offset per determinare
;;;       direzione e calcola distanza offset da quella selezione
(defun offset-polyline (pt-list	 /	  n	   p1	    p2
			a12	 delta-ang	   dist	    offset-pt1
			offset-pt2	  out	   pt1	    pt2
			pt3	 pt1b	  pt2b	   pt3b	    pt4b
			inter
		       )
  (setq out '())
  (if (and pt-list (> (length pt-list) 1))
    (progn
      (setq p1	(car pt-list)
	    p2	(cadr pt-list)
	    a12	(angle p1 p2)
      )
      (setq offset-pt1
	     (getpoint "\nSpecifica punto riferimento offset: ")
      )
      (setq delta-ang (if (< (angle offset-pt1 p2) a12)
			(* pi 0.5)
			(* pi -0.5)
		      )
	    dist      (calc-distance p1 p2)
      )
      (setq offset-pt2 (polar offset-pt1 a12 dist)
	    out	       (cons offset-pt2 out)
      )
      (setq n 0)
      (while (< (+ n 2) (length pt-list))
	(setq pt1 (nth n pt-list)
	      pt2 (nth (+ n 1) pt-list)
	      pt3 (nth (+ n 2) pt-list)
	)
	(setq pt1b (polar pt1 (+ (angle pt1 pt2) delta-ang) dist)
	      pt2b (polar pt2 (+ (angle pt1 pt2) delta-ang) dist)
	      pt3b (polar pt2 (+ (angle pt2 pt3) delta-ang) dist)
	      pt4b (polar pt3 (+ (angle pt2 pt3) delta-ang) dist)
	)
	(setq inter (inters pt1b pt2b pt3b pt4b nil))
	(if inter
	  (setq out (cons inter out))
	)
	(setq n (1+ n))
      )
      (setq out
	     (cons (polar (nth (- (length pt-list) 1) pt-list)
			  (+ (angle (nth (- (length pt-list) 2) pt-list)
				    (nth (- (length pt-list) 1) pt-list)
			     )
			     delta-ang
			  )
			  dist
		   )
		   out
	     )
      )
      (reverse out)
    )
    nil
  )
)
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================