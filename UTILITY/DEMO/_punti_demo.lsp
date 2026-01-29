;;;=============================================================================
;;; 8. FUNZIONI DIMOSTRATIVE
;;;=============================================================================

;;;------------------------------------------------------------------------------
;;; PUNTO-A-STRINGA
;;; Converte un punto in stringa per visualizzazione
;;; Uso interno per funzioni demo
;;;------------------------------------------------------------------------------
(defun punto-a-stringa (pt / str)
  (if pt
    (progn
      (setq str "(")
      (setq str (strcat str (rtos (car pt) 2 2)))
      (setq str (strcat str " "))
      (setq str (strcat str (rtos (cadr pt) 2 2)))
      (if (caddr pt)
	(progn
	  (setq str (strcat str " "))
	  (setq str (strcat str (rtos (caddr pt) 2 2)))
	)
      )
      (setq str (strcat str ")"))
      str
    )
    "nil"
  )
)

;;;------------------------------------------------------------------------------
;;; DEMO-CALCOLI-BASE
;;; Dimostra operazioni base di distanza e coordinate
;;;------------------------------------------------------------------------------
(defun c:demo-calcoli-base ()
  (princ "\n=== DEMO CALCOLI BASE ===")
  (setq	pt1 '(0 0)
	pt2 '(3 4)
  )
  (princ (strcat "\nDistanza da "
		 (punto-a-stringa pt1)
		 " a "
		 (punto-a-stringa pt2)
		 " = "
		 (rtos (calc-distance pt1 pt2) 2 3)
	 )
  )
  (setq mid (calc-midpoint pt1 pt2))
  (princ (strcat "\nPunto medio = " (punto-a-stringa mid)))
  (setq triangle '((0 0) (10 0) (5 10)))
  (setq center (calc-centroid triangle))
  (princ (strcat "\nBaricentro triangolo = "
		 (punto-a-stringa center)
	 )
  )
  (command "_.line" pt1 pt2 "")
  (command "_.circle" mid 0.5)
  (command "_.pline"
	   (nth 0 triangle)
	   (nth 1 triangle)
	   (nth 2 triangle)
	   "_c"
  )
  (command "_.circle" center 0.5)
  (princ "\n=== Demo completata ===")
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-OPERAZIONI-RETTANGOLO
;;; Dimostra creazione rettangolo e calcolo perimetro
;;;------------------------------------------------------------------------------
(defun c:demo-rett ()
  (princ "\n=== DEMO OPERAZIONI RETTANGOLO ===")
  (setq	corner1	'(0 0)
	corner2	'(10 5)
  )
  (setq rect-pts (create-rectangle-points corner1 corner2))
  (princ (strcat "\nAngoli rettangolo: "
		 (punto-a-stringa (nth 0 rect-pts))
		 " "
		 (punto-a-stringa (nth 1 rect-pts))
		 " "
		 (punto-a-stringa (nth 2 rect-pts))
		 " "
		 (punto-a-stringa (nth 3 rect-pts))
	 )
  )
  (setq perim (calc-perimeter rect-pts))
  (princ (strcat "\nPerimetro = " (rtos perim 2 2)))
  (command "_.pline")
  (foreach pt rect-pts (command pt))
  (command "")

  ;; CORREZIONE: Usa entmake per creare il testo (pi? compatibile)
  (setq	text-pt	     (calc-midpoint corner1 corner2)
	text-height  0.5
	text-content (strcat "P=" (rtos perim 2 1))
  )

  (entmake (list '(0 . "TEXT")
		 '(100 . "AcDbEntity")
		 '(100 . "AcDbText")
		 (cons 10 text-pt)	; Punto di inserimento
		 (cons 40 text-height)	; Altezza testo
		 (cons 1 text-content)	; Contenuto testo
		 (cons 50 0.0)		; Angolo rotazione (0 gradi)
		 '(72 . 1)		; Allineamento orizzontale: Centro
		 (cons 11 text-pt)	; Punto di allineamento (per allineamento centro)
		 '(73 . 2)		; Allineamento verticale: Centro medio
	   )
  )

  (princ "\n=== Demo completata ===")
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-PUNTO-VICINO
;;; Dimostra ricerca punti pi? vicini in una lista
;;;------------------------------------------------------------------------------
(defun c:demo-vicino ()
  (princ "\n=== DEMO PUNTO PIU' VICINO ===")
  (setq ref-pt '(5 5))
  (setq pt-cloud '((0 0) (10 0) (10 10) (0 10) (5 2) (8 8)))
  (setq nearest (find-nearest-point ref-pt pt-cloud))
  (setq dist (calc-distance ref-pt nearest))
  (princ
    (strcat "\nPunto riferimento: " (punto-a-stringa ref-pt))
  )
  (princ
    (strcat "\nPunto piu' vicino: " (punto-a-stringa nearest))
  )
  (princ (strcat "\nDistanza: " (rtos dist 2 3)))
  (command "_.circle" ref-pt 0.5)
  (foreach pt pt-cloud
    (command "_.circle" pt 0.3)
  )
  (command "_.line" ref-pt nearest "")
  (princ "\n=== Demo completata ===")
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-GET-X-COORDS
;;; Dimostra estrazione coordinate X
;;;------------------------------------------------------------------------------
(defun c:demo-get-x-coords ()
  (princ "\n=== DEMO get-x-coords / get-max-x ===")
  (setq pts '((1 2) (3 4) (5 6)))
  (princ (strcat "\nPunti: "
		 (punto-a-stringa (nth 0 pts))
		 " "
		 (punto-a-stringa (nth 1 pts))
		 " "
		 (punto-a-stringa (nth 2 pts))
	 )
  )
  (princ (strcat "\nX coords: "
		 (princ-to-string (get-x-coords pts))
	 )
  )
  (princ (strcat "\nMax X: " (rtos (get-max-x pts) 2 2)))
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-CALC-CUMULATIVE-DISTANCE
;;;------------------------------------------------------------------------------
(defun c:demo-calc-cumulative-distance ()
  (princ "\n=== DEMO calc-cumulative-distance ===")
  (setq pts '((0 0) (5 0) (10 0)))
  (setq target '(5 0))
  (princ (strcat "\nTarget: " (punto-a-stringa target)))
  (princ
    (strcat "\nDistanza cumulativa: "
	    (rtos (calc-cumulative-distance target pts) 2 3)
    )
  )
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-FIND-NEAREST-PAIR
;;;------------------------------------------------------------------------------
(defun c:demo-find-nearest-pair	()
  (princ
    "\n=== DEMO find-nearest-point / find-nearest-pair ==="
  )
  (setq	ref '(5 1)
	pts '((0 0) (5 0) (10 0))
  )
  (setq pair (find-nearest-pair ref pts))
  (princ (strcat "\nReference: " (punto-a-stringa ref)))
  (princ (strcat "\nNearest pair: "
		 (punto-a-stringa (car pair))
		 " , "
		 (punto-a-stringa (cadr pair))
	 )
  )
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-IS-POINT-NEAR-LIST
;;;------------------------------------------------------------------------------
(defun c:demo-is-point-near-list ()
  (princ "\n=== DEMO is-point-near-list ===")
  (setq	test '(1 1)
	pts  '((0 0) (2 2) (5 5))
  )
  (princ (strcat "\nTest point: " (punto-a-stringa test)))
  (princ (strcat "\nIs near: "
		 (if (is-point-near-list test pts)
		   "SI"
		   "NO"
		 )
	 )
  )
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-CALC-PERPENDICULAR-ANGLE
;;;------------------------------------------------------------------------------
(defun c:demo-calc-perpendicular-angle ()
  (princ "\n=== DEMO calc-perpendicular-angle ===")
  (setq	select-pt '(2 2)
	triple	  '((0 0) (5 0) (5 5))
  )
  (setq ang (calc-perpendicular-angle select-pt triple))
  (princ (strcat "\nSelect: "
		 (punto-a-stringa select-pt)
		 " Triple: "
		 (punto-a-stringa (nth 0 triple))
		 ","
		 (punto-a-stringa (nth 1 triple))
		 ","
		 (punto-a-stringa (nth 2 triple))
	 )
  )
  (if (and ang (numberp ang))
    (princ
      (strcat "\nPerpendicular angle (rad): " (rtos ang 2 4))
    )
    (princ
      "\nPerpendicular angle: valore non numerico o errore nella funzione"
    )
  )
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-FILTER-POINTS-ABOVE-Y
;;;------------------------------------------------------------------------------
(defun c:demo-filter-points-above-y ()
  (princ "\n=== DEMO filter-points-above-y ===")
  (setq pts '((0 0) (1 2) (2 5) (3 1)))
  (setq res (filter-points-above-y pts 1.5))
  (princ (strcat "\nInput pts: " (princ-to-string pts)))
  (princ
    (strcat "\nFiltered (Y>1.5): " (princ-to-string res))
  )
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-CHECK-POINT-ORIENTATION
;;;------------------------------------------------------------------------------
(defun c:demo-check-point-orientation ()
  (princ "\n=== DEMO check-point-orientation ===")
  (setq	p1 '(2 2)
	p2 '(1 1)
  )
  (princ (strcat "\nP1: "
		 (punto-a-stringa p1)
		 " P2: "
		 (punto-a-stringa p2)
	 )
  )
  (setq res (check-point-orientation p1 p2))
  (princ (strcat "\nResult: "
		 (if res
		   "T"
		   "NIL"
		 )
	 )
  )
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-OFFSET-POLYLINE
;;;------------------------------------------------------------------------------
(defun c:demo-offset-polyline ()
  (princ "\n=== DEMO offset-polyline ===")
  (princ
    "\nSpecifica punto riferimento offset quando richiesto."
  )
  (setq pts '((0 0) (5 0) (5 5) (0 5)))
  (princ
    (strcat "\nInput poly pts: " (princ-to-string pts))
  )
  (setq out (offset-polyline pts))
  (if out
    (progn
      (princ
	(strcat "\nOffset points count: " (itoa (length out)))
      )
      (command "_.pline")
      (foreach p out (command p))
      (command "")
    )
    (princ "\nOffset non creato o utente annullato.")
  )
  (princ)
)


;;;------------------------------------------------------------------------------
;;; DEMO-SELEZIONE-INTERATTIVA
;;; Dimostra selezione interattiva punti e analisi
;;;------------------------------------------------------------------------------
(defun c:demo-selezione	()
  (princ "\n=== DEMO SELEZIONE INTERATTIVA ===")
  (princ
    "\nSeleziona punti da analizzare (Invio al termine)..."
  )
  (setq pts (select-points-interactive))
  (if (and pts (> (length pts) 0))
    (progn
      (princ
	(strcat "\n" (itoa (length pts)) " punti selezionati")
      )
      (if (> (length pts) 1)
	(progn
	  (setq center (calc-centroid pts))
	  (setq max-x (get-max-x pts))
	  (princ (strcat "\nBaricentro: " (punto-a-stringa center)))
	  (princ (strcat "\nCoordinata X massima: " (rtos max-x 2 3)))
	  (command "_.circle" center 1.0)
	  (zoom-to-points pts)
	)
      )
      (princ "\n=== Demo completata ===")
    )
    (princ "\nNessun punto selezionato.")
  )
  (princ)
)

;;;------------------------------------------------------------------------------
;;; TEST-PROJECTION
;;; Test completo delle funzioni di proiezione
;;;------------------------------------------------------------------------------
(defun c:test-projection
       (/ seg-pt1 seg-pt2 test-pts proj-pt param dist i)
  (princ
    "\n+========================================================+"
  )
  (princ
    "\n|          TEST PROIEZIONE PUNTO SU SEGMENTO            |"
  )
  (princ
    "\n+========================================================+"
  )

  (setq	seg-pt1	'(0.0 0.0 0.0)
	seg-pt2	'(10.0 0.0 0.0)
  )

  (princ (strcat "\n\nSegmento: "
		 (punto-a-stringa seg-pt1)
		 " -> "
		 (punto-a-stringa seg-pt2)
	 )
  )

  (setq	test-pts (list
		   '(5.0 5.0 0.0)	; Sopra centro
		   '(-2.0
		     3.0
		     0.0
		    )			; Prima inizio
		   '(12.0
		     4.0
		     0.0
		    )			; Dopo fine
		   '(0.0 0.0 0.0)	; Su inizio
		   '(10.0
		     0.0
		     0.0
		    )			; Su fine
		   '(5.0 0.0 0.0)
		 )
  )					; Su centro

  (setq i 1)
  (foreach pt test-pts
    (princ (strcat "\n\n--- Test " (itoa i) " ---"))
    (princ (strcat "\nPunto: " (punto-a-stringa pt)))

    (setq proj-pt (project-point-to-segment seg-pt1 seg-pt2 pt T))
    (princ (strcat "\nProiezione vincolata: "
		   (if proj-pt
		     (punto-a-stringa proj-pt)
		     "NIL (fuori)"
		   )
	   )
    )

    (setq proj-pt (project-point-to-segment seg-pt1 seg-pt2 pt nil))
    (princ (strcat "\nProiezione libera: "
		   (if proj-pt
		     (punto-a-stringa proj-pt)
		     "NIL"
		   )
	   )
    )

    (setq param (get-projection-parameter seg-pt1 seg-pt2 pt))
    (if	param
      (princ (strcat "\nParametro t: " (rtos param 2 4)))
    )

    (setq dist (distance-point-to-segment seg-pt1 seg-pt2 pt))
    (princ (strcat "\nDistanza minima: " (rtos dist 2 4)))

    (princ
      (strcat "\nSul segmento: "
	      (if (is-point-on-segment seg-pt1 seg-pt2 pt 0.001)
		"SI"
		"NO"
	      )
      )
    )

    (setq i (1+ i))
  )

  (princ
    "\n\n+========================================================+"
  )
  (princ
    "\n|                  TEST COMPLETATO                       |"
  )
  (princ
    "\n+========================================================+"
  )
  (princ)
)

;;;------------------------------------------------------------------------------
;;; TEST-PROJ-VISUAL
;;; Test visivo interattivo delle proiezioni
;;;------------------------------------------------------------------------------
(defun c:test-proj-visual
       (/ seg-pt1 seg-pt2 test-pt proj-pt old-cmdecho)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (princ "\n=== TEST VISIVO PROIEZIONE ===")
  (princ "\n\nSeleziona primo punto del segmento...")
  (setq seg-pt1 (getpoint))

  (if seg-pt1
    (progn
      (princ "\nSeleziona secondo punto del segmento...")
      (setq seg-pt2 (getpoint seg-pt1))

      (if seg-pt2
	(progn
	  (command "_.LINE" seg-pt1 seg-pt2 "")

	  (princ "\nSeleziona punto da proiettare...")
	  (setq test-pt (getpoint))

	  (if test-pt
	    (progn
	      (command "_.CIRCLE" test-pt 0.5)

	      (setq proj-pt (project-point-to-segment
			      seg-pt1
			      seg-pt2
			      test-pt
			      T
			    )
	      )

	      (if proj-pt
		(progn
		  (command "_.CIRCLE" proj-pt 0.3)
		  (command "_.LINE" test-pt proj-pt "")

		  (princ (strcat "\n\nProiezione: "
				 (punto-a-stringa proj-pt)
			 )
		  )
		  (princ (strcat "\nDistanza: "
				 (rtos (distance test-pt proj-pt) 2 4)
			 )
		  )
		)
		(princ "\n\nProiezione fuori dal segmento!")
	      )
	    )
	    (princ "\n\nPunto non selezionato")
	  )
	)
	(princ "\n\nSegmento non completato")
      )
    )
    (princ "\n\nOperazione annullata")
  )

  (setvar "CMDECHO" old-cmdecho)
  (princ)
)

;;;------------------------------------------------------------------------------
;;; DEMO-TUTTO
;;; Esegue tutte le funzioni dimostrative in sequenza
;;;------------------------------------------------------------------------------
(defun c:demo-tutto ()
  (princ "\n")
  (princ
    "\n+========================================================+"
  )
  (princ
    "\n|  UTILITA' GEOMETRIA PUNTI - DIMOSTRAZIONE COMPLETA    |"
  )
  (princ
    "\n+========================================================+"
  )
  (princ "\n")
  (c:demo-calcoli-base)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-get-x-coords)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-calc-cumulative-distance)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-find-nearest-pair)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-is-point-near-list)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-rett)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-calc-perpendicular-angle)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:test-projection)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:test-proj-visual)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-vicino)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-selezione)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-filter-points-above-y)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-check-point-orientation)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-offset-polyline)
  (princ "\n")
  (princ
    "\n+========================================================+"
  )
  (princ
    "\n|              TUTTE LE DEMO COMPLETATE                  |"
  )
  (princ
    "\n+========================================================+"
  )
  (princ)
)