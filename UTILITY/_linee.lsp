;;;=============================================================================
;;; GESTIONE LINEE
;;; Ultimo aggiornamento: 12/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per operazioni sulle linee
;;; Dipendenze esterne: SAME-SIGN (_mat.lsp), GET-ENTITY-TYPE, GET-ENTITY-ATTRIBUTE SET-ENTITY-TYPE (_entita.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. IMPOSTAZIONI E DEFAULT LINEE
;;; 2. CREAZIONE LINEE
;;; 3. CALCOLO INTERSEZIONI
;;; 4. RICERCA E SELEZIONE LINEE
;;; 5. OPERAZIONI TAGLIO
;;; 6. OPERAZIONI UNIONE
;;; 7. ESTRAZIONE VERTICI
;;;=============================================================================
;;; 1. IMPOSTAZIONI E DEFAULT LINEE
;;;=============================================================================
;;; SET-LINE-DEFAULTS
;;; Imposta i valori predefiniti per linee (tipo, colore, peso, layer)
;;; Argomenti:
;;;   Nessuno
;;; Restituisce:
;;;   T se eseguito correttamente
;;; Note:
;;;   Non apre finestre di dialogo - usa solo variabili di sistema
(defun set-line-defaults ()
  ;; Imposta tipo linea CONTINUOUS
  (setvar "CELTYPE" "CONTINUOUS")
  ;; Imposta colore BYBLOCK (0)
  (setvar "CECOLOR" "BYBLOCK")
  ;; Imposta peso linea predefinito (0 = BYBLOCK o BYLAYER)
  (setvar "CELWEIGHT" -1)		; -1 = BYLAYER, -2 = BYBLOCK, -3 = DEFAULT
  ;; Imposta layer corrente a "0"
  (setvar "CLAYER" "0")
  ;; Imposta scala tipo linea corrente
  (setvar "CELTSCALE" 0.01)
  T
)
;;; GET-LINE-PROPERTIES
;;; Ottiene tutte le proprietà di una linea
;;; Argomenti:
;;;   line-ent - Entità linea
;;; Ritorna: Lista associativa con proprietà
(defun get-line-properties (line-ent / props)
  (if (= (get-entity-type line-ent) "LINE")
    (progn
      (setq
	props (list
		(cons 'start-point (get-entity-attribute line-ent 10))
		(cons 'end-point (get-entity-attribute line-ent 11))
		(cons 'layer (get-entity-attribute line-ent 8))
		(cons 'color (get-entity-attribute line-ent 62))
		(cons 'linetype (get-entity-attribute line-ent 6))
		(cons 'ltscale (get-entity-attribute line-ent 48))
		(cons 'lineweight (get-entity-attribute line-ent 370))
	      )
      )
      props
    )
    nil
  )
)
;;;=============================================================================
;;; 2. CREAZIONE LINEE
;;;=============================================================================
;;; INSERT-LINE-INTERACTIVE
;;; Inserisce linea interattivamente
;;; Ritorna: Entità linea creata
(defun insert-line-interactive (/ pt prev-pt)
  (setq pt (getpoint "\nPunto iniziale: "))
  (if pt
    (progn
      (command "_.LINE" pt)
      (setq prev-pt pt)
      (while (setq pt (getpoint	prev-pt
				"\nPunto successivo (Invio per uscire): "
		      )
	     )
	(command pt)
	(setq prev-pt pt)
      )
      (command "")
      (entlast)
    )
    nil
  )
)
;;; CREATE-LINE
;;; Crea linea tra due punti
;;; Argomenti:
;;;   pt1 - Punto iniziale
;;;   pt2 - Punto finale
;;;   layer - Layer (opzionale)
;;;   color - Colore (opzionale)
;;; Ritorna: Entità linea creata
(defun create-line (pt1 pt2 layer color / line-data)
  (setq	line-data (list	'(0 . "LINE")
			(cons 10 pt1)
			(cons 11 pt2)
		  )
  )
  (if layer
    (setq line-data (append line-data (list (cons 8 layer))))
  )
  (if color
    (setq line-data (append line-data (list (cons 62 color))))
  )
  (entmake line-data)
  (entlast)
)
;;;=============================================================================
;;; 3. CALCOLO INTERSEZIONI
;;;=============================================================================
;;; CALC-LINE-INTERSECTION
;;; Calcola punto di intersezione tra due linee
;;; Argomenti:
;;;   line1 - Lista (pt1 pt2) prima linea
;;;   line2 - Lista (pt3 pt4) seconda linea
;;; Ritorna: Punto intersezione o nil se non si intersecano
;;; Algoritmo robusto che gestisce tutti i casi
(defun calc-line-intersection (line1	 line2	   /	fuzzy	  p1
			       p2   p3	 p4   x1   x2	x3   x4	  y1
			       y2   y3	 y4   a1   b1	c1   r3	  r4
			       a2   b2	 c2   r1   r2	denom	  num
			       x    y
			      )
  (setq	fuzzy 1e-8
	p1    (car line1)
	p2    (cadr line1)
	p3    (car line2)
	p4    (cadr line2)
  )
  (if (or (equal p1 p3 fuzzy)
	  (equal p1 p4 fuzzy)
	  (equal p2 p3 fuzzy)
	  (equal p2 p4 fuzzy)
      )
    nil
    (progn
      (setq x1 (car p1)
	    x2 (car p2)
	    x3 (car p3)
	    x4 (car p4)
	    y1 (cadr p1)
	    y2 (cadr p2)
	    y3 (cadr p3)
	    y4 (cadr p4)
	    a1 (- y2 y1)
	    b1 (- x1 x2)
	    c1 (- (* x2 y1) (* x1 y2))
	    r3 (+ (* a1 x3) (* b1 y3) c1)
	    r4 (+ (* a1 x4) (* b1 y4) c1)
      )
      (if (and (not (equal r3 0.0 fuzzy))
	       (not (equal r4 0.0 fuzzy))
	       (same-sign r3 r4)
	  )
	nil
	(progn
	  (setq	a2 (- y4 y3)
		b2 (- x3 x4)
		c2 (- (* x4 y3) (* x3 y4))
		r1 (+ (* a2 x1) (* b2 y1) c2)
		r2 (+ (* a2 x2) (* b2 y2) c2)
	  )
	  (if (and (not (equal r1 0.0 fuzzy))
		   (not (equal r2 0.0 fuzzy))
		   (same-sign r1 r2)
	      )
	    nil
	    (progn
	      (setq denom (- (* a1 b2) (* a2 b1)))
	      (if (equal denom 0.0 fuzzy)
		nil
		(progn
		  (setq	num (- (* b1 c2) (* b2 c1))
			x   (/ num denom)
			num (- (* a2 c1) (* a1 c2))
			y   (/ num denom)
		  )
		  (list x y (caddr p1))
		)
	      )
	    )
	  )
	)
      )
    )
  )
)

;;;=============================================================================
;;; 4. RICERCA E SELEZIONE LINEE
;;;=============================================================================
;;; FIND-LINE-AT-POINT
;;; Trova linea in un punto selezionato
;;; Argomenti:
;;;   pt-sel - Punto selezione
;;; Ritorna: Lista (pt-start pt-middle pt-end) o nil
;;; pt-middle è il punto più vicino sulla linea
(defun find-line-at-point (pt-sel / aperture-old pt ent p1 p2)
  (setq aperture-old (getvar "aperture"))
  (setvar "aperture" 50)
  (if (= (getvar "snapmode") 0)
    (setq pt (osnap pt-sel "_NEA"))
    (setq pt pt-sel)
  )
  (setvar "aperture" aperture-old)
  (if (and pt (ssget pt))
    (progn
      (setq ent (ssname (ssget pt) 0))
      (if (= (get-entity-type ent) "LINE")
	(progn
	  (setq	p1 (get-entity-attribute ent 10)
		p2 (get-entity-attribute ent 11)
	  )
	  (list p1 pt p2)
	)
	(progn
	  (princ "\nNessuna linea trovata! Avvicinarsi con zoom.")
	  nil
	)
      )
    )
    nil
  )
)
;;; SELECT-LINE-INTERACTIVE
;;; Selezione interattiva di una linea
;;; Argomenti:
;;;   prompt - Messaggio prompt
;;; Ritorna: Entità linea o nil
(defun select-line-interactive (prompt / sel ent)
  (setq sel (entsel prompt))
  (if sel
    (progn
      (setq ent (car sel))
      (if (= (get-entity-type ent) "LINE")
	ent
	(progn
	  (princ "\nL'entita' selezionata non e' una linea!")
	  nil
	)
      )
    )
    nil
  )
)
;;;=============================================================================
;;; 5. OPERAZIONI TAGLIO
;;;=============================================================================
;;; TRIM-TWO-LINES
;;; Taglia/estende una linea fino a incontrare l'altra
;;; Argomenti:
;;;   line1-ent - Prima linea (da modificare)
;;;   line2-ent - Seconda linea (riferimento)
;;; Ritorna: T se successo
(defun trim-two-lines (line1-ent line2-ent / pt1 pt2 pt3 pt4 int-pt)
  (if (and line1-ent line2-ent)
    (progn
      (setq pt1	(get-entity-attribute line1-ent 10)
	    pt2	(get-entity-attribute line1-ent 11)
	    pt3	(get-entity-attribute line2-ent 10)
	    pt4	(get-entity-attribute line2-ent 11)
      )
      (setq int-pt (inters pt1 pt2 pt3 pt4 nil))
      (if int-pt
	(progn
	  (set-entity-attribute line1-ent 10 int-pt)
	  T
	)
	nil
      )
    )
    nil
  )
)
;;; FIND-BREAK-POINTS
;;; Trova punti di taglio su una linea
;;; Argomenti:
;;;   line-info - Lista (pt1 pt-middle pt2) da find-line-at-point
;;;   break-length - Lunghezza interruzione
;;; Ritorna: Lista (pt-break1 pt-break2) o nil
(defun find-break-points (line-info break-length	/
			  p1	    pt	      p2	line-length
			  p11	    p22	      d1	d2
			  dmax
			 )
  (if line-info
    (progn
      (setq p1		(car line-info)
	    pt		(cadr line-info)
	    p2		(caddr line-info)
	    line-length	(distance p1 p2)
      )
      (if (> line-length break-length)
	(progn
	  (setq	p11  (polar pt (angle p2 p1) (/ break-length 2.0))
		p22  (polar pt (angle p1 p2) (/ break-length 2.0))
		d1   (distance p1 p22)
		d2   (distance p2 p11)
		dmax (max d1 d2)
	  )
	  (if (> line-length dmax)
	    (list p11 p22)
	    nil
	  )
	)
	nil
      )
    )
    nil
  )
)
;;; BREAK-LINE
;;; Taglia una linea tra due punti
;;; Argomenti:
;;;   pt1 - Primo punto taglio
;;;   pt2 - Secondo punto taglio
;;; Ritorna: T
(defun break-line (pt1 pt2)
  (command "_.break" pt1 pt2)
  T
)
;;; BREAK-LINE-AT-POINT
;;; Taglia linea in un punto con interruzione specifica
;;; Argomenti:
;;;   pt-sel - Punto selezione
;;;   break-length - Lunghezza interruzione
;;; Ritorna: T se successo
(defun break-line-at-point (pt-sel break-length / line-info break-pts)
  (setq line-info (find-line-at-point pt-sel))
  (if line-info
    (progn
      (setq break-pts (find-break-points line-info break-length))
      (if break-pts
	(progn
	  (break-line (car break-pts) (cadr break-pts))
	  T
	)
	(progn
	  (princ "\nLinea troppo corta per l'interruzione!")
	  nil
	)
      )
    )
    nil
  )
)
;;;=============================================================================
;;; 6. OPERAZIONI UNIONE
;;;=============================================================================
;;; JOIN-LINES
;;; La funzione join-lines crea una linea risultante che collega i punti pi� lontani delle due linee originali rispetto al punto di selezione pt1
;;; Argomenti:
;;;   pt1 - Primo punto area ricerca
;;;   pt2 - Secondo punto area ricerca
;;; Ritorna: Entit� linea creata o nil
(defun join-lines (pt1	     pt2       /	 ss	   idx
		   ent	     ent-data  line-count	   color
		   layer     ltscale   lineweight	   linetype
		   start-pt  end-pt    temp-pt
		  )
  (setq ss (ssget "_C" pt1 pt2))
  (if (and ss (= (sslength ss) 2))	; Assicura che ci siano esattamente due linee selezionate
    (progn
      (setq idx	0
	    line-count
	     0
      )
      (while (setq ent (ssname ss idx))
	(setq ent-data (entget ent))
	(if (= (cdr (assoc 0 ent-data)) "LINE")
	  (progn
	    (setq line-count (1+ line-count)
		  color	     (cdr (assoc 62 ent-data))
		  layer	     (cdr (assoc 8 ent-data))
		  ltscale    (cdr (assoc 48 ent-data))
		  lineweight (cdr (assoc 370 ent-data))
		  linetype   (cdr (assoc 6 ent-data))
		  temp-pt    (cdr (assoc 10 ent-data))
	    )
	    (if	(< (distance temp-pt pt1)
		   (distance (cdr (assoc 11 ent-data)) pt1)
		)
	      (setq temp-pt (cdr (assoc 11 ent-data)))
	    )
	    (entdel ent)
	    (cond
	      ((= line-count 1) (setq start-pt temp-pt))
	      ((= line-count 2) (setq end-pt temp-pt))
	    )
	  )
	)
	(setq idx (1+ idx))
      )
      (if (and start-pt end-pt layer)
	(if (null color)
	  (progn
	    (entmake (list '(0 . "LINE")
			   (cons 8 layer)
			   (cons 10 start-pt)
			   (cons 11 end-pt)
		     )
	    )
	    (command "_.change" (entlast) "" "_p" "_co" "_BYLAYER" "")
	  )
	  (cond
	    ((and linetype lineweight ltscale)
	     (entmake (list '(0 . "LINE")
			    (cons 62 color)
			    (cons 8 layer)
			    (cons 10 start-pt)
			    (cons 11 end-pt)
			    (cons 48 ltscale)
			    (cons 370 lineweight)
			    (cons 6 linetype)
		      )
	     )
	    )
	    ((and linetype lineweight)
	     (entmake (list '(0 . "LINE")
			    (cons 62 color)
			    (cons 8 layer)
			    (cons 10 start-pt)
			    (cons 11 end-pt)
			    (cons 370 lineweight)
			    (cons 6 linetype)
		      )
	     )
	    )
	    ((and linetype ltscale)
	     (entmake (list '(0 . "LINE")
			    (cons 62 color)
			    (cons 8 layer)
			    (cons 10 start-pt)
			    (cons 11 end-pt)
			    (cons 48 ltscale)
			    (cons 6 linetype)
		      )
	     )
	    )
	    ((and lineweight ltscale)
	     (entmake (list '(0 . "LINE")
			    (cons 62 color)
			    (cons 8 layer)
			    (cons 10 start-pt)
			    (cons 11 end-pt)
			    (cons 48 ltscale)
			    (cons 370 lineweight)
		      )
	     )
	    )
	    (linetype
	     (entmake (list '(0 . "LINE")
			    (cons 62 color)
			    (cons 8 layer)
			    (cons 10 start-pt)
			    (cons 11 end-pt)
			    (cons 6 linetype)
		      )
	     )
	    )
	    (ltscale
	     (entmake (list '(0 . "LINE")
			    (cons 62 color)
			    (cons 8 layer)
			    (cons 10 start-pt)
			    (cons 11 end-pt)
			    (cons 48 ltscale)
		      )
	     )
	    )
	    (lineweight
	     (entmake (list '(0 . "LINE")
			    (cons 62 color)
			    (cons 8 layer)
			    (cons 10 start-pt)
			    (cons 11 end-pt)
			    (cons 370 lineweight)
		      )
	     )
	    )
	    (T
	     (entmake (list '(0 . "LINE")
			    (cons 62 color)
			    (cons 8 layer)
			    (cons 10 start-pt)
			    (cons 11 end-pt)
		      )
	     )
	    )
	  )
	)
      )
    )
    (progn
      (princ "\nSelezionare esattamente due linee.")
      nil
    )
  )
)
;;;=============================================================================
;;; CONNECT-LINES-WITH-SEGMENT
;;; Crea una nuova linea che collega i punti più vicini di due linee esistenti
;;; Argomenti:
;;;   pt1 - Primo punto area ricerca
;;;   pt2 - Secondo punto area ricerca
;;; Ritorna: Entità linea creata o nil
(defun connect-lines-with-segment (pt1	       pt2	   /
				   ss	       lines	   line1
				   line2       line1-data  line2-data
				   p1	       p2	   p3
				   p4	       closest-pair
				   connection-pt1
				   connection-pt2	   layer
				   color       linetype	   ltscale
				   lineweight
				  )

  ;; Trova le due linee più vicine ai punti di selezione
  (setq ss (ssget "_C" pt1 pt2 '((0 . "LINE"))))
  (if (and ss (>= (sslength ss) 2))
    (progn
      ;; Seleziona le prime due linee trovate
      (setq line1      (ssname ss 0)
	    line2      (ssname ss 1)
	    line1-data (entget line1)
	    line2-data (entget line2)
      )
      ;; Ottieni i punti delle linee
      (setq p1 (cdr (assoc 10 line1-data))
	    p2 (cdr (assoc 11 line1-data))
	    p3 (cdr (assoc 10 line2-data))
	    p4 (cdr (assoc 11 line2-data))
      )
      ;; Trova la coppia di punti pi� vicini tra le due linee
      (setq closest-pair   (find-closest-endpoints p1 p2 p3 p4)
	    connection-pt1 (car closest-pair)
	    connection-pt2 (cadr closest-pair)
      )
      ;; Ottieni le propriet� dalla prima linea
      (setq layer      (cdr (assoc 8 line1-data))
	    color      (cdr (assoc 62 line1-data))
	    linetype   (cdr (assoc 6 line1-data))
	    ltscale    (cdr (assoc 48 line1-data))
	    lineweight (cdr (assoc 370 line1-data))
      )
      ;; Crea la nuova linea di connessione
      (setq new-line-data
	     (list
	       '(0 . "LINE")
	       (cons 8 layer)
	       (cons 10 connection-pt1)
	       (cons 11 connection-pt2)
	     )
      )

      ;; Aggiungi gli attributi opzionali se presenti
      (if color
	(setq new-line-data
	       (append new-line-data (list (cons 62 color)))
	)
      )
      (if linetype
	(setq new-line-data
	       (append new-line-data
		       (list (cons 6 linetype))
	       )
	)
      )
      (if ltscale
	(setq new-line-data
	       (append new-line-data
		       (list (cons 48 ltscale))
	       )
	)
      )
      (if lineweight
	(setq new-line-data
	       (append new-line-data
		       (list (cons 370 lineweight))
	       )
	)
      )
      ;; Crea la nuova linea
      (entmake new-line-data)
      (entlast)
    )
    (progn
      (princ "\nSelezionare almeno due linee.")
      nil
    )
  )
)
;;=============================================================================
;;; FIND-CLOSEST-ENDPOINTS
;;; Trova la coppia di punti più vicini tra due linee
;;; Argomenti: quattro punti (due per linea)
;;; Ritorna: lista con i due punti più vicini
(defun find-closest-endpoints
			      (p1 p2 p3 p4 / d13 d14 d23 d24 min-dist)
  (setq	d13	 (distance p1 p3)
	d14	 (distance p1 p4)
	d23	 (distance p2 p3)
	d24	 (distance p2 p4)
	min-dist (min d13 d14 d23 d24)
  )
  (cond
    ((equal min-dist d13 1e-8) (list p1 p3))
    ((equal min-dist d14 1e-8) (list p1 p4))
    ((equal min-dist d23 1e-8) (list p2 p3))
    ((equal min-dist d24 1e-8) (list p2 p4))
  )
)
;;;=============================================================================
;;; 7. ESTRAZIONE VERTICI
;;;=============================================================================
;;; EXTRACT-LINE-VERTICES
;;; Estrae vertici da selezione interattiva di linee
;;; Ritorna: Lista punti vertici in sequenza
(defun extract-line-vertices (/ sel pt1 pt2 pt-list)
  (setq pt-list nil)
  (while (setq sel (nentsel
		     "\nSeleziona linee in sequenza (Invio per finire): "
		   )
	 )
    (setq pt1 (get-entity-attribute (car sel) 10)
	  pt2 (get-entity-attribute (car sel) 11)
    )
    (if	(not (member pt1 pt-list))
      (setq pt-list (cons pt1 pt-list))
    )
    (if	(not (member pt2 pt-list))
      (setq pt-list (cons pt2 pt-list))
    )
    (reverse pt-list)
  )
  (princ)
)

;;;=============================================================================
;;; FINE FILE
;;;=============================================================================
