;;;=============================================================================
;;; GESTIONE LWPOLYLINE/POLYLINE
;;; Ultimo aggiornamento: 22/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per operazioni sulle polilinee
;;; Dipendenze esterne: get-entity-type, get-entity-width, set-entity-attribute (_entita.lsp)
;;;                     reverse-list, insert-at-position (_liste.lsp)
;;;                     calc-midpoint, zoom-to-points (_punti.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1 GESTIONE VERTICI - ESTRAZIONE E INFO
;;; 2 RICERCA E INDICIZZAZIONE
;;; 3 MODIFICA/INSERIMENTO VERTICI
;;; 4 RIMOZIONE/AGGIORNAMENTO VERTICI
;;; 5 TRATTI E GEOMETRIA
;;; 6 MISURE E CALCOLI
;;; 7 CREAZIONE E VISUALIZZAZIONE
;;;=============================================================================
;;; 1 GESTIONE VERTICI - ESTRAZIONE E INFO
;;;=============================================================================
;;; GET-POLY-VERTICES
;;; Estrae vertici da entità  POLYLINE o LWPOLYLINE (lista di punti)
;;; Argomenti: ent ; Ritorna: lista punti
;;; Esempio di utilizzo: (get-poly-vertices ent)
;;; Nota: funziona anche con POLYLINE 3D ma non con POLYLINE 2D
(defun get-poly-vertices (ent / data vertices item) 
  (setq data     (entget ent)
        vertices '()
  )
  (foreach item data 
    (if (= (car item) 10) 
      (setq vertices (cons (cdr item) vertices))
    )
  )
  (reverse vertices)
)
;;; GET-LWPOLYLINE-VERTICES
;;; Alias più esplicito su LWPOLYLINE
;;; Argomenti: ent ; Ritorna: lista punti
;;; Esempio di utilizzo: (get-lwpolyline-vertices ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-lwpolyline-vertices (ent) 
  (get-poly-vertices ent)
)
;;; GET-NUMBERED-LWPOLYLINE-VERTICES
;;; Restituisce ((0 . pt) (1 . pt) ...)
;;; Argomenti: ent ; Ritorna: lista associativa
;;; Esempio di utilizzo: (get-numbered-lwpolyline-vertices ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-numbered-lwpolyline-vertices (ent / data vertices idx) 
  (setq data     (entget ent)
        vertices '()
        idx      -1
  )
  (foreach item data 
    (if (= (car item) 10) 
      (progn 
        (setq idx (1+ idx))
        (setq vertices (cons (cons idx (cdr item)) vertices))
      )
    )
  )
  (reverse vertices)
)
;;; GET-VERTEX-BY-INDEX
;;; 0-based
;;; Argomenti: ent index ; Ritorna: punto o nil
;;; Esempio di utilizzo: (get-vertex-by-index ent 2)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-vertex-by-index (ent index / verts) 
  (setq verts (get-lwpolyline-vertices ent))
  (if (and verts (>= index 0) (< index (length verts))) 
    (nth index verts)
    nil
  )
)
;;; GET-VERTEX-COUNT
;;; Argomenti: ent ; Ritorna: intero
;;; Esempio di utilizzo: (get-vertex-count ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-vertex-count (ent) 
  (length (get-lwpolyline-vertices ent))
)
;;; GET-FIRST-VERTEX
;;; Argomenti: ent ; Ritorna: punto o nil
;;; Esempio di utilizzo: (get-first-vertex ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-first-vertex (ent) 
  (car (get-lwpolyline-vertices ent))
)
;;; GET-LAST-VERTEX
;;; Argomenti: ent ; Ritorna: punto o nil
;;; Esempio di utilizzo: (get-last-vertex ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-last-vertex (ent / v) 
  (setq v (get-lwpolyline-vertices ent))
  (if v 
    (nth (1- (length v)) v)
    nil
  )
)
;;; IS-POLYLINE-CLOSED
;;; Argomenti: ent ; Ritorna: T se chiusa, nil altrimenti
;;; Esempio di utilizzo: (is-polyline-closed ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun is-polyline-closed (ent / data flag) 
  (setq data (entget ent))
  (setq flag (cdr (assoc 70 data)))
  (if flag 
    (= (logand flag 1) 1)
    nil
  )
)
;;; GET-POLYLINE-PROPERTIES
;;; restituisce associazioni base
;;; Argomenti: ent ; Ritorna: lista associativa
;;; Esempio di utilizzo: (get-polyline-properties ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-polyline-properties (ent / d) 
  (setq d (entget ent))
  (list 
    (cons 'layer (cdr (assoc 8 d)))
    (cons 'color (cdr (assoc 62 d)))
    (cons 'linetype (cdr (assoc 6 d)))
    (cons 'closed (cdr (assoc 70 d)))
    (cons 'vertex-count (cdr (assoc 90 d)))
    (cons 'ltscale (cdr (assoc 48 d)))
    (cons 'lineweight (cdr (assoc 370 d)))
    (cons 'elevation (cdr (assoc 38 d)))
    (cons 'thickness (cdr (assoc 39 d)))
  )
)
;;;=============================================================================
;;; 2 RICERCA E INDICIZZAZIONE
;;;=============================================================================
(defun pt-equal-2d (p1 p2 tol)
  (and
    (< (abs (- (car  p1) (car  p2))) tol)
    (< (abs (- (cadr p1) (cadr p2))) tol)
  )
)

;;; FIND-VERTEX-POSITION - cerca punto nella lista, ritorna indice 0-based o -1
;;; Argomenti: vertex vertex-list ; Ritorna: indice o -1
;;; Esempio di utilizzo: (find-vertex-position '(10 20) '((0 0) (10 20) (30 40)))
;;; Nota: usa tolleranza 1e-6 per confronto punti 
(defun find-vertex-position (vertex vertex-list / pos len found)
  (cond
    ((or (not (listp vertex)) (not (listp vertex-list)))
     -1
    )
    (t
     (setq pos   0
           len   (length vertex-list)
           found nil
     )
     (while (and (not found) (< pos len))
       (if (pt-equal-2d vertex (nth pos vertex-list) 1e-6)
         (setq found T)
         (setq pos (1+ pos))
       )
     )
     (if found pos -1)
    )
  )
)

;;; FIND-VERTEX-AFTER-POINT - trova la posizione del vertice successivo del segmento che interseca il punto (1-based)
;;; Argomenti: point ent ; Ritorna: indice vertice segmento o nil
;;; Esempio di utilizzo: (find-segment-after-point '(10 20) ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun find-vertex-after-point (point ent / vlist pos v1 v2 perp1 perp2 result) 
  (if (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE") 
    (progn 
      (setq vlist  (get-lwpolyline-vertices ent)
            pos    0
            result nil
      )
      (while (and (nth pos vlist) (nth (1+ pos) vlist)) 
        (setq v1    (nth pos vlist)
              v2    (nth (1+ pos) vlist)
              perp1 (polar point (+ (/ pi 2) (angle v1 v2)) -0.1)
              perp2 (polar point (+ (/ pi 2) (angle v1 v2)) 0.1)
        )
        (if (inters v1 v2 perp1 perp2 nil) 
          (setq result (1+ pos))
        )
        (setq pos (1+ pos))
      )
      result
    )
    nil
  )
)
;;; FIND-SEGMENT-AT-POINT - ritorna (v1 v2 pos)
;;; Argomenti: point ent ; Ritorna: lista o nil
;;; Esempio di utilizzo: (find-segment-at-point '(10 20) ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun find-segment-at-point (point ent / pos verts) 
  (setq pos (find-vertex-after-point point ent))
  (if pos 
    (progn 
      (setq verts (get-lwpolyline-vertices ent))
      (list (nth (1- pos) verts) (nth pos verts) pos)
    )
    nil
  )
)
;;;=============================================================================
;;; 3 MODIFICA/INSERIMENTO VERTICI
;;;=============================================================================
;;; INSERT-VERTEX-AFTER-POSITION - versione robusta che mantiene larghezze/bulge e codice 90
;;; Argomenti: point entity prev-position(0-based) ; Ritorna: T o nil
;;; Esempio di utilizzo: (insert-vertex-after-position '(10 20) ent 2)
;;; Nota: funziona solo con LWPOLYLINE
(defun insert-vertex-after-position (point entity prev-position / new-vertex ent-data 
                                     result temp40 temp41 temp42 found new-data 
                                     vertex-count index
                                    ) 
  (setq new-vertex   (list (car point) (cadr point))
        ent-data     (entget entity)
        temp40       0.0
        temp41       0.0
        temp42       0.0
        found        nil
        result       '()
        vertex-count nil
        index        -1
  )
  (foreach item ent-data 
    (cond 
      ((= (car item) 90)
       (setq vertex-count (1+ (cdr item)))
       (setq result (cons (cons 90 vertex-count) result))
      )
      ((= (car item) 40)
       (setq temp40 (cdr item))
       (setq result (cons item result))
      )
      ((= (car item) 41)
       (setq temp41 (cdr item))
       (setq result (cons item result))
      )
      ((= (car item) 42)
       (setq temp42 (cdr item))
       (setq result (cons item result))
      )
      ((= (car item) 10)
       (setq index (1+ index))
       (if 
         (and (not found) 
              (equal (cdr item) 
                     (cdr 
                       (assoc prev-position 
                              (get-numbered-lwpolyline-vertices entity)
                       )
                     )
                     1e-6
              )
         )
         (progn 
           (setq found T)
           (setq result (cons item result))
           (setq result (cons (cons 10 new-vertex) result))
           (if (> temp40 0.0) (setq result (cons (cons 40 temp40) result)))
           (if (> temp41 0.0) (setq result (cons (cons 41 temp41) result)))
           (setq result (cons (cons 42 0.0) result))
           (setq temp40 0.0
                 temp41 0.0
                 temp42 0.0
           )
         )
         (progn 
           (setq result (cons item result))
           (setq temp40 0.0
                 temp41 0.0
                 temp42 0.0
           )
         )
       )
      )
      (T
       (setq result (cons item result))
      )
    )
  )
  (if found 
    (progn 
      (setq new-data (reverse result))
      (entmod new-data)
      (entupd entity)
      T
    )
    (progn 
      (princ "\nErrore: Vertice precedente non trovato.")
      nil
    )
  )
)
;;; INSERT-VERTEX-AFTER-POSITION-V2 - ricostruisce la polyline (piÃ¹ sicura in alcuni CAD)
;;; Argomenti: point entity prev-position ; Ritorna: T o nil
;;; Esempio di utilizzo: (insert-vertex-after-position-v2 '(10 20) ent 2)
;;; Nota: funziona solo con LWPOLYLINE
(defun insert-vertex-after-position-v2 (point entity prev-position / verts new-verts 
                                        edata layer color linetype closed i
                                       ) 
  (setq verts     (get-lwpolyline-vertices entity)
        new-verts '()
  )
  (if (or (< prev-position 0) (>= prev-position (length verts))) 
    (progn 
      (princ "\nErrore: Posizione non valida.")
      nil
    )
    (progn 
      (setq edata    (entget entity)
            layer    (cdr (assoc 8 edata))
            color    (cdr (assoc 62 edata))
            linetype (cdr (assoc 6 edata))
            closed   (cdr (assoc 70 edata))
            i        0
      )
      (foreach v verts 
        (setq new-verts (cons v new-verts))
        (if (= i prev-position) 
          (setq new-verts (cons (list (car point) (cadr point)) new-verts))
        )
        (setq i (1+ i))
      )
      (setq new-verts (reverse new-verts))
      (entdel entity)
      (create-lwpolyline-from-points new-verts layer color closed)
      T
    )
  )
)


;;; FUNZIONE D'APPOGGIO
;;;
;;; REPLACE-LWPOLYLINE-WITH-VERTICES
(defun replace-lwpolyline-with-vertices
       (ent verts / edata closed new-ent)

  ;; ------------------------------------------------------------
  ;; Sostituisce una LWPOLYLINE con una nuova,
  ;; usando la lista completa dei vertici passati.
  ;;
  ;; ent   : ename LWPOLYLINE originale
  ;; verts : lista ((x y) (x y) ...)
  ;;
  ;; Ritorna: ename della nuova polilinea oppure nil
  ;; ------------------------------------------------------------

  (if (and ent
           verts
           (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
      )

    (progn
      ;; --- Legge TUTTI i dati prima della cancellazione
      (setq edata (entget ent))

      ;; --- Stato closed (booleano sicuro)
      (setq closed
            (= 1 (logand 1 (cdr (assoc 70 edata))))
      )

      ;; --- Elimina entità originale
      (entdel ent)

      ;; --- Ricrea polilinea base
      (setq new-ent
            (create-lwpolyline-from-points
              verts
              (cdr (assoc 8  edata))   ;; layer
              (cdr (assoc 62 edata))   ;; colore
              closed                   ;; closed
            )
      )

      ;; --- Ripristino proprietà avanzate (solo se presenti)
      (foreach code '(6 48 370 210 38 43)
        (if (assoc code edata)
          (set-entity-attribute
            new-ent
            code
            (cdr (assoc code edata))
          )
        )
      )

      new-ent
    )

    (progn
      (princ "\nErrore: entità non valida o non LWPOLYLINE.")
      nil
    )
  )
)


;;; INSERT-VERTEX-AT-POINT
;;; Inserisce pt nel tratto di polilinea più vicino
;;; Se più vicino all'inizio ? inserisce come primo vertice
;;; Argomenti: ent pt
;;; Ritorna: pt o nil

(defun insert-vertex-at-point (ent pt / verts best-dist best-i i p1 p2 proj d new-verts)
  ;; Controllo entità valida
  (if (not (and ent pt))
    (exit)
  )

  ;; Estrae vertici
  (setq verts (get-lwpolyline-vertices ent))

  ;; Caso polilinea vuota o singolo vertice
  (if (< (length verts) 2)
    (exit)
  )

  ;; Inizializzazione
  (setq best-dist nil)
  (setq best-i    0)
  (setq i 0)

  ;; Analizza ogni segmento
  (while (< i (- (length verts) 1))
    (setq p1 (nth i verts))
    (setq p2 (nth (+ i 1) verts))

    ;; Proiezione punto su segmento
    (setq proj (project-point-to-segment  p1 p2 pt nil))
    (setq d (distance pt proj))

    ;; Aggiorna minimo
    (if (or (not best-dist) (< d best-dist))
      (progn
        (setq best-dist d)
        (setq best-i i)
      )
    )

    (setq i (+ i 1))
  )

  ;; Controllo: più vicino all'inizio?
  (if (< (distance pt (car verts)) best-dist)
    (setq best-i -1)
  )

  ;; Costruzione nuova lista vertici
  (setq new-verts nil)
  (setq i 0)

  (while (< i (length verts))
    ;; Inserimento prima del primo vertice
    (if (= best-i -1)
      (progn
        (setq new-verts (append new-verts (list pt)))
        (setq best-i -2) ;; disabilita
      )
    )

    ;; Inserimento dopo il segmento i
    (setq new-verts (append new-verts (list (nth i verts))))

    (if (= i best-i)
      (setq new-verts (append new-verts (list pt)))
    )

    (setq i (+ i 1))
  )

  ;; Sostituisce polilinea
  (replace-lwpolyline-with-vertices ent new-verts)

  pt
)


;;; APPEND-POLYLINE-VERTEX - aggiunge in coda mantenendo le proprietà
;;; Argomenti: ent pt ; Ritorna: pt o nil
;;; Esempio di utilizzo: (append-polyline-vertex ent '(30 40))
;;; Nota: funziona solo con LWPOLYLINE
;;; APPEND-POLYLINE-VERTEX
;;; Aggiunge un vertice in coda
(defun append-polyline-vertex
       (ent pt / verts new-verts)

  (if (and ent
           pt
           (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))

    (progn
      ;; --- vertici esistenti
      (setq verts (get-lwpolyline-vertices ent))

      ;; --- aggiunta in coda
      (setq new-verts
            (append verts (list (list (car pt) (cadr pt))))
      )

      ;; --- sostituzione tramite funzione di appoggio
      (replace-lwpolyline-with-vertices ent new-verts)

      pt
    )
    nil
  )
)


;;; APPEND-OR-PREPEND-POLYLINE-VERTEX - aggiunge all'estremo più vicino mantenendo le proprietà
;;; Argomenti: ent pt ; Ritorna: pt o nil
;;; Esempio di utilizzo: (append-polyline-or-prepend-vertex ent '(30 40))
;;; Nota: funziona solo con LWPOLYLINE
;;; APPEND-OR-PREPEND-POLYLINE-VERTEX
;;; Aggiunge il vertice all'estremo più vicino
(defun append-or-prepend-polyline-vertex
       (ent pt / verts first lastvx d-first d-last new-verts)

  (if (and ent
           pt
           (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))

    (progn
      ;; --- vertici
      (setq verts (get-lwpolyline-vertices ent))

      (if (< (length verts) 2)
        nil
        (progn
          ;; --- estremi
          (setq first  (car verts)
                lastvx (last verts))

          ;; --- distanze
          (setq d-first (distance pt first)
                d-last  (distance pt lastvx))

          ;; --- costruzione nuova lista
          (setq new-verts
                (if (< d-first d-last)
                  (cons (list (car pt) (cadr pt)) verts)
                  (append verts (list (list (car pt) (cadr pt))))
                )
          )

          ;; --- sostituzione centralizzata
          (replace-lwpolyline-with-vertices ent new-verts)

          pt
        )
      )
    )
    nil
  )
)



;;; ADD-POLYLINE-VERTEX - interattiva semplificata: aggiunge ptNew vicino a snap
;;; Argomenti: ptsel ptNew ; Ritorna: ptNew
;;; Esempio di utilizzo: (add-polyline-vertex ptsel '(30 40))
;;; Nota: funziona solo con LWPOLYLINE

(defun add-polyline-vertex (ptsel ptNew / apert snapPt ent)

  (setq apert (getvar "aperture"))
  (setvar "aperture" 20)
  (setq snapPt (osnap ptsel "_nea"))
  (setvar "aperture" apert)

  (if snapPt
    (setq ent (ssname (ssget snapPt) 0))
    (setq ent nil)
  )

  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (insert-vertex-at-point ent ptNew )
  )

  ptNew
)





;;;=============================================================================
;;; 4 RIMOZIONE/AGGIORNAMENTO VERTICI
;;;=============================================================================

;;; UPDATE-LWP-VERTEX - aggiorna vertice specifico oldVx->newVx
;;; Argomenti: ent oldVx newVx ; Ritorna: newVx o nil
;;; Esempio di utilizzo: (update-lwp-vertex ent '(10 20) '(15 25))
;;; Nota: funziona solo con LWPOLYLINE
(defun update-lwp-vertex (ent pickPt newVx / data newed bestIdx bestDist
                               idx item d)

  ;; ------------------------------------------------------------
  ;; Aggiorna il vertice di una LWPOLYLINE più vicino a pickPt
  ;;
  ;; ent    : LWPOLYLINE
  ;; pickPt: punto selezionato (WCS)
  ;; newVx : nuovo vertice (x y) o (x y z)
  ;;
  ;; Ritorna: newVx oppure nil
  ;; ------------------------------------------------------------

  (if (not (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")))
    (progn
      (princ "\nErrore: entità non valida.")
      nil
    )

    (progn
      ;; lettura DXF
      (setq data      (entget ent)
            bestIdx   nil
            bestDist  nil
            idx       -1
            newed     '()
      )

      ;; prima passata: trova indice del vertice più vicino
      (foreach item data
        (if (= (car item) 10)
          (progn
            (setq idx (1+ idx))
            (setq d (distance pickPt (cdr item)))
            (if (or (null bestDist) (< d bestDist))
              (setq bestDist d
                    bestIdx  idx)
            )
          )
        )
      )

      (if (null bestIdx)
        (progn
          (princ "\nNessun vertice trovato.")
          nil
        )

        (progn
          ;; seconda passata: ricostruzione entità
          (setq idx -1)

          (foreach item data
            (cond
              ;; vertice da sostituire
              ((= (car item) 10)
               (setq idx (1+ idx))
               (if (= idx bestIdx)
                 (setq newed
                       (append newed
                               (list (cons 10 (list (car newVx)
                                                    (cadr newVx))))))
                 (setq newed (append newed (list item)))
               )
              )

              ;; tutto il resto invariato
              (t
               (setq newed (append newed (list item)))
              )
            )
          )

          ;; applica modifica
          (entmod newed)
          (entupd ent)
          newVx
        )
      )
    )
  )
)


;;; REMOVE-POLYLINE-VERTEX - versione precedente più semplice (mantiene compatibilità )
;;; Argomenti: ptsel ; Ritorna: T o nil
;;; Esempio di utilizzo: (remove-polyline-vertex ptsel)
;;; Nota: funziona solo con LWPOLYLINE
(defun remove-polyline-vertex (ptsel / apert snapPt ent
                                      verts edata
                                      bestIdx bestDist
                                      idx v d newVerts newEnt)

  ;; ------------------------------------------------------------
  ;; Rimuove il vertice di una LWPOLYLINE più vicino a ptsel
  ;; Usa la stessa logica di _nea
  ;; ------------------------------------------------------------

  ;; --- snap NEA per individuare l'entità
  (setq apert (getvar "aperture"))
  (setvar "aperture" 20)
  (setq snapPt (osnap ptsel "_nea"))
  (setvar "aperture" apert)

  (if (not snapPt)
    (progn
      (princ "\nNessuna entità trovata.")
      nil
    )

    (progn
      (setq ent (ssname (ssget snapPt) 0))

      (if (not (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")))
        (progn
          (princ "\nEntità non valida.")
          nil
        )

        (progn
          ;; --- vertici e dati originali
          (setq verts (get-lwpolyline-vertices ent)
                edata (entget ent)
          )

          (if (< (length verts) 3)
            (progn
              (princ "\nImpossibile rimuovere: servono almeno 3 vertici.")
              nil
            )

            (progn
              ;; --- trova vertice più vicino
              (setq bestIdx  nil
                    bestDist nil
                    idx       0
              )

              (foreach v verts
                (setq d (distance snapPt v))
                (if (or (null bestDist) (< d bestDist))
                  (setq bestDist d
                        bestIdx  idx)
                )
                (setq idx (1+ idx))
              )

              ;; --- costruzione nuova lista vertici
              (setq newVerts '()
                    idx 0)

              (foreach v verts
                (if (/= idx bestIdx)
                  (setq newVerts (append newVerts (list v)))
                )
                (setq idx (1+ idx))
              )

              ;; --- elimina e ricrea polilinea
              (entdel ent)

              (setq newEnt
                    (create-lwpolyline-from-points
                      newVerts
                      (cdr (assoc 8  edata))   ;; layer
                      (cdr (assoc 62 edata))   ;; colore
                      (cdr (assoc 70 edata))   ;; closed
                    )
              )

              ;; --- ripristino proprietà DXF
              (foreach pair edata
                (if (and (numberp (car pair))
                         (not (member (car pair)
                                      '(0 10 20 30 70 90))))
                  (set-entity-attribute newEnt (car pair) (cdr pair))
                )
              )

              newEnt
            )
          )
        )
      )
    )
  )
)

;;; MOVE-LWP-VERTEX
;;; move-lwp-vertex - sposta il vertice selezionato nel nuovo punto
;;; Argomenti: pt1 ; Ritorna: nil
;;; Esempio di utilizzo: (move-lwp-vertex)
;;; Nota: funziona solo con LWPOLYLINE
(defun move-lwp-vertex ( pt1 / sel ent data verts nearest ndist pt2) 
  (if pt1 
    (progn 
      (setq sel (ssget pt1 '((0 . "LWPOLYLINE"))))
      (if sel 
        (progn 
          (setq ent   (ssname sel 0)
                data  (entget ent)
                verts '()
          )
          (foreach item data 
            (if (= (car item) 10) 
              (setq verts (append verts (list (cdr item))))
            )
          )
          (setq nearest nil
                ndist   1e99
          )
          (foreach item verts 
            (if (< (distance pt1 item) ndist) 
              (setq nearest item
                    ndist   (distance pt1 item)
              )
            )
          )
          (if nearest 
            (progn 
              (setq pt2 pt1)
              (if (setq pt2 (getpoint "\nSposta a: "))
		(progn
                (update-lwp-vertex ent nearest pt2)
                (setq nearest pt2)
		)
              )
              (if pt1 (setvar "lastpoint" pt1))
            )
            (princ "\nNessun vertice vicino al punto indicato.")
          )
        )
        (princ "\nNessuna polilinea trovata vicino al punto selezionato.")
      )
    )
  )
  (princ)
)
;;;=============================================================================
;;; 5 TRATTI E GEOMETRIA
;;;=============================================================================
;;; DISTANCE-TO-SEGMENT - distanza minima punto->segmento
;;; Argomenti: pt segment ; Ritorna: reale
;;; Esempio di utilizzo: (distance-to-segment '(10 20) '((0 0) (30 40)))
;;; Nota: segmento = (p1 p2)
(defun distance-to-segment (pt segment / p1 p2 len t_val projection) 
  (setq p1 (car segment)
        p2 (cadr segment)
  )
  (if (equal p1 p2 1e-8) 
    (distance pt p1)
    (progn 
      (setq len (distance p1 p2))
      (if (= len 0.0) 
        (distance pt p1)
        (progn 
          (setq t_val (/ 
                        (+ (* (- (car pt) (car p1)) (- (car p2) (car p1))) 
                           (* (- (cadr pt) (cadr p1)) (- (cadr p2) (cadr p1)))
                        )
                        (* len len)
                      )
          )
          (cond 
            ((<= t_val 0.0)
             (distance pt p1)
            )
            ((>= t_val 1.0)
             (distance pt p2)
            )
            (T
             (setq projection (list (+ (car p1) (* t_val (- (car p2) (car p1)))) 
                                    (+ (cadr p1) (* t_val (- (cadr p2) (cadr p1))))
                              )
             )
             (distance pt projection)
            )    
         )
        )
      )
    )
  )
)
;;; FIND-NEAREST-SEGMENT - dato punto e lista vertici trova il segmento piÃ¹ vicino
;;; Argomenti: pt vertex-list ; Ritorna: (p1 p2) o nil
;;; Esempio di utilizzo: (find-nearest-segment '(10 20) '((0 0) (30 40) (50 60)))
;; Nota: vertex-list = (v1 v2 v3 ...)
(defun find-nearest-segment (pt vertex-list / minDist best seg curDist) 
  (setq minDist 1e308
        best    nil
  )
  (while (and vertex-list (cdr vertex-list)) 
    (setq seg     (list (car vertex-list) (cadr vertex-list))
          curDist (distance-to-segment pt seg)
    )
    (if (< curDist minDist) 
      (progn 
        (setq minDist curDist)    
       (setq best seg)
      )
    )
    (setq vertex-list (cdr vertex-list))
  )
  best
)
;;; GET-POLY-SEGMENT-NEAR-POINT
;;; Trova il segmento più vicino al punto su una polilinea
;;; Argomenti: pt ent ; Ritorna: lista (p1 p2) o nil
;;; Esempio di utilizzo: (get-poly-segment-near-point pt ent)
(defun get-poly-segment-near-point (pt ent / vertexList)
  (setq vertexList (get-poly-vertices ent))
  (find-nearest-segment pt vertexList)
)
;;; GET-POLY-SEGMENT-AT-POINT - trova segmento che contiene il punto (considera larghezza)
;;; Argomenti: pt ent ; Ritorna: (v1 v2) o nil
;;; Esempio di utilizzo: (get-poly-segment-at-point '(10 20) ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-poly-segment-at-point
       (pt ent / verts width offset seg result)
  ;; ------------------------------------------------------------
  ;; Restituisce il segmento (v1 v2) di una LWPOLYLINE che
  ;; intercetta il punto pt (con tolleranza legata alla larghezza)
  ;;
  ;; pt   : punto di input (verrà snappato su NEA)
  ;; ent  : entità LWPOLYLINE
  ;; Ritorna: (v1 v2) oppure nil
  ;; ------------------------------------------------------------

  ;; Verifica tipo entità
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))

    (progn
      ;; Snap del punto sulla polilinea
      (setq pt (osnap pt "_NEA"))

      ;; Lista vertici
      (setq verts (get-poly-vertices ent))

      ;; Larghezza globale (se assente ? 0.0)
      (setq width (cdr (assoc 43 (entget ent))))
      (if (not width) (setq width 0.0))

      ;; Offset di ricerca:
      ;; - minimo 0.1
      ;; - aumentato se la polilinea ha larghezza
      (setq offset (+ 0.1 (if (> width 0.0) 0.1 0.0)))

      ;; Ricerca del segmento:
      ;; si scorre coppia (v1 v2) e si esce al primo match
      (while (and verts (cdr verts) (not result))
        (setq seg (list (car verts) (cadr verts)))

        ;; Test di intersezione tramite retta perpendicolare
        (if
          (inters
            (car seg)
            (cadr seg)
            (polar pt (+ (angle (car seg) (cadr seg)) (/ pi -2)) offset)
            (polar pt (+ (angle (car seg) (cadr seg)) (/ pi  2)) offset)
            nil
          )
          (setq result seg)
        )

        ;; Avanza al segmento successivo
        (setq verts (cdr verts))
      )

      result
    )

    nil
  )
)

;;;=============================================================================
;;; 6 MISURE E CALCOLI
;;;=============================================================================
;;; CALCULATE-DISTANCE-FROM-START - distanza lungo polilinea dal primo vertice fino a pt
;;; Argomenti: pt ent ; Ritorna: reale o nil
;;; Esempio di utilizzo: (calculate-distance-from-start '(10 20) ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun calculate-distance-from-start (pt ent / vertex-list pos total vertex1 vertex2) 
  (if 
    (and pt 
         ent
         (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
         (setq pt (osnap pt "_NEA"))
    )
    (progn 
      (setq vertex-list (get-poly-vertices ent)
            pos         0
            total       0.0
      )
      (while (and (< pos (1- (length vertex-list))) total) 
        (setq vertex1 (nth pos vertex-list)
              vertex2 (nth (1+ pos) vertex-list)
        )
        (if (or (equal vertex1 pt 1e-6) (inters vertex1 vertex2 pt pt nil)) 
          (progn 
            (setq total (+ total (distance vertex1 pt)))
            (setq pos (length vertex-list))
          )
          (progn 
            (setq total (+ total (distance vertex1 vertex2)))
            (setq pos (1+ pos))
          )
        )
      )
      total
    )
    nil
  )
)
;;; GET-POLYLINE-LENGTH - lunghezza totale
;;; Argomenti: ent ; Ritorna: reale
;;; Esempio di utilizzo: (get-polyline-length ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-polyline-length (ent / verts total) 
  (setq verts (get-poly-vertices ent)
        total 0.0
  )
  (while (and verts (cdr verts)) 
    (setq total (+ total (distance (car verts) (cadr verts))))
    (setq verts (cdr verts))
  )
  total
)
;;; GET-LWP-ANGLE - angolo del segmento in corrispondenza del punto
;;; Argomenti: pt ent ; Ritorna: angolo o nil
;;; Esempio di utilizzo: (get-lwp-angle '(10 20) ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun get-lwp-angle (pt ent / seg) 
  (setq seg (get-poly-segment-at-point pt ent))
  (if seg 
    (angle (car seg) (cadr seg))
    nil
  )
)
;;;=============================================================================
;;; 7 CREAZIONE E VISUALIZZAZIONE
;;;=============================================================================
;;; CREATE-LWPOLYLINE-FROM-POINTS - crea LWPOLYLINE da lista punti
;;; Argomenti: points layer color closed ; Ritorna: ent o nil
;;; Esempio di utilizzo: (create-lwpolyline-from-points '((0 0) (10 10) (20 0)) "0" 1 T)
;;; Nota: points = ((x1 y1) (x2 y2) ...)
(defun create-lwpolyline-from-points (points layer color closed / lwpoly-data flag)
  (if points
    (progn
      (setq flag
            (if (= closed 1) 1 0) ; FIX CRITICO
            lwpoly-data
            (list
              '(0 . "LWPOLYLINE")
              '(100 . "AcDbEntity")
              '(100 . "AcDbPolyline")
              (cons 90 (length points))
              (cons 70 flag)
            )
      )
      (if layer (setq lwpoly-data (append lwpoly-data (list (cons 8 layer)))))
      (if color (setq lwpoly-data (append lwpoly-data (list (cons 62 color)))))
      (foreach pt points
        (setq lwpoly-data (append lwpoly-data (list (cons 10 pt))))
      )
      (entmake lwpoly-data)
      (entlast)
    )
    nil
  )
)

;;; DRAW-LWPOLYLINE - comando PLINE interattivo per disegno da lista punti
;;; Argomenti: vertex-list ; Ritorna: nil
;;; Esempio di utilizzo: (draw-lwpolyline '((0 0) (10 10) (20 0)))
;;; Nota: vertex-list = ((x1 y1) (x2 y2) ...)
(defun draw-lwpolyline (vertex-list / old-osmode) 
  (setq old-osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "_pline")
  (foreach vertex vertex-list 
    (command vertex)
  )
  (command "")
  (setvar "OSMODE" old-osmode)
  (princ)
)
;;; NUMBER-VERTICES - crea testi numerati ai vertici (usa modify-entity se presente)
;;; Argomenti: entity, color, height ; Ritorna: lista handle testi
;;; Esempio di utilizzo: (number-vertices ent color height)
;;; Nota: funziona solo con LWPOLYLINE
(defun number-vertices (entity color height / vertex-list pos text-handles data)
  (if (= (cdr (assoc 0 (entget entity))) "LWPOLYLINE")
    (progn
      (setq vertex-list
	     (get-poly-vertices entity)
	    pos	0
	    text-handles
	     '()
      )
      (foreach vertex vertex-list
	(command ".text" vertex "" 0 (itoa pos))
	(set-entity-attribute (entlast) 40 height)
	(if (= (get-entity-attribute entity 62) color)
	  (set-entity-attribute (entlast) 62 5)
	  (set-entity-attribute (entlast) 62 color)
	)
	(setq text-handles
	       (cons (cdr (assoc 5 (entget (entlast))))
		     text-handles
	       )
	)
	(setq pos (1+ pos))
      )
      (reverse text-handles)
    )
  )
)
;;; ZOOM-LWPOLYLINE - zoom su lista punti (usa zoom-to-points se disponibile)
;;; Argomenti: ent ; Ritorna: nil
;;; Esempio di utilizzo: (zoom-lwpolyline ent)
;;; Nota: funziona solo con LWPOLYLINE
(defun zoom-lwpolyline (ent / vertex-list) 
  (setq vertex-list (get-poly-vertices ent))
  (if vertex-list 
    (if (boundp 'zoom-to-points)  
      (zoom-to-points vertex-list)
      (progn 
        (command "_.ZOOM" 
                 "_W"
                 (apply 'min (mapcar 'car vertex-list))
                 (apply 'max (mapcar 'car vertex-list))
        )
      )
    )
  )
  (princ)
)
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================