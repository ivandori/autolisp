;;; Demo per funzioni in _polilinee.lsp
;;; Uso: caricare il file e chiamare le funzioni demo-*
;;; Compatible: ProgeCAD / AutoLISP puro (senza VL)

(defun demo--get-entity (prompt / sel)
  (setq sel (entsel prompt))
  (if sel (car sel) nil)
)

(defun demo-get-poly-vertices (/ ent)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent (princ (get-poly-vertices ent)))
  (princ)
)

(defun demo-get-lwpolyline-vertices (/ ent)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent (princ (get-lwpolyline-vertices ent)))
  (princ)
)

(defun demo-get-numbered-lwpolyline-vertices (/ ent)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent (princ (get-numbered-lwpolyline-vertices ent)))
  (princ)
)

(defun demo-get-vertex-by-index (/ ent idx)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq idx (getint "\nIndice (0-based): "))
      (princ (get-vertex-by-index ent idx))
    )
  )
  (princ)
)

(defun demo-get-vertex-count (/ ent cnt)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq cnt (get-vertex-count ent))
      (princ "\nVertici: ")
      (princ cnt)
    )
  )
  (princ)
)

(defun demo-get-first-last-vertices (/ ent first last)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq first (get-first-vertex ent))
      (setq lastvx (get-last-vertex ent))
      (princ "\nFirst: ") (princ first)
      (princ "  Last: ") (princ last)
    )
  )
  (princ)
)

(defun demo-is-polyline-closed (/ ent closed)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq closed (is-polyline-closed ent))
      (princ "\nChiusa: ") (princ closed)
    )
  )
  (princ)
)

(defun demo-get-polyline-properties (/ ent props)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq props (get-polyline-properties ent))
      (princ "\nProprietÃ : ") (princ props)
    )
  )
  (princ)
)

(defun demo-find-vertex-position (/ v lst pos)
  ;; Punto da cercare
  (setq v (getpoint "\nPunto da cercare: "))
  (cond
    ((null v)
     (princ "\nOperazione annullata: punto non selezionato.")
    )
    (t
     ;; Inserimento lista vertici
     (princ "\nInserisci lista vertici (es. ((0 0) (10 10))): ")
       (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
       (if ent (setq lst (get-lwpolyline-vertices ent)))

     ;; Controlli sulla lista
     (cond
       ((null lst)
        (princ "\nErrore: nessuna lista inserita.")
       )
       ((not (listp lst))
        (princ "\nErrore: input non è una lista.")
       )
      
       (t
        ;; Ricerca posizione
        (setq pos (find-vertex-position v lst))
        (if (and pos (> pos -1))
          (progn
            (princ "\nIndice: ")
            (princ pos)
          )
          (princ "\nPunto non trovato nella lista.")
        )
       )
     )
    )
  )
  (princ)
)


(defun demo-find-vertex-after-point (/ pt ent seg)
  (setq pt (getpoint "\nPunto: "))
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq seg (find-vertex-after-point pt ent))
      (princ "\nSegmento: ") (princ seg)
    )
  )
  (princ)
)

(defun demo-find-segment-at-point (/ pt ent seg)
  (setq pt (getpoint "\nPunto: "))
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq seg (find-segment-at-point pt ent))
      (princ "\nSegmento: ") (princ seg)
    )
  )
  (princ)
)

(defun demo-insert-vertex-after-position (/ ent pt idx res)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq pt (getpoint "\nNuovo punto: "))
      (setq idx (getint "\nIndice precedente (0-based): "))
      (setq res (insert-vertex-after-position pt ent idx))
      (princ "\nRisultato: ") (princ res)
    )
  )
  (princ)
)

(defun demo-insert-vertex-after-position-v2 (/ ent pt idx res)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq pt (getpoint "\nNuovo punto: "))
      (setq idx (getint "\nIndice precedente (0-based): "))
      (setq res (insert-vertex-after-position-v2 pt ent idx))
      (princ "\nRisultato: ") (princ res)
    )
  )
  (princ)
)

(defun demo-insert-vertex-at-point (/ ent pt res)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq pt (getpoint "\nPunto da inserire: "))
      (setq res (insert-vertex-at-point ent pt ))
      (princ "\nRisultato: ") (princ res)
    )
  )
  (princ)
)

(defun demo-append-polyline-vertex (/ ent pt res)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq pt (getpoint "\nPunto da aggiungere: "))
      (setq res (append-polyline-vertex ent pt))
      (princ "\nRisultato: ") (princ res)
    )
  )
  (princ)
)

(defun demo-append-or-prepend-polyline-vertex (/ ent pt res)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq pt (getpoint "\nPunto da aggiungere: "))
      (setq res (append-or-prepend-polyline-vertex ent pt))
      (princ "\nRisultato: ") (princ res)
    )
  )
  (princ)
)

(defun demo-add-polyline-vertex (/ ptsel ptNew res)
  (setq ptsel (getpoint "\nPunto vicino polilinea: "))
  (setq ptNew (getpoint "\nNuovo punto: "))
  (setq res (add-polyline-vertex ptsel ptNew))
  (princ "\nRisultato: ") (princ res)
  (princ)
)

(defun demo-update-lwp-vertex (/ ent old new res)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq old (getpoint "\nVertice da sostituire: "))
      (setq new (getpoint "\nNuovo vertice: "))
      (setq res (update-lwp-vertex ent old new))
      (princ "\nRisultato: ") (princ res)
    )
  )
  (princ)
)

(defun demo-remove-polyline-vertex (/ ptsel res)
  (setq ptsel (getpoint "\nClick vicino al vertice: "))
  (setq res (remove-polyline-vertex ptsel))
  (princ "\nRisultato: ") (princ res)
  (princ)
)

(defun demo-move-lwp-vertex (/ ptsel res)
  (setq ptsel (getpoint "\nClick vicino al vertice: "))
  (setq res (move-lwp-vertex ptsel))
  (princ "\nRisultato: ") (princ res)
  (princ)
)

(defun demo-distance-to-segment (/ pt p1 p2 dist)
  (setq pt (getpoint "\nPunto: "))
  (setq p1 (getpoint "\nSegmento p1: "))
  (setq p2 (getpoint "\nSegmento p2: "))
  (setq dist (distance-to-segment pt (list p1 p2)))
  (princ "\nDistanza: ") (princ dist)
  (princ)
)

(defun demo-find-nearest-segment (/ pt ptv lst res)
  ;; Punto di riferimento
  (setq pt (getpoint "\nPunto: "))
  (cond
    ((null pt)
     (princ "\nOperazione annullata: punto non selezionato.")
    )
    (t
     ;; Inserimento vertici
     (princ "\nInserisci lista vertici (INVIO per terminare): ")
     (while (setq ptv (getpoint))
       (setq lst (cons ptv lst))
     )

     ;; Controllo lista vertici
     (cond
       ((or (null lst) (< (length lst) 2))
        (princ "\nErrore: servono almeno due vertici per definire un segmento.")
       )
       
       (t
        ;; Calcolo segmento più vicino
        (setq res (find-nearest-segment pt lst))
        (if res
          (progn
            (princ "\nSegmento più vicino: ")
            (princ res)
          )
          (princ "\nErrore: nessun segmento valido trovato.")
        )
       )
     )
    )
  )
  (princ)
)



(defun demo-get-poly-segment-near-point (/ ent pt res)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq pt (getpoint "\nPunto: "))
      (setq res (get-poly-segment-near-point pt ent))
      (princ "\nSegmento più vicino: ") (princ res)
    )
    (princ "\nNessuna entità selezionata")
  )
  (princ)
)

(defun demo-get-poly-segment-at-point (/ ent pt res)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq pt (getpoint "\nPunto: "))
      (setq res (get-poly-segment-at-point pt ent))
      (princ "\nSegmento: ") (princ res)
    )
  )
  (princ)
)

(defun demo-calculate-distance-from-start (/ ent pt dist)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq pt (getpoint "\nPunto sulla polilinea: "))
      (setq dist (calculate-distance-from-start pt ent))
      (princ "\nDistanza da inizio: ") (princ dist)
    )
  )
  (princ)
)

(defun demo-get-polyline-length (/ ent len)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq len (get-polyline-length ent))
      (princ "\nLunghezza: ") (princ len)
    )
  )
  (princ)
)

(defun demo-get-lwp-angle (/ ent pt ang)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq pt (getpoint "\nPunto sul segmento: "))
      (setq ang (get-lwp-angle pt ent))
      (princ "\nAngolo: ") (princ ang)
    )
  )
  (princ)
)

(defun demo-create-lwpolyline-from-points (/ ent)
  (setq ent (create-lwpolyline-from-points '((0 0) (10 0) (10 10) (0 10)) "0" 1 T))
  (princ "\nPolilinea creata: ") (princ ent)
  (princ)
)

(defun demo-draw-lwpolyline (/)
  (draw-lwpolyline '((0 0) (5 5) (10 0)))
  (princ "\nPolilinea disegnata")
  (princ)
)

(defun demo-number-vertices (/ ent res)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent
    (progn
      (setq res (number-vertices ent 3 2.5))
      (princ "\nVertici numerati: ") (princ res)
    )
  )
  (princ)
)

(defun demo-zoom-lwpolyline (/ ent)
  (setq ent (demo--get-entity "\nSeleziona LWPOLYLINE: "))
  (if ent (zoom-lwpolyline ent))
  (princ "\nZoom eseguito")
  (princ)
)

;;; Comando principale che esegue tutti i demo
(defun c:demopoly (/ demos idx fn-name)
  (setq demos (list
    "demo-get-poly-vertices" "demo-get-lwpolyline-vertices" 
    "demo-get-numbered-lwpolyline-vertices" "demo-get-vertex-by-index" 
    "demo-get-vertex-count" "demo-get-first-last-vertices" "demo-is-polyline-closed"
    "demo-get-polyline-properties" "demo-find-vertex-position" "demo-find-segment-after-point" 
    "demo-find-segment-at-point" "demo-insert-vertex-after-position" "demo-insert-vertex-after-position-v2" 
    "demo-insert-vertex-at-point" "demo-append-polyline-vertex" "demo-add-polyline-vertex" 
    "demo-update-lwp-vertex"  "demo-remove-polyline-vertex" 
    "demo-move-lwp-vertex" "demo-distance-to-segment" "demo-find-nearest-segment"
    "demo-get-poly-segment-near-point" "demo-get-poly-segment-at-point" "demo-calculate-distance-from-start"
    "demo-get-polyline-length" "demo-get-lwp-angle" "demo-create-lwpolyline-from-points" 
    "demo-draw-lwpolyline" "demo-number-vertices" "demo-zoom-lwpolyline"
  ))
  (princ "\n================== DEMO POLILINEE ==================")
  (setq idx 0)
  (while (< idx (length demos))
    (setq fn-name (nth idx demos))
    (princ (strcat "\n[" (itoa (+ idx 1)) "/" (itoa (length demos)) "] -> " fn-name))
    (cond
      ((= fn-name "demo-get-poly-vertices") (demo-get-poly-vertices))
      ((= fn-name "demo-get-lwpolyline-vertices") (demo-get-lwpolyline-vertices))
      ((= fn-name "demo-get-numbered-lwpolyline-vertices") (demo-get-numbered-lwpolyline-vertices))
      ((= fn-name "demo-get-vertex-by-index") (demo-get-vertex-by-index))
      ((= fn-name "demo-get-vertex-count") (demo-get-vertex-count))
      ((= fn-name "demo-get-first-last-vertices") (demo-get-first-last-vertices))
      ((= fn-name "demo-is-polyline-closed") (demo-is-polyline-closed))
      ((= fn-name "demo-get-polyline-properties") (demo-get-polyline-properties))
      ((= fn-name "demo-find-vertex-position") (demo-find-vertex-position))
      ((= fn-name "demo-find-vertex-after-point") (demo-find-vertex-after-point))
      ((= fn-name "demo-find-segment-at-point") (demo-find-segment-at-point))
      ((= fn-name "demo-insert-vertex-after-position") (demo-insert-vertex-after-position))
      ((= fn-name "demo-insert-vertex-after-position-v2") (demo-insert-vertex-after-position-v2))
      ((= fn-name "demo-insert-vertex-at-point") (demo-insert-vertex-at-point))
      ((= fn-name "demo-append-polyline-vertex") (demo-append-polyline-vertex))
      ((= fn-name "demo-append-or-prepend-polyline-vertex") (demo-append-or-prepend-polyline-vertex))
      ((= fn-name "demo-add-polyline-vertex") (demo-add-polyline-vertex))
      ((= fn-name "demo-update-lwp-vertex") (demo-update-lwp-vertex))
      ((= fn-name "demo-remove-polyline-vertex") (demo-remove-polyline-vertex))
      ((= fn-name "demo-move-lwp-vertex") (demo-move-lwp-vertex))
      ((= fn-name "demo-distance-to-segment") (demo-distance-to-segment))
      ((= fn-name "demo-find-nearest-segment") (demo-find-nearest-segment))
      ((= fn-name "demo-get-poly-segment-near-point") (demo-get-poly-segment-near-point))
      ((= fn-name "demo-get-poly-segment-at-point") (demo-get-poly-segment-at-point))
      ((= fn-name "demo-calculate-distance-from-start") (demo-calculate-distance-from-start))
      ((= fn-name "demo-get-polyline-length") (demo-get-polyline-length))
      ((= fn-name "demo-get-lwp-angle") (demo-get-lwp-angle))
      ((= fn-name "demo-create-lwpolyline-from-points") (demo-create-lwpolyline-from-points))
      ((= fn-name "demo-draw-lwpolyline") (demo-draw-lwpolyline))
      ((= fn-name "demo-number-vertices") (demo-number-vertices))
      ((= fn-name "demo-zoom-lwpolyline") (demo-zoom-lwpolyline))
    )
    (setq idx (+ idx 1))
  )
  (princ "\n=================== DEMO COMPLETATI ===================")
  (princ)
)

;;; Fine file demo