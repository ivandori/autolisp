;;;==============================================================================
;;; GESTIONE ENTITA' AUTOCAD
;;; Ultimo aggiornamento: 01/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: [MIT License](https://opensource.org/licenses/MIT)
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per manipolare, modificare e gestire entità di disegno
;;; Dipendenze esterne:  CALC-MIDPOINT (_punti.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. LETTURA ATTRIBUTI ENTITA'
;;; 2. MODIFICA ATTRIBUTI ENTITA'
;;; 3. GESTIONE BLOCCHI RICORSIVA
;;; 4. BOUNDING BOX (senza VLA)
;;; 5. TRASFORMAZIONI GEOMETRICHE
;;; 6. GESTIONE HANDLE
;;; 7. OPERAZIONI SPECIALI
;;;=============================================================================
;;; 1. LETTURA ATTRIBUTI ENTITA'
;;;=============================================================================
;;; GET-ENTITY-ATTRIBUTE
;;; Ottiene valore attributo di un'entità
;;; Argomenti:
;;;   entity - Nome entità
;;;   attrib-code - Codice attributo DXF
;;; Ritorna: Valore attributo o nil
;;; Esempi:
;;;   (get-entity-attribute ent 0)  => Tipo entità ("LINE", "CIRCLE")
;;;   (get-entity-attribute ent 8)  => Layer
;;;   (get-entity-attribute ent 62) => Colore
(defun get-entity-attribute (entity attrib-code / elist)
  (if entity
    (progn
      (setq elist (entget entity))
      (cdr (assoc attrib-code elist)))
    nil))
;;; GET-ENTITY-ATTRIBUTES-LIST
;;; Ottiene lista di tutti i valori per attributo ripetuto
;;; Argomenti:
;;;   entity - Nome entità
;;;   attrib-code - Codice attributo
;;; Ritorna: Lista di valori (utile per LWPOLYLINE con vertici multipli)
;;; Esempio:
;;;   (get-entity-attributes-list ent 10) => Lista di punti vertici
(defun get-entity-attributes-list (entity attrib-code 
                                   / elist value values piece)
  (if entity
    (progn
      (setq elist (entget entity)
            piece (assoc attrib-code elist)
            value (cdr piece))
      (if value
        (progn
          (setq values (list value))
          (while (setq elist (cdr (member piece elist)))
            (setq piece (assoc attrib-code elist)
                  value (cdr piece))
            (if value
              (setq values (append values (list value)))))))
      values)
    nil))
;;; GET-ENTITY-HANDLE
;;; Ottiene handle di un'entità
;;; Argomenti:
;;;   entity - Nome entità
;;; Ritorna: Handle come stringa
(defun get-entity-handle (entity)
  (cdr (assoc 5 (entget entity))))
;;; GET-ENTITY-NAME
;;; Ottiene nome di un'entità (per blocchi)
;;; Argomenti:
;;;   entity - Nome entità
;;; Ritorna: Nome blocco o nil
(defun get-entity-name (entity)
  (cdr (assoc 2 (entget entity))))
;;; GET-ENTITY-TYPE
;;; Ottiene tipo di un'entità
;;; Argomenti:
;;;   entity - Nome entità
;;; Ritorna: Tipo entità ("LINE", "CIRCLE", "INSERT", ecc.)
(defun get-entity-type (entity)
  (cdr (assoc 0 (entget entity))))
;;; GET_ENTITY_WIDTH
;;; Returns constant width of polyline (DXF group 43)
;;; ============================================================
(DEFUN GET_ENTITY_WIDTH (ent)
  (CDR (ASSOC 43 (ENTGET ent)))
)=============================================================================
;;; 2. MODIFICA ATTRIBUTI ENTITA'
;;;=============================================================================
;;; SET-ENTITY-ATTRIBUTE
;;; Modifica valore attributo di un'entità
;;; Argomenti:
;;;   entity - Nome entità
;;;   attrib-code - Codice attributo DXF
;;;   value - Nuovo valore
;;; Ritorna: T se successo
;;; Esempi:
;;;   (set-entity-attribute ent 8 "NUOVO_LAYER")
;;;   (set-entity-attribute ent 62 1)  ; Rosso
(defun set-entity-attribute (entity attrib-code value / new-pair elist old-pair)
  (setq new-pair (cons attrib-code value))
  (setq elist (entget entity))
  (if (setq old-pair (assoc attrib-code elist))
    (setq elist (subst new-pair old-pair elist))
    (setq elist (reverse (cons new-pair (reverse elist)))))
  (entmod elist)
  (entupd (cdr (assoc -1 elist)))
  T)
;;; REMOVE-ENTITY-ATTRIBUTE
;;; Rimuove un attributo da un'entità
;;; Argomenti:
;;;   entity - Nome entità
;;;   attrib-code - Codice attributo da rimuovere
;;; Ritorna: T se successo
;;; Esempio:
;;;   (remove-entity-attribute ent 62) ; Rimuove colore
(defun remove-entity-attribute (entity attrib-code / elist old-pair result)
  (setq elist (entget entity))
  (if (setq old-pair (assoc attrib-code elist))
    (progn
      (setq result nil)
      (foreach item elist
        (if (not (equal item old-pair))
          (setq result (append result (list item)))))
      (entmod result)
      (entupd (cdr (assoc -1 result)))
      T)
    nil))
;;; SET-ENTITY-TO-BYLAYER
;;; Imposta entità su layer 0 con colore BYBLOCK
;;; Argomenti:
;;;   entity - Nome entità
;;; Ritorna: T
(defun set-entity-to-bylayer (entity)
  (set-entity-attribute entity 8 "0")
  (set-entity-attribute entity 62 0)
  T)
;;;=============================================================================
;;; 3. GESTIONE BLOCCHI RICORSIVA
;;;=============================================================================
;;; MODIFY-BLOCK-RECURSIVE
;;; Modifica attributo di tutte le entità in un blocco (ricorsivo)
;;; Argomenti:
;;;   entity - Entità blocco
;;;   attrib-code - Codice attributo
;;;   value - Nuovo valore
;;; Nota: Gestisce blocchi annidati e attributi
;;; Esempio:
;;;   (modify-block-recursive ent 62 3) ; Imposta colore rosso
(defun modify-block-recursive (entity attrib-code value 
                               / edata ent2 ent3 ent-att)
  (if entity
    (progn
      (setq edata (entget entity))
      (cond
        ((= (cdr (assoc 0 edata)) "INSERT")
         (setq ent-att entity)
         (while (= (get-entity-attribute 
                     (setq ent-att (entnext ent-att)) 0) "ATTRIB")
           (set-entity-attribute ent-att attrib-code value))
         (setq ent2 (cdr (assoc -2 
                          (cdr (cdr (cdr (cdr 
                            (tblsearch "BLOCK" 
                              (cdr (assoc 2 edata)))))))))))
        ((= (cdr (assoc 0 edata)) "DIMENSION")
         (setq ent2 (cdr (assoc -2 
                          (tblsearch "BLOCK" 
                            (cdr (assoc 2 edata))))))))
      (if (or (= (cdr (assoc 0 edata)) "INSERT")
              (= (cdr (assoc 0 edata)) "DIMENSION"))
        (progn
          (setq ent3 ent2)
          (while (and ent3 
                     (/= (get-entity-attribute ent3 0) "SEQEND"))
            (modify-block-recursive ent3 attrib-code value)
            (setq ent3 (entnext ent3)))))
      (set-entity-attribute entity attrib-code value)
      (entupd entity))))
;;; MODIFY-ALL-BLOCKS-BY-NAME
;;; Modifica attributo di tutti i blocchi con nome specifico
;;; Argomenti:
;;;   block-name - Nome blocco da cercare
;;;   attrib-code - Codice attributo
;;;   value - Nuovo valore
;;; Ritorna: Numero blocchi modificati
;;; Esempio:
;;;   (modify-all-blocks-by-name "MIO_BLOCCO" 62 5) ; Imposta colore blu
(defun modify-all-blocks-by-name (block-name attrib-code value 
                                  / ss idx count ent)
  (setq count 0)
  (if (setq ss (ssget "X" (list (cons 2 block-name))))
    (progn
      (setq idx 0)
      (while (setq ent (ssname ss idx))
        (modify-block-recursive ent attrib-code value)
        (setq count (1+ count))
        (setq idx (1+ idx)))
      (command "_.regen")))
  count)
;;;=============================================================================
;;; 4. BOUNDING BOX (SENZA VLA - Compatibile DraftSight)
;;;=============================================================================
;;; GET-ENTITY-BOUNDING-BOX
;;; Calcola bounding box di un'entità (senza VLA)
;;; Argomenti:
;;;   entity - Nome entità
;;; Ritorna: Lista (punto-min punto-max) o nil
;;; Nota: Funziona con LINE, CIRCLE, LWPOLYLINE, ARC, INSERT
(defun get-entity-bounding-box (entity / etype pts min-pt max-pt)
  (setq etype (get-entity-type entity))
  (cond
    ((= etype "LINE")
     (setq pts (list (cdr (assoc 10 (entget entity)))
                     (cdr (assoc 11 (entget entity))))))
    ((= etype "CIRCLE")
     (setq center (cdr (assoc 10 (entget entity)))
           radius (cdr (assoc 40 (entget entity)))
           pts (list (list (- (car center) radius) 
                          (- (cadr center) radius))
                     (list (+ (car center) radius) 
                          (+ (cadr center) radius)))))
    ((= etype "LWPOLYLINE")
     (setq pts (get-entity-attributes-list entity 10)))
    ((= etype "INSERT")
     (setq pts (list (cdr (assoc 10 (entget entity)))))))
  (if pts
    (progn
      (setq min-pt (list 1e99 1e99)
            max-pt (list -1e99 -1e99))
      (foreach pt pts
        (if (< (car pt) (car min-pt))
          (setq min-pt (list (car pt) (cadr min-pt))))
        (if (< (cadr pt) (cadr min-pt))
          (setq min-pt (list (car min-pt) (cadr pt))))
        (if (> (car pt) (car max-pt))
          (setq max-pt (list (car pt) (cadr max-pt))))
        (if (> (cadr pt) (cadr max-pt))
          (setq max-pt (list (car max-pt) (cadr pt)))))
      (list min-pt max-pt))
    nil))
;;; ZOOM-TO-ENTITY
;;; Zooma su un'entità
;;; Argomenti:
;;;   entity - Nome entità
;;; Ritorna: T se successo
(defun zoom-to-entity (entity / bbox zmin zmax)
  (if (setq bbox (get-entity-bounding-box entity))
    (progn
      (setq zmin (car bbox)
            zmax (cadr bbox))
      (command "_.zoom" "_w" zmin zmax)
      T)
    nil))
;;;=============================================================================
;;; 5. TRASFORMAZIONI GEOMETRICHE
;;;=============================================================================
;;; MOVE-ENTITY-TO-POINT
;;; Muove entità posizionando angolo bounding box in punto
;;; Argomenti:
;;;   entity - Nome entità
;;;   target-pt - Punto destinazione
;;; Ritorna: T se successo
;;; Esempio:
;;;   (move-entity-to-point ent '(100 100)) ; Sposta entità
(defun move-entity-to-point (entity target-pt / bbox min-pt)
  (if (and entity target-pt)
    (progn
      (if (setq bbox (get-entity-bounding-box entity))
        (progn
          (setq min-pt (car bbox))
          (command "_.move" entity "" min-pt target-pt)
          T)
        nil))
    nil))
;;; SCALE-ENTITY-TO-BOX
;;; Scala entità per adattarla a dimensioni specifiche
;;; Argomenti:
;;;   entity - Nome entità
;;;   target-pt - Punto target per angolo superiore destro
;;; Ritorna: T se successo
;;; Esempio:
;;;   (scale-entity-to-box ent '(200 150)) ; Scala entità
(defun scale-entity-to-box (entity target-pt 
                           / bbox min-pt max-pt x1 y1 x2 y2 x3 y3 
                           dx12 dy12 dx13 dy13 scale-x scale-y scale-factor)
  (if (and entity target-pt)
    (progn
      (if (setq bbox (get-entity-bounding-box entity))
        (progn
          (setq min-pt (car bbox)
                max-pt (cadr bbox)
                x1 (car min-pt)
                y1 (cadr min-pt)
                x2 (car max-pt)
                y2 (cadr max-pt)
                x3 (car target-pt)
                y3 (cadr target-pt)
                dx12 (abs (- x1 x2))
                dy12 (abs (- y1 y2))
                dx13 (abs (- x1 x3))
                dy13 (abs (- y1 y3)))
          (if (and (> dx12 0) (> dy12 0))
            (progn
              (setq scale-x (/ dx13 dx12)
                    scale-y (/ dy13 dy12)
                    scale-factor (min scale-x scale-y))
              (if (> scale-factor 0)
                (progn
                  (command "_.scale" entity "" min-pt scale-factor)
                  T))))))
    nil)))
;;; MOVE-AND-SCALE-ENTITY
;;; Muove e scala entità in un'operazione
;;; Argomenti:
;;;   entity - Nome entità
;;;   move-pt - Punto per spostamento
;;;   scale-pt - Punto per scala
;;; Ritorna: T se successo
(defun move-and-scale-entity (entity move-pt scale-pt)
  (if (and entity move-pt scale-pt)
    (progn
      (move-entity-to-point entity move-pt)
      (scale-entity-to-box entity scale-pt)
      T)
    nil))
;;; CENTER-ENTITY-BETWEEN-POINTS
;;; Centra entità tra due punti
;;; Argomenti:
;;;   entity - Nome entità
;;;   pt1 - Primo punto
;;;   pt2 - Secondo punto
;;; Ritorna: T se successo
(defun center-entity-between-points (entity pt1 pt2 
                                    / bbox min-pt max-pt center-pt mid-pt)
  (if (and entity pt1 pt2)
    (progn
      (if (setq bbox (get-entity-bounding-box entity))
        (progn
          (setq min-pt (car bbox)
                max-pt (cadr bbox)
                center-pt (calc-midpoint min-pt max-pt)
                mid-pt (calc-midpoint pt1 pt2))
          (command "_.move" entity "" center-pt mid-pt)
          T))
    nil)))
;;;=============================================================================
;;; 6. GESTIONE HANDLE
;;;=============================================================================
;;; HANDLE-EXISTS
;;; Verifica se un handle esiste e l'entità è valida
;;; Argomenti:
;;;   handle - Handle come stringa
;;; Ritorna: T se esiste, nil altrimenti
;;; Esempio:
;;;   (handle-exists "1A2B") => T o nil
(defun handle-exists (handle)
  (cond
    ((null handle) nil)
    ((= handle "") nil)
    ((not (= (type handle) 'str)) nil)
    ((null (handent handle)) nil)
    ((and (handent handle) (null (entget (handent handle)))) nil)
    ((and (handent handle) (entget (handent handle))) T)
    (t nil)))
;;; DELETE-BY-HANDLE
;;; Elimina entità tramite handle
;;; Argomenti:
;;;   handle - Handle entità da eliminare
;;; Ritorna: T se successo
;;; Esempio:
;;;   (delete-by-handle "1A2B") => T o nil
(defun delete-by-handle (handle)
  (if (handle-exists handle)
    (progn
      (command "_.erase" (handent handle) "")
      T)
    nil))
;;; GET-ENTITY-BY-HANDLE
;;; Ottiene entità da handle
;;; Argomenti:
;;;   handle - Handle come stringa
;;; Ritorna: Nome entità o nil
;;; Esempio:
;;;   (get-entity-by-handle "1A2B") => Nome entità
(defun get-entity-by-handle (handle)
  (if (handle-exists handle)
    (handent handle)
    nil))
;;;=============================================================================
;;; 7. OPERAZIONI SPECIALI
;;;=============================================================================
;;; FLIP-ENTITY
;;; Ribalta entità di 180 gradi
;;; Argomenti:
;;;   entity - Nome entità
;;; Ritorna: T se successo
(defun flip-entity (entity / ang)
  (if (setq ang (get-entity-attribute entity 50))
    (progn
      (set-entity-attribute entity 50 (+ ang pi))
      T)
    (progn
      (princ "\nEntita' non valida per ribaltamento!")
      nil)))
;;; MIRROR-ENTITY
;;; Specchia entità rispetto al suo punto di inserimento
;;; Argomenti:
;;;   entity - Nome entità
;;;   insert-pt - Punto inserimento (da entsel)
;;; Ritorna: T se successo
;;; Esempio:
;;;   (mirror-entity ent insert-pt) ; Specchia entità
(defun mirror-entity (entity insert-pt / ang)
  (if (setq ang (get-entity-attribute entity 50))
    (progn
      (command "_.mirror" insert-pt "" insert-pt 
               (polar insert-pt ang 10) "_Y")
      T)
    (progn
      (princ "\nEntita' non valida per specchiatura!")
      nil)))
;;; DELETE-ENTITY-SAFE
;;; Elimina entità senza possibilità di ripristino
;;; Argomenti:
;;;   entity - Nome entità
;;; Ritorna: T
(defun delete-entity-safe (entity)
  (command "_.erase" entity "")
  T)
;;;
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================