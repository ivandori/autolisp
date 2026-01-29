;;;=============================================================================
;;; GESTIONE GRUPPI SELEZIONE (SELECTION SET)
;;; Ultimo aggiornamento: 12/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per operazioni sui gruppi di selezione
;;; Dipendenze esterne: CALC-MIDPOINT (_punti.lsp) GET-ENTITY-TYPE, SET-ENTITY-ATTRIBUTE, GET-ENTITY-ATTRIBUTE, GET-ENTITY-BOUNDING-BOX (_entita.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. MODIFICA ATTRIBUTI GRUPPI
;;; 2. RICERCA IN GRUPPI SELEZIONE
;;; 3. BOUNDING BOX GRUPPI (senza VLA)
;;; 4. ZOOM E VISUALIZZAZIONE
;;; 5. CONVERSIONE E UTILITA'
;;;=============================================================================
;;; 1. MODIFICA ATTRIBUTI GRUPPI
;;;=============================================================================
;;; MODIFY-SS-ATTRIBUTE
;;; Modifica attributo per tutte le entità in un selection set
;;; Argomenti:
;;;   ss - Selection set
;;;   attrib-code - Codice attributo DXF
;;;   value - Nuovo valore
;;;   recursive - T per modificare anche blocchi annidati
;;; Ritorna: Numero entità modificate
;;; Esempio:
;;;   (modify-ss-attribute ss 62 1 T) ; Imposta colore rosso a tutte le entità in ss
(defun modify-ss-attribute (ss attrib-code value recursive / idx count ent etype)
  (setq idx 0 count 0)
  (if ss
    (while (setq ent (ssname ss idx))
      (setq etype (get-entity-type ent))
      (if (or (= etype "INSERT") (= etype "DIMENSION"))
        (if recursive
          (modify-block-recursive ent attrib-code value)
          (set-entity-attribute ent attrib-code value))
        (set-entity-attribute ent attrib-code value))
      (setq count (1+ count))
      (setq idx (1+ idx))))
  count)
;;; SET-SS-TO-LAYER
;;; Sposta tutte le entità di un SS su un layer
;;; Argomenti:
;;;   ss - Selection set
;;;   layer-name - Nome layer destinazione
;;; Ritorna: Numero entità modificate
(defun set-ss-to-layer (ss layer-name)
  (modify-ss-attribute ss 8 layer-name T))
;;; SET-SS-COLOR
;;; Imposta colore per tutte le entità di un SS
;;; Argomenti:
;;;   ss - Selection set
;;;   color - Numero colore (0=BYBLOCK, 256=BYLAYER)
;;; Ritorna: Numero entità modificate
(defun set-ss-color (ss color)
  (modify-ss-attribute ss 62 color T))
;;; SET-SS-TO-BYLAYER
;;; Imposta tutte le entità su layer 0 e BYBLOCK
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Numero entità modificate
(defun set-ss-to-bylayer (ss / count)
  (setq count (set-ss-to-layer ss "0"))
  (set-ss-color ss 0)
  count)
;;;=============================================================================
;;; 2. RICERCA IN GRUPPI SELEZIONE
;;;=============================================================================
;;; FIND-RIGHTMOST-ENTITY
;;; Trova entità con punto inserzione più a destra
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Entità più a destra o nil
(defun find-rightmost-entity (ss / idx ent rightmost-ent pt x xmax)
  (setq idx 0)
  (if ss
    (while (setq ent (ssname ss idx))
      (if (null rightmost-ent)
        (setq rightmost-ent ent))
      (setq pt (get-entity-attribute ent 10)
            x (car pt))
      (if (null xmax)
        (setq xmax x))
      (if (> x xmax)
        (setq rightmost-ent ent
              xmax x))
      (setq idx (1+ idx))))
  rightmost-ent)
;;; FIND-LEFTMOST-ENTITY
;;; Trova entità con punto inserzione più a sinistra
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Entità più a sinistra o nil
(defun find-leftmost-entity (ss / idx ent leftmost-ent pt x xmin)
  (setq idx 0)
  (if ss
    (while (setq ent (ssname ss idx))
      (if (null leftmost-ent)
        (setq leftmost-ent ent))
      (setq pt (get-entity-attribute ent 10)
            x (car pt))
      (if (null xmin)
        (setq xmin x))
      (if (< x xmin)
        (setq leftmost-ent ent
              xmin x))
      (setq idx (1+ idx))))
  leftmost-ent)
;;; FIND-TOPMOST-ENTITY
;;; Trova entità con punto inserzione più in alto
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Entità più in alto o nil
(defun find-topmost-entity (ss / idx ent topmost-ent pt y ymax)
  (setq idx 0)
  (if ss
    (while (setq ent (ssname ss idx))
      (if (null topmost-ent)
        (setq topmost-ent ent))
      (setq pt (get-entity-attribute ent 10)
            y (cadr pt))
      (if (null ymax)
        (setq ymax y))
      (if (> y ymax)
        (setq topmost-ent ent
              ymax y))
      (setq idx (1+ idx))))
  topmost-ent)
;;; FIND-BOTTOMMOST-ENTITY
;;; Trova entità con punto inserzione più in basso
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Entità più in basso o nil
(defun find-bottommost-entity (ss / idx ent bottommost-ent pt y ymin)
  (setq idx 0)
  (if ss
    (while (setq ent (ssname ss idx))
      (if (null bottommost-ent)
        (setq bottommost-ent ent))
      (setq pt (get-entity-attribute ent 10)
            y (cadr pt))
      (if (null ymin)
        (setq ymin y))
      (if (< y ymin)
        (setq bottommost-ent ent
              ymin y))
      (setq idx (1+ idx))))
  bottommost-ent)
;;;=============================================================================
;;; 3. BOUNDING BOX GRUPPI (SENZA VLA)
;;;=============================================================================
;;; GET-SS-BOUNDING-BOX
;;; Calcola bounding box di un selection set (senza VLA)
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Lista (punto-min punto-max) o nil
(defun get-ss-bounding-box (ss / idx ent bbox ll-ss ur-ss ll-ent ur-ent)
  (setq idx 0)
  (if ss
    (progn
      (while (setq ent (ssname ss idx))
        (if (setq bbox (get-entity-bounding-box ent))
          (progn
            (setq ll-ent (car bbox)
                  ur-ent (cadr bbox))
            (if (not ll-ss)
              (setq ll-ss ll-ent
                    ur-ss ur-ent)
              (progn
                (if (> (car ur-ent) (car ur-ss))
                  (if (> (cadr ur-ent) (cadr ur-ss))
                    (setq ur-ss ur-ent)
                    (setq ur-ss (list (car ur-ent) (cadr ur-ss))))
                  (if (> (cadr ur-ent) (cadr ur-ss))
                    (setq ur-ss (list (car ur-ss) (cadr ur-ent)))))
                (if (< (car ll-ent) (car ll-ss))
                  (if (< (cadr ll-ent) (cadr ll-ss))
                    (setq ll-ss ll-ent)
                    (setq ll-ss (list (car ll-ent) (cadr ll-ss))))
                  (if (< (cadr ll-ent) (cadr ll-ss))
                    (setq ll-ss (list (car ll-ss) (cadr ll-ent)))))))))
        (setq idx (1+ idx)))
      (if (and ll-ss ur-ss)
        (list ll-ss ur-ss)
        nil))
    nil))
;;; GET-SS-CENTER
;;; Calcola punto centrale di un selection set
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Punto centrale (x y) o nil
(defun get-ss-center (ss / bbox ll ur)
  (if (setq bbox (get-ss-bounding-box ss))
    (progn
      (setq ll (car bbox)
            ur (cadr bbox))
      (calc-midpoint ll ur)); funzione definita esternamente
    nil))
;;; GET-SS-DIMENSIONS
;;; Ottiene larghezza e altezza di un selection set
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Lista (larghezza altezza) o nil
(defun get-ss-dimensions (ss / bbox ll ur width height)
  (if (setq bbox (get-ss-bounding-box ss))
    (progn
      (setq ll (car bbox)
            ur (cadr bbox)
            width (- (car ur) (car ll))
            height (- (cadr ur) (cadr ll)))
      (list width height))
    nil))
;;;=============================================================================
;;; 4. ZOOM E VISUALIZZAZIONE
;;;=============================================================================
;;; ZOOM-TO-SS
;;; Zooma su un selection set
;;; Argomenti:
;;;   ss - Selection set
;;;   padding - Margine opzionale (default 5.0)
;;; Ritorna: T se successo
(defun zoom-to-ss (ss padding / bbox ll ur llx lly urx ury pad)
  (if (null padding) (setq padding 5.0))
  (if (setq bbox (get-ss-bounding-box ss))
    (progn
      (setq ll (car bbox)
            ur (cadr bbox)
            llx (- (car ll) padding)
            lly (- (cadr ll) padding)
            urx (+ (car ur) padding)
            ury (+ (cadr ur) padding))
      (command "_.zoom" "_w" (list llx lly) (list urx ury))
      T)
    nil))
;;; DRAW-SS-BOX
;;; Disegna rettangolo bounding box di un SS
;;; Argomenti:
;;;   ss - Selection set
;;;   color - Colore linea (opzionale, default 1=rosso)
;;; Ritorna: Entità rettangolo creato o nil
(defun draw-ss-box (ss color / bbox ll ur old-color rect-ent)
  (if (null color) (setq color 1))
  (if (setq bbox (get-ss-bounding-box ss))
    (progn
      (setq ll (car bbox)
            ur (cadr bbox)
            old-color (getvar "cecolor"))
      (setvar "cecolor" (itoa color))
      (command "_.rectangle" ll ur)
      (setq rect-ent (entlast))
      (setvar "cecolor" old-color)
      rect-ent)
    nil))
;;; HIGHLIGHT-SS
;;; Evidenzia temporaneamente un selection set
;;; Argomenti:
;;;   ss - Selection set
;;;   mode - T per evidenziare, nil per rimuovere evidenziazione
;;; Ritorna: T
(defun highlight-ss (ss mode / idx ent)
  (setq idx 0)
  (if ss
    (while (setq ent (ssname ss idx))
      (if mode
        (redraw ent 3)
        (redraw ent 4))
      (setq idx (1+ idx))))
  T)
;;;=============================================================================
;;; 5. CONVERSIONE E UTILITA'
;;;=============================================================================
;;; SS-TO-LIST
;;; Converte selection set in lista di entità
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Lista di nomi entità (Esempio: (ent1 ent2 ent3 ...))
(defun ss-to-list (ss / idx ent-list ent)
  (setq idx 0 ent-list nil)
  (if ss
    (while (setq ent (ssname ss idx))
      (setq ent-list (append ent-list (list ent)))
      (setq idx (1+ idx))))
  ent-list)
;;; LIST-TO-SS
;;; Converte lista di entità in selection set
;;; Argomenti:
;;;   ent-list - Lista di nomi entità
;;; Ritorna: Selection set o nil
;;; Esempio:
;;;   (list-to-ss '(ent1 ent2 ent3)) => SS
(defun list-to-ss (ent-list / ss)
  (setq ss (ssadd))
  (foreach ent ent-list
    (ssadd ent ss))
  (if (> (sslength ss) 0)
    ss
    nil))
;;; COUNT-SS-BY-TYPE
;;; Conta entità per tipo in un SS
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Lista associativa ((tipo . conteggio) ...)
(defun count-ss-by-type (ss / idx ent etype counts pair)
  (setq idx 0 counts nil)
  (if ss
    (while (setq ent (ssname ss idx))
      (setq etype (get-entity-type ent))
      (if (setq pair (assoc etype counts))
        (setq counts (subst (cons etype (1+ (cdr pair))) pair counts))
        (setq counts (append counts (list (cons etype 1)))))
      (setq idx (1+ idx))))
  counts)
;;; FILTER-SS-BY-TYPE
;;; Filtra SS per tipo entità
;;; Argomenti:
;;;   ss - Selection set
;;;   entity-type - Tipo da filtrare (es. "LINE", "CIRCLE")
;;; Ritorna: Nuovo selection set filtrato
(defun filter-ss-by-type (ss entity-type / idx ent new-ss)
  (setq idx 0 new-ss (ssadd))
  (if ss
    (while (setq ent (ssname ss idx))
      (if (= (get-entity-type ent) entity-type)
        (ssadd ent new-ss))
      (setq idx (1+ idx))))
  (if (> (sslength new-ss) 0)
    new-ss
    nil))
;;; FILTER-SS-BY-LAYER
;;; Filtra SS per layer
;;; Argomenti:
;;;   ss - Selection set
;;;   layer-name - Nome layer da filtrare
;;; Ritorna: Nuovo selection set filtrato
(defun filter-ss-by-layer (ss layer-name / idx ent new-ss)
  (setq idx 0 new-ss (ssadd))
  (if ss
    (while (setq ent (ssname ss idx))
      (if (= (get-entity-attribute ent 8) layer-name)
        (ssadd ent new-ss))
      (setq idx (1+ idx))))
  (if (> (sslength new-ss) 0)
    new-ss
    nil))
;;; SS-LENGTH
;;; Ottiene numero entità in SS
;;; Argomenti:
;;;   ss - Selection set
;;; Ritorna: Numero entità
(defun ss-length (ss)
  (if ss
    (sslength ss)
    0))
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================