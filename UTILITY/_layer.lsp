;;;=============================================================================
;;; GESTIONE LAYER
;;; Ultimo aggiornamento: 12/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per operazioni sui layer
;;; Dipendenze esterne: GET-ENTITY-ATTRIBUTE, SET-ENTITY-ATTRIBUTE (_entita.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. VERIFICA E INTERROGAZIONE LAYER
;;; 2. CREAZIONE E MODIFICA LAYER
;;; 3. STATO LAYER (ON/OFF/LOCK/FREEZE)
;;; 4. CATTURA E RIPRISTINO STATO
;;; 5. NAVIGAZIONE LAYER COME PAGINE
;;; 6. OPERAZIONI MASSIVE
;;; 7. ORDINAMENTO E ORGANIZZAZIONE
;;;=============================================================================
;;; 1. VERIFICA E INTERROGAZIONE LAYER
;;;=============================================================================
;;; LAYER-EXISTS
;;; Verifica se un layer esiste nel disegno
;;; Argomenti:
;;;   layer-name - Nome layer da verificare
;;; Ritorna: T se esiste, nil altrimenti
(defun layer-exists (layer-name)
  (if (not (null (tblsearch "LAYER" layer-name))) T nil))
;;; IS-LAYER-ON
;;; Verifica se un layer è acceso
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: T se acceso, nil se spento
(defun is-layer-on (layer-name / layer-info)
  (if (setq layer-info (tblsearch "LAYER" layer-name))
    (if (> (cdr (assoc 62 layer-info)) 0) T nil)
    nil))
;;; IS-LAYER-LOCKED
;;; Verifica se un layer è bloccato
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: T se bloccato, nil se sbloccato
(defun is-layer-locked (layer-name / layer-info flags)
  (if (setq layer-info (tblsearch "LAYER" layer-name))
    (progn
      (setq flags (cdr (assoc 70 layer-info)))
      (if (= (logand flags 4) 4) T nil))
    nil))
;;; IS-LAYER-FROZEN
;;; Verifica se un layer è congelato
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: T se congelato, nil se scongelato
(defun is-layer-frozen (layer-name / layer-info flags)
  (if (setq layer-info (tblsearch "LAYER" layer-name))
    (progn
      (setq flags (cdr (assoc 70 layer-info)))
      (if (= (logand flags 1) 1) T nil))
    nil))
;;; GET-LAYER-COLOR
;;; Ottiene colore di un layer
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: Numero colore o nil
(defun get-layer-color (layer-name / layer-info)
  (if (setq layer-info (tblsearch "LAYER" layer-name))
    (abs (cdr (assoc 62 layer-info)))
    nil))
;;; GET-LAYER-LINETYPE
;;; Ottiene tipo linea di un layer
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: Nome tipo linea o nil
(defun get-layer-linetype (layer-name / layer-info)
  (if (setq layer-info (tblsearch "LAYER" layer-name))
    (cdr (assoc 6 layer-info))
    nil))
;;;=============================================================================
;;; 2. CREAZIONE E MODIFICA LAYER
;;;=============================================================================
;;; CREATE-LAYER
;;; Crea un nuovo layer o lo imposta come corrente
;;; Argomenti:
;;;   layer-name - Nome layer
;;;   color - Colore (opzionale)
;;;   linetype - Tipo linea (opzionale)
;;; Ritorna: T se successo
(defun create-layer (layer-name color linetype / regen)
  (if (and layer-name (/= layer-name ""))
    (if (tblsearch "LAYER" layer-name)
      (progn
        (command "_.LAYER" "_S" layer-name "")
        (command "_.redraw")
        T)
      (progn
        (setq regen (getvar "REGENMODE"))
        (setvar "REGENMODE" 0)
        (command "_.LAYER" "_M" layer-name)
        (if color
          (command "_C" color layer-name))
        (if linetype
          (command "_LT" linetype layer-name))
        (command "")
        (setvar "REGENMODE" regen)
        T))
    nil))
;;; SET-CURRENT-LAYER
;;; Imposta layer corrente
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: T se successo
(defun set-current-layer (layer-name)
  (if (and layer-name (/= layer-name "") (layer-exists layer-name))
    (progn
      (setvar "clayer" layer-name)
      T)
    nil))
;;; GET-CURRENT-LAYER
;;; Ottiene nome layer corrente
;;; Ritorna: Nome layer corrente
(defun get-current-layer ()
  (getvar "clayer"))
;;;=============================================================================
;;; 3. STATO LAYER (ON/OFF/LOCK/FREEZE)
;;;=============================================================================
;;; TURN-LAYER-ON
;;; Accende un layer
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: T se successo
(defun turn-layer-on (layer-name)
  (if (and layer-name (/= layer-name "") (layer-exists layer-name))
    (progn
      (command "_.layer" "_set" layer-name "_on" layer-name "")
      T)
    nil))
;;; TURN-LAYER-OFF
;;; Spegne un layer (passando prima su layer 0)
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: T se successo
(defun turn-layer-off (layer-name)
  (if (and layer-name (/= layer-name ""))
    (progn
      (command "_.layer" "_set" "0" "_off" layer-name "")
      T)
    nil))
;;; LOCK-LAYER
;;; Blocca un layer (può essere lista separata da virgole)
;;; Argomenti:
;;;   layer-spec - Nome layer o lista separata da virgole
;;; Ritorna: T
(defun lock-layer (layer-spec)
  (command "_.layer" "_lock" layer-spec "")
  T)
;;; UNLOCK-LAYER
;;; Sblocca un layer (può essere lista separata da virgole)
;;; Argomenti:
;;;   layer-spec - Nome layer o lista separata da virgole
;;; Ritorna: T
(defun unlock-layer (layer-spec)
  (command "_.layer" "_unlock" layer-spec "")
  T)
;;; FREEZE-LAYER
;;; Congela un layer
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: T
(defun freeze-layer (layer-name)
  (if (and layer-name (/= layer-name ""))
    (progn
      (command "_.layer" "_set" "0" "_freeze" layer-name "")
      T)
    nil))
;;; THAW-LAYER
;;; Scongela un layer
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: T
(defun thaw-layer (layer-name)
  (if (and layer-name (/= layer-name ""))
    (progn
      (command "_.layer" "_thaw" layer-name "")
      T)
    nil))
;;; SET-LAYER-PLOTTABLE
;;; Imposta layer come stampabile
;;; Argomenti:
;;;   layer-name - Nome layer
;;;   plottable - T per stampabile, nil per non stampabile
;;; Ritorna: T
(defun set-layer-plottable (layer-name plottable)
  (if plottable
    (command "_.layer" "_p" "_p" layer-name "")
    (command "_.layer" "_p" "_n" layer-name ""))
  T)
;;;=============================================================================
;;; 4. CATTURA E RIPRISTINO STATO
;;;=============================================================================
;;; LIST-ALL-LAYERS
;;; Elenca tutti i layer nel disegno
;;; Ritorna: Lista nomi layer
(defun list-all-layers (/ lay result)
  (setq lay (tblnext "LAYER" T)
        result (list (cdr (cadr lay))))
  (while (setq lay (tblnext "layer"))
    (setq result (cons (cdr (cadr lay)) result)))
  (reverse result))
;;; LIST-LAYERS-ON-OFF
;;; Elenca layer accesi e spenti
;;; Ritorna: Imposta variabili globali G-LAYERS-ON e G-LAYERS-OFF
(defun list-layers-on-off (/ lay)
  (setq g-layers-on nil
        g-layers-off nil)
  (setq lay (tblnext "LAYER" T))
  (if (> (cdr (assoc 62 lay)) 0)
    (setq g-layers-on (cons (cdr (cadr lay)) g-layers-on))
    (setq g-layers-off (cons (cdr (cadr lay)) g-layers-off)))
  (while (setq lay (tblnext "layer"))
    (if (> (cdr (assoc 62 lay)) 0)
      (setq g-layers-on (cons (cdr (cadr lay)) g-layers-on))
      (setq g-layers-off (cons (cdr (cadr lay)) g-layers-off))))
  (setq g-layers-on (reverse g-layers-on)
        g-layers-off (reverse g-layers-off)))
;;; CAPTURE-LAYER-STATE
;;; Cattura stato layer corrente e lista on/off
;;; Ritorna: Imposta G-LAYER-OLD, G-LAYERS-ON, G-LAYERS-OFF
(defun capture-layer-state ()
  (setq g-layer-old (getvar "clayer"))
  (list-layers-on-off))
;;; RESTORE-LAYER-STATE
;;; Ripristina layer corrente e stato on/off
;;; Ritorna: T
(defun restore-layer-state ()
  (if g-layer-old
    (setvar "clayer" g-layer-old))
  (restore-layers-on-off)
  T)
;;; SET-LAYERS-ON-OFF
;;; Accende e spegne liste di layer
;;; Argomenti:
;;;   on-list - Lista layer da accendere
;;;   off-list - Lista layer da spegnere
;;; Ritorna: T
(defun set-layers-on-off (on-list off-list / layer-on layer-off el is-current)
  (setq layer-on "" layer-off "" is-current nil)
  (foreach el on-list
    (setq layer-on (strcat layer-on "," el)))
  (if (> (strlen layer-on) 0)
    (setq layer-on (substr layer-on 2)))
  (foreach el off-list
    (setq layer-off (strcat layer-off "," el))
    (if (= (getvar "clayer") el)
      (setq is-current T)))
  (if (> (strlen layer-off) 0)
    (setq layer-off (substr layer-off 2)))
  (if (= (substr layer-off 1 1) "*")
    (setvar "clayer" "0"))
  (if (and (> (strlen layer-on) 0) (> (strlen layer-off) 0))
    (if is-current
      (command "_.layer" "_on" layer-on "_off" layer-off "_Y" "")
      (command "_.layer" "_on" layer-on "_off" layer-off ""))
    (progn
      (if (> (strlen layer-on) 0)
        (command "_.layer" "_on" layer-on ""))
      (if (> (strlen layer-off) 0)
        (if is-current
          (command "_.layer" "_off" layer-off "_Y" "")
          (command "_.layer" "_off" layer-off "")))))
  T)
;;; RESTORE-LAYERS-ON-OFF
;;; Ripristina stato layer salvato
;;; Ritorna: T
(defun restore-layers-on-off ()
  (set-layers-on-off g-layers-on g-layers-off))
;;;=============================================================================
;;; 5. NAVIGAZIONE LAYER COME PAGINE
;;;=============================================================================
;;; Variabile globale per prefisso pagine (default "PAG_")
(if (not g-page-prefix) (setq g-page-prefix "PAG_"))
;;; NEXT-PAGE
;;; Avanza alla pagina successiva
;;; Ritorna: T se successo
(defun next-page ()
  (browse-page T))
;;; PREVIOUS-PAGE
;;; Torna alla pagina precedente
;;; Ritorna: T se successo
(defun previous-page ()
  (browse-page nil))
;;; BROWSE-PAGE
;;; Sfoglia pagine layer (avanti o indietro)
;;; Argomenti:
;;;   forward - T per avanti, nil per indietro
;;; Ritorna: T se successo
(defun browse-page (forward / fixed-layer current-layer current-page 
                    next-page next-layer)
  (setq fixed-layer (strcat g-page-prefix "0"))
  (setq current-layer (getvar "clayer")
        current-page (atoi (substr current-layer 
                                  (1+ (strlen g-page-prefix)))))
  (if forward
    (setq next-page (1+ current-page))
    (setq next-page (1- current-page)))
  (setq next-layer (strcat g-page-prefix (itoa next-page)))
  (if (layer-exists next-layer)
    (if (> next-page 0)
      (progn
        (create-layer next-layer nil nil)
        (set-layers-on-off (list fixed-layer) (list current-layer))
        T))
    nil))
;;; GO-TO-PAGE-ZERO
;;; Va alla pagina zero (layer base)
;;; Ritorna: T se successo
(defun go-to-page-zero (/ fixed-layer)
  (setq fixed-layer (strcat g-page-prefix "0"))
  (if (layer-exists fixed-layer)
    (progn
      (command "._layer" "_off" "*" "_y" "_set" fixed-layer 
               "_on" fixed-layer "")
      T)
    nil))
;;; GO-TO-PAGE-ONE
;;; Va alla pagina uno
;;; Ritorna: T se successo
(defun go-to-page-one (/ layer-one)
  (go-to-page-zero)
  (setq layer-one (strcat g-page-prefix "1"))
  (if (layer-exists layer-one)
    (progn
      (command "._layer" "_set" layer-one "_on" layer-one "")
      T)
    nil))
;;;=============================================================================
;;; 6. OPERAZIONI MASSIVE
;;;=============================================================================
;;; MOVE-ENTITIES-TO-LAYER
;;; Sposta entità da lista layer a nuovo layer
;;; Argomenti:
;;;   source-layers - Lista nomi layer sorgente
;;;   target-layer - Nome layer destinazione
;;;   color - Colore per nuovo layer (opzionale)
;;; Ritorna: Numero entità spostate
(defun move-entities-to-layer (source-layers target-layer color 
                               / ss layer-list idx ent prev-layer linetype count)
  (setq count 0)
  (princ "\nSpostamento entita' in corso...")
  (foreach el source-layers
    (command "_.layer" "_U" el "_T" el "")
    (setq layer-list (cons (cons 8 el) layer-list)))
  (setq ss (ssget "X"
             (append '((-4 . "<OR"))
                     layer-list
                     '((-4 . "OR>")))))
  (if (and ss (> (sslength ss) 0))
    (progn
      (setq prev-layer (getvar "clayer"))
      (create-layer target-layer color nil)
      (setq idx 0)
      (while (setq ent (ssname ss idx))
        (setq layer-ent (get-entity-attribute ent 8))
        (setq linetype (get-layer-linetype layer-ent))
        (set-entity-attribute ent 8 target-layer)
        (set-entity-attribute ent 62 256)
        (if (null (get-entity-attribute ent 6))
          (set-entity-attribute ent 6 linetype))
        (setq count (1+ count))
        (setq idx (1+ idx)))
      (setvar "clayer" prev-layer)
      (princ (strcat "\n" (itoa count) " entita' spostate"))))
  count)
;;; TURN-ALL-LAYERS-OFF
;;; Spegne tutti i layer tranne quello specificato
;;; Argomenti:
;;;   except-layer - Layer da mantenere acceso (default "0")
;;; Ritorna: T
(defun turn-all-layers-off (except-layer)
  (if (null except-layer) (setq except-layer "0"))
  (command "_.layer" "_off" "*" "_y" "_on" except-layer "")
  T)
;;; TURN-ALL-LAYERS-ON
;;; Accende tutti i layer
;;; Ritorna: T
(defun turn-all-layers-on ()
  (command "_.layer" "_on" "*" "")
  T)
;;; UNLOCK-ALL-LAYERS
;;; Sblocca tutti i layer
;;; Ritorna: T
(defun unlock-all-layers ()
  (command "_.layer" "_unlock" "*" "")
  T)
;;; THAW-ALL-LAYERS
;;; Scongela tutti i layer
;;; Ritorna: T
(defun thaw-all-layers ()
  (command "_.layer" "_thaw" "*" "")
  T)
;;;=============================================================================
;;; 7. ORDINAMENTO E ORGANIZZAZIONE
;;;=============================================================================
;;; SET-LAYER-DRAW-ORDER
;;; Modifica ordine visualizzazione entità di un layer
;;; Argomenti:
;;;   layer-name - Nome layer
;;;   order - "_Back" o "_Front"
;;; Ritorna: Numero entità elaborate
(defun set-layer-draw-order (layer-name order / ss idx ent count)
  (setq count 0)
  (if (setq ss (ssget "X" (list (cons 8 layer-name))))
    (progn
      (command "_.draworder" ss "" order)
      (setq idx 0)
      (while (setq ent (ssname ss idx))
        (entupd ent)
        (setq count (1+ count))
        (setq idx (1+ idx)))))
  count)
;;; SEND-LAYER-TO-BACK
;;; Porta layer in fondo
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: Numero entità
(defun send-layer-to-back (layer-name)
  (set-layer-draw-order layer-name "_Back"))
;;; BRING-LAYER-TO-FRONT
;;; Porta layer in primo piano
;;; Argomenti:
;;;   layer-name - Nome layer
;;; Ritorna: Numero entità
(defun bring-layer-to-front (layer-name)
  (set-layer-draw-order layer-name "_Front"))

;;;=============================================================================
;;; FINE FILE
;;;=============================================================================