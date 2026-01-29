;;;=============================================================================
;;; GESTIONE XDATA (DATI ESTESI)  
;;; Ultimo aggiornamento: 14/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per la gestione dei Extended Data (XDATA) delle entità AutoCAD
;;; Dipendenze esterne: get-entity-handle (_entita.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. RICERCA ENTITA' CON XDATA
;;; 2. LETTURA VALORI XDATA
;;; 3. SCRITTURA XDATA
;;; 4. ELIMINAZIONE XDATA
;;; 5. GESTIONE COLLEGAMENTI XDATA
;;;=============================================================================
;;; CODICI XDATA COMUNI
;;; 1000 - Stringa ASCII
;;; 1001 - Nome applicazione registrata
;;; 1002 - Parentesi apertura/chiusura { }
;;; 1003 - Nome layer
;;; 1005 - Handle database
;;; 1010 - 3 numeri reali (punto 3D)
;;; 1011 - Posizione world space 3D
;;; 1040 - Numero reale
;;; 1070 - Integer 16-bit
;;; 1071 - Long integer 32-bit
;;;=============================================================================
;;; 1. RICERCA ENTITA' CON XDATA
;;;=============================================================================
;;; SEARCH-ENTITIES-BY-XDATA
;;; Trova entità collegate ad un'entità padre tramite XDATA
;;; Argomenti:
;;;   parent-ent - Entità padre di riferimento
;;;   entity-type - Tipo entità da cercare (es. "LWPOLYLINE,HATCH")
;;;   block-name - Nome blocco (solo per INSERT), nil per altri
;;;   xdata-name - Nome applicazione XDATA ("*" per tutte)
;;; Ritorna: Lista di entità trovate
;;; Esempio:
;;;   (search-entities-by-xdata ent-principale "LWPOLYLINE" nil "MIA_APP")
(defun search-entities-by-xdata (parent-ent entity-type block-name xdata-name
                                 / parent-handle ss idx entity-list entity
                                 elist xd-list xd-string filter-list)
  (setq parent-handle (get-entity-handle parent-ent))  
  ;; Costruisce la lista di filtri in base ai parametri
  (cond
    ;; Caso 1: block-name specificato
    (block-name
     (setq filter-list (list '(410 . "Model")))
     (if entity-type
       (setq filter-list (append filter-list
                          (list '(-4 . "<OR")
                                (cons 0 entity-type)
                                (cons 2 block-name)
                                '(-4 . "OR>"))))
       (setq filter-list (append filter-list
                          (list (cons 0 "INSERT")
                                (cons 2 block-name)))))
     (setq filter-list (append filter-list (list (list -3 (list xdata-name))))))
    ;; Caso 2: solo entity-type specificato
    (entity-type
     (setq filter-list (list '(410 . "Model")
                             (cons 0 entity-type)
                             (list -3 (list xdata-name)))))
    ;; Caso 3: nessun filtro su tipo/blocco - solo xdata
    (t
     (setq filter-list (list '(410 . "Model")
                             (list -3 (list xdata-name))))))
  (setq ss (ssget "X" filter-list))
  (if ss
    (progn
      (setq idx 0
            entity-list nil)
      (while (setq entity (ssname ss idx))
        (setq elist (entget entity (list xdata-name)))
        (setq xd-list (cdr (assoc -3 elist)))
        (setq xd-string (cdr (assoc 1000 (cdr (last xd-list)))))
        (if (= parent-handle xd-string)
          (setq entity-list (cons entity entity-list)))
        (setq idx (1+ idx)))))
  entity-list)
;;;=============================================================================
;;; FIND-ENTITIES-WITH-XDATA
;;; Trova tutte le entit� che contengono una specifica applicazione XDATA
;;;
;;; Argomenti:
;;;   xdata-name  - Nome applicazione XDATA (stringa)
;;;   entity-type - Tipo entit� (stringa, opzionale; nil = tutti i tipi)
;;;
;;; Ritorna:
;;;   Lista di ename delle entit� trovate
;;;
;;; Esempio:
;;;   (find-entities-with-xdata "MIA_APP" "LWPOLYLINE")
;;;=============================================================================
(defun find-entities-with-xdata (xdata-name entity-type / ss result idx ent)

  (setq result '())

  (setq ss
    (if entity-type
      (ssget
        "X"
        (list
          '(410 . "Model")
          (cons 0 entity-type)
          (list -3 (list xdata-name))
        )
      )
      (ssget
        "X"
        (list
          '(410 . "Model")
          (list -3 (list xdata-name))
        )
      )
    )
  )

  (if ss
    (progn
      (setq idx 0)
      (repeat (sslength ss)
        (setq ent (ssname ss idx))
        (setq result (cons ent result))
        (setq idx (1+ idx))
      )
    )
  )

  (reverse result)
)

;;;=============================================================================
;;; 2. LETTURA VALORI XDATA
;;;=============================================================================
;;; GET-XDATA-APP-NAME
;;; Ottiene il nome della prima applicazione XDATA di un'entità
;;; Argomenti:
;;;   entity - Entità da interrogare
;;; Ritorna: Nome applicazione o nil
;;; Esempio:
;;;   (get-xdata-app-name ent)
(defun get-xdata-app-name (entity)
  (car (car (cdr (assoc -3 (entget entity '("*")))))))
;;; GET-XDATA-STRING
;;; Ottiene valore stringa da XDATA
;;; Argomenti:
;;;   entity - Entità da interrogare
;;;   app-name - Nome applicazione XDATA
;;; Ritorna: Valore stringa (ultimo inserito) o nil
;;; Esempio:
;;;   (get-xdata-string ent "MIA_APP")
(defun get-xdata-string (entity app-name / elist xd-list result)
  (setq elist (entget entity (list app-name)))
  (setq xd-list (cdr (assoc -3 elist)))
  (setq result (cdr (assoc 1000 (cdr (assoc app-name xd-list)))))
  (if result result ""))  ; restituisce stringa vuota se nil
;;; GET-XDATA-INTEGER
;;; Ottiene valore integer da XDATA
;;; Argomenti:
;;;   entity - Entità da interrogare
;;;   app-name - Nome applicazione XDATA
;;; Ritorna: Valore integer (ultimo inserito) o nil
;;; Esempio:
;;;   (get-xdata-integer ent "MIA_APP")
(defun get-xdata-integer (entity app-name / elist xd-list)
  (setq elist (entget entity (list app-name)))
  (setq xd-list (cdr (assoc -3 elist)))
  (cdr (assoc 1070 (cdr (last xd-list)))))
;;; GET-XDATA-REAL
;;; Ottiene valore real da XDATA
;;; Argomenti:
;;;   entity - Entità da interrogare
;;;   app-name - Nome applicazione XDATA
;;; Ritorna: Valore real (ultimo inserito) o nil
;;; Esempio:
;;;   (get-xdata-real ent "MIA_APP")
(defun get-xdata-real (entity app-name / elist xd-list)
  (setq elist (entget entity (list app-name)))
  (setq xd-list (cdr (assoc -3 elist)))
  (cdr (assoc 1040 (cdr (last xd-list)))))
;;; GET-XDATA-POINT
;;; Ottiene valore punto 3D da XDATA
;;; Argomenti:
;;;   entity - Entità da interrogare
;;;   app-name - Nome applicazione XDATA
;;; Ritorna: Punto (x y z) o nil
;;; Esempio:
;;;   (get-xdata-point ent "MIA_APP")
(defun get-xdata-point (entity app-name / elist xd-list)
  (setq elist (entget entity (list app-name)))
  (setq xd-list (cdr (assoc -3 elist)))
  (cdr (assoc 1010 (cdr (last xd-list)))))
;;; HAS-XDATA
;;; Verifica se un'entità ha XDATA per un'applicazione
;;; Argomenti:
;;;   entity - Entità da verificare
;;;   app-name - Nome applicazione
;;; Ritorna: T se ha XDATA, nil altrimenti
;;; Esempio:
;;;   (has-xdata ent "MIA_APP")
(defun has-xdata (entity app-name / elist)
  (setq elist (entget entity (list app-name)))
  (if (assoc -3 elist) T nil))
;;;=============================================================================
;;; 3. SCRITTURA XDATA
;;;=============================================================================
;;; SET-XDATA-STRING
;;; Imposta valore stringa in XDATA
;;; Argomenti:
;;;   entity - Entità target
;;;   app-name - Nome applicazione XDATA
;;;   value - Valore stringa da impostare
;;; Ritorna: T se successo, nil se errore
;;; Esempio:
;;;   (set-xdata-string ent "MIA_APP" "valore")
(defun set-xdata-string (entity app-name value)
  (add-xdata entity app-name 1000 value))
;;; SET-XDATA-INTEGER
;;; Imposta valore integer in XDATA
;;; Argomenti:
;;;   entity - Entità target
;;;   app-name - Nome applicazione XDATA
;;;   value - Valore integer da impostare
;;; Ritorna: T se successo
;;; Esempio:
;;;   (set-xdata-integer ent "MIA_APP" 123)
(defun set-xdata-integer (entity app-name value)
  (add-xdata entity app-name 1070 value))
;;; SET-XDATA-REAL
;;; Imposta valore real in XDATA
;;; Argomenti:
;;;   entity - Entità target
;;;   app-name - Nome applicazione XDATA
;;;   value - Valore real da impostare
;;; Ritorna: T se successo
;;; Esempio:
;;;   (set-xdata-real ent "MIA_APP" 1.23)
(defun set-xdata-real (entity app-name value)
  (add-xdata entity app-name 1040 value))
;;; SET-XDATA-POINT
;;; Imposta punto 3D in XDATA
;;; Argomenti:
;;;   entity - Entità target
;;;   app-name - Nome applicazione XDATA
;;;   point - Punto (x y z)
;;; Ritorna: T se successo
;;; Esempio:
;;;   (set-xdata-point ent "MIA_APP" '(10 20 0))
(defun set-xdata-point (entity app-name point)
  (add-xdata entity app-name 1010 point))
;;; ADD-XDATA
;;; Funzione generica per aggiungere XDATA
;;; Argomenti:
;;;   entity - Entità target
;;;   app-name - Nome applicazione
;;;   xdata-code - Codice tipo dato (1000, 1040, 1070, ecc.)
;;;   value - Valore da impostare
;;; Ritorna: T se successo
;;; Esempio:
;;;   (add-xdata ent "MIA_APP" 1000 "valore")
(defun add-xdata (entity app-name xdata-code value 
                  / elist xd-list xd-app app-data new-data)
  (regapp app-name)
  (setq elist (entget entity (list app-name)))
  (setq app-data (list app-name
                       (cons 1002 "{")
                       (cons xdata-code value)
                       (cons 1002 "}")))
  (if (setq xd-list (assoc -3 elist))
    (progn
      (setq xd-list (cdr xd-list))
      (if (setq xd-app (assoc app-name xd-list))
        (setq new-data (subst app-data xd-app xd-list))
        (setq new-data (append xd-list (list app-data))))
      (setq elist (subst (cons -3 new-data) (assoc -3 elist) elist)))
    (setq elist (append elist (list (cons -3 (list app-data))))))
  (entmod elist)
  (entupd entity)
  T)
;;;=============================================================================
;;; 4. ELIMINAZIONE XDATA
;;;=============================================================================
;;; REMOVE-XDATA
;;; Rimuove XDATA di un'applicazione da un'entità
;;; Argomenti:
;;;   entity - Entità da cui rimuovere XDATA
;;;   app-name - Nome applicazione (o lista di nomi)
;;; Ritorna: T se successo
;;; Esempio:
;;;   (remove-xdata ent "MIA_APP")
(defun remove-xdata (entity app-name / entlst tmplst memb)
  (setq entlst (entget entity app-name))
  (foreach memb (cdr (assoc -3 entlst))
    (setq tmplst (cons -3 (list (cons (car memb) nil)))
          entlst (subst tmplst (assoc -3 entlst) entlst)
          entlst (entmod entlst)))
  T)
;;; REMOVE-ALL-XDATA
;;; Rimuove tutti gli XDATA da un'entità
;;; Argomenti:
;;;   entity - Entità da pulire
;;; Ritorna: T se successo
;;; Esempio:
;;;   (remove-all-xdata ent)
(defun remove-all-xdata (entity)
  (remove-xdata entity '("*")))
;;;=============================================================================
;;; 5. GESTIONE COLLEGAMENTI XDATA
;;;=============================================================================
;;; DELETE-LINKED-ENTITIES
;;; Elimina tutte le entità collegate tramite XDATA
;;; Argomenti:
;;;   parent-entity - Entità padre
;;;   xdata-name - Nome applicazione XDATA
;;; Ritorna: Numero entità eliminate
;;; Esempio:
;;;   (delete-linked-entities ent "MIA_APP")
(defun delete-linked-entities (parent-entity xdata-name 
                                / ss idx entity-list entity elist 
                                xd-list xd-string count)
  (setq count 0)
  (if (setq ss (ssget "X" 
                 (list '(410 . "Model") 
                       (list -3 (list xdata-name)))))
    (progn
      (setq idx 0
            entity-list nil)
      (while (setq entity (ssname ss idx))
        (setq elist (entget entity (list xdata-name)))
        (setq xd-list (cdr (assoc -3 elist)))
        (setq xd-string (cdr (assoc 1000 (cdr (last xd-list)))))
        (if (= (get-entity-handle parent-entity) xd-string)
          (setq entity-list (cons entity entity-list)))
        (setq idx (1+ idx)))
      (foreach entity entity-list
        (if (entget entity)
          (progn
            (entdel entity)
            (setq count (1+ count)))))))
  count)
;;; DELETE-LINKED-ENTITIES-FILTERED
;;; Elimina entità collegate filtrate per tipo
;;; Argomenti:
;;;   parent-entity - Entità padre
;;;   xdata-name - Nome applicazione XDATA
;;;   type-filter - Lista tipi entità da eliminare
;;; Ritorna: Numero entità eliminate
;;; Esempio:
;;;   (delete-linked-entities-filtered ent "MIA_APP" '("LWPOLYLINE" "INSERT"))
(defun delete-linked-entities-filtered (parent-entity xdata-name type-filter
                                        / ss idx entity-list entity elist
                                        xd-list xd-string el count ent-type)
  (setq count 0)
  (if (setq ss (ssget "X"
                 (list '(410 . "Model")
                       (list -3 (list xdata-name)))))
    (progn
      (setq idx 0
            entity-list nil)
      (while (setq entity (ssname ss idx))
        (setq elist (entget entity (list xdata-name)))
        (setq xd-list (cdr (assoc -3 elist)))
        (setq xd-string (cdr (assoc 1000 (cdr (last xd-list)))))
        (if (= (get-entity-handle parent-entity) xd-string)
          (progn
            (setq ent-type (cdr (assoc 0 elist)))
            (foreach el type-filter
              (if (= ent-type el)
                (setq entity-list (cons entity entity-list))))))
        (setq idx (1+ idx)))
      (foreach entity entity-list
        (if (entget entity)
          (progn
            (entdel entity)
            (setq count (1+ count)))))))
  count)
;;; DELETE-ALL-LINKED-ENTITIES
;;; Elimina tutte le entità con XDATA collegato a parent
;;; Argomenti:
;;;   parent-entity - Entità padre
;;; Ritorna: Numero entità eliminate
;;; Esempio:
;;;   (delete-all-linked-entities ent)
(defun delete-all-linked-entities (parent-entity)
  (delete-linked-entities parent-entity "*"))
;;; LINK-ENTITY-TO-PARENT
;;; Collega un'entità ad un'entità padre tramite XDATA
;;; Argomenti:
;;;   child-entity - Entità figlio da collegare
;;;   parent-entity - Entità padre
;;;   app-name - Nome applicazione XDATA
;;; Ritorna: T se successo
;;; Esempio:
;;;   (link-entity-to-parent child-ent parent-ent "MIA_APP")
(defun link-entity-to-parent (child-entity parent-entity app-name)
  (set-xdata-string child-entity app-name 
                   (get-entity-handle parent-entity)))
;;;=============================================================================
;;; 6. FUNZIONI HELPER
;;;=============================================================================
;;; LIST-XDATA-APPS
;;; Lista tutte le applicazioni XDATA di un'entità
;;; Argomenti:
;;;   entity - Entità da analizzare
;;; Ritorna: Lista nomi applicazioni
;;; Esempio:
;;;   (list-xdata-apps ent)
(defun list-xdata-apps (entity / elist xd-list result app)
  (setq elist (entget entity '("*")))
  (setq xd-list (cdr (assoc -3 elist)))
  (setq result nil)
  (if xd-list
    (foreach app xd-list
      (setq result (cons (car app) result))))
  (reverse result))
;;; XDATA-SIZE
;;; Calcola dimensione XDATA corrente di un'entità
;;; Argomenti:
;;;   entity - Entità da analizzare
;;; Ritorna: Bytes utilizzati
;;; Esempio:
;;;   (xdata-size ent)
(defun xdata-size (entity / elist xd-list)
  (setq elist (entget entity '("*")))
  (setq xd-list (assoc -3 elist))
  (if xd-list
    (xdsize xd-list)
    0))
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================