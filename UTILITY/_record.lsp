;;;=============================================================================
;;; GESTIONE RECORD  
;;; Ultimo aggiornamento: 14/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per  manipolare record (liste associative)
;;; Dipendenze esterne: -
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; STRUTTURA RECORD
;;; Un record è una lista associativa del tipo:
;;; '((campo1 . valore1) (campo2 . valore2) (campo3 . valore3))
;;; Esempio: '((nome . "Mario") (eta . 30) (citta . "Milano"))
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. MODIFICA CAMPI RECORD
;;; 2. QUERY E RICERCA
;;; 3. ESTRAZIONE DATI
;;;=============================================================================
;;; 1. MODIFICA CAMPI RECORD
;;;=============================================================================
;;; REPLACE-FIELD
;;; Sostituisce il valore di un campo in un record
;;; Argomenti:
;;;   record - Record (lista associativa)
;;;   field  - Nome del campo (simbolo o stringa)
;;;   value  - Nuovo valore da assegnare
;;; Ritorna: Nuovo record con campo modificato
;;; Esempio:
;;;   (replace-field '((nome . "Mario") (eta . 30)) 'eta 31)
;;;   => ((nome . "Mario") (eta . 31))
(defun replace-field (record field value / new-pair old-pair)
  (setq new-pair (cons field value))
  (setq old-pair (assoc field record))
  (if old-pair
    (subst new-pair old-pair record)
    (cons new-pair record)))
;;; UPDATE-FIELD
;;; Aggiorna il valore di un campo esistente in un record
;;; Argomenti:
;;;   record - Record (lista associativa)
;;;   field  - Nome del campo da aggiornare
;;;   value  - Nuovo valore
;;; Ritorna: Record aggiornato
;;; Nota: Se il campo non esiste, viene aggiunto
;;; Esempio:
;;;   (update-field '((x . 10) (y . 20)) 'x 15)
;;;   => ((x . 15) (y . 20))
(defun update-field (record field value / new-pair old-pair)
  (setq new-pair (cons field value))
  (setq old-pair (assoc field record))
  (if old-pair
    (subst new-pair old-pair record)
    (cons new-pair record)))
;;;=============================================================================
;;; 2. QUERY E RICERCA
;;;=============================================================================
;;; QUERY-RECORDS
;;; Cerca tutti i record in una lista che hanno un campo con valore specifico
;;; Argomenti:
;;;   field       - Nome del campo da verificare
;;;   value       - Valore da cercare
;;;   record-list - Lista di record da interrogare
;;; Ritorna: Lista di record che soddisfano la condizione, o 'ERRORE se nessuno
;;; Esempio:
;;;   (query-records 'citta "Milano" 
;;;     '(((nome . "Mario") (citta . "Milano"))
;;;       ((nome . "Luca") (citta . "Roma"))
;;;       ((nome . "Anna") (citta . "Milano"))))
;;;   => (((nome . "Mario") (citta . "Milano")) 
;;;       ((nome . "Anna") (citta . "Milano")))
(defun query-records (field value record-list / current-val rec result)
  (setq result nil)
  (foreach rec record-list
    (progn
      (setq current-val (cdr (assoc field rec)))
      (if (equal value current-val)
        (if (= result nil)
          (setq result (list rec))
          (setq result (append result (list rec)))))))
  (cond
    ((= result nil) 'ERRORE)
    (t result)))
;;; FIND-RECORDS-BY-FIELD
;;; Alias per query-records con nome più descrittivo
(defun find-records-by-field (field value record-list)
  (query-records field value record-list))
;;; GET-FIELD-VALUE
;;; Estrae il valore di un campo da un record
;;; Argomenti:
;;;   record - Record da cui estrarre
;;;   field  - Nome del campo
;;; Ritorna: Valore del campo, o nil se non esiste
;;; Esempio:
;;;   (get-field-value '((nome . "Mario") (eta . 30)) 'eta) => 30
(defun get-field-value (record field)
  (cdr (assoc field record)))
;;;=============================================================================
;;; 3. ESTRAZIONE DATI
;;;=============================================================================
;;; EXTRACT-FIELD-VALUES
;;; Estrae tutti i valori unici di un campo da una lista di record
;;; Argomenti:
;;;   record-list - Lista di record
;;;   field       - Nome del campo da estrarre
;;; Ritorna: Lista di valori unici (senza duplicati)
;;; Esempio:
;;;   (extract-field-values 
;;;     '(((nome . "Mario") (citta . "Milano"))
;;;       ((nome . "Luca") (citta . "Roma"))
;;;       ((nome . "Anna") (citta . "Milano")))
;;;     'citta)
;;;   => ("Milano" "Roma")
(defun extract-field-values (record-list field / result rec val prev-val)
  (setq result nil)
  (foreach rec record-list
    (progn
      (setq val (cdr (assoc field rec)))
      (if (= result nil)
        (setq result (list val))
        (if (/= prev-val val)
          (setq result (cons val result))))
      (setq prev-val val)))
  (reverse result))
;;; EXTRACT-ALL-FIELD-VALUES
;;; Estrae tutti i valori di un campo (inclusi duplicati)
;;; Argomenti:
;;;   record-list - Lista di record
;;;   field       - Nome del campo da estrarre
;;; Ritorna: Lista di tutti i valori (con duplicati)
;;; Esempio:
;;;   (extract-all-field-values 
;;;     '(((x . 1) (y . 2)) ((x . 1) (y . 3)) ((x . 2) (y . 2)))
;;;     'x)
;;;   => (1 1 2)
(defun extract-all-field-values (record-list field / result rec val)
  (setq result nil)
  (foreach rec record-list
    (setq val (cdr (assoc field rec)))
    (setq result (cons val result)))
  (reverse result))
;;; COUNT-RECORDS
;;; Conta quanti record soddisfano una condizione
;;; Argomenti:
;;;   field       - Nome del campo
;;;   value       - Valore da cercare
;;;   record-list - Lista di record
;;; Ritorna: Numero di record trovati
(defun count-records (field value record-list / count rec current-val)
  (setq count 0)
  (foreach rec record-list
    (setq current-val (cdr (assoc field rec)))
    (if (equal value current-val)
      (setq count (1+ count))))
  count)
;;; FILTER-RECORDS
;;; Filtra record basandosi su una funzione di test
;;; Argomenti:
;;;   test-func   - Funzione che ritorna T o nil per ogni record
;;;   record-list - Lista di record da filtrare
;;; Ritorna: Lista di record che passano il test
;;; Esempio:
;;;   (filter-records 
;;;     (function (lambda (r) (> (cdr (assoc 'eta r)) 25)))
;;;     '(((nome . "Mario") (eta . 30)) ((nome . "Luca") (eta . 20))))
;;;   => (((nome . "Mario") (eta . 30)))
(defun filter-records (test-func record-list / result rec)
  (setq result nil)
  (foreach rec record-list
    (if (apply test-func (list rec))
      (setq result (append result (list rec)))))
  result)
;;;=============================================================================
;;; FUNZIONI AVANZATE
;;;=============================================================================
;;; ADD-FIELD
;;; Aggiunge un nuovo campo a un record
;;; Argomenti:
;;;   record - Record originale
;;;   field  - Nome nuovo campo
;;;   value  - Valore del nuovo campo
;;; Ritorna: Record con campo aggiunto
(defun add-field (record field value)
  (append record (list (cons field value))))
;;; REMOVE-FIELD
;;; Rimuove un campo da un record
;;; Argomenti:
;;;   record - Record originale
;;;   field  - Nome campo da rimuovere
;;; Ritorna: Record senza il campo specificato
(defun remove-field (record field / pair)
  (setq pair (assoc field record))
  (if pair
    (remove-item pair record)
    record))
;;; REMOVE-ITEM (funzione helper)
(defun remove-item (item lst / result)
  (setq result nil)
  (foreach el lst
    (if (not (equal el item))
      (setq result (append result (list el)))))
  result)
;;; HAS-FIELD
;;; Verifica se un record contiene un campo
;;; Argomenti:
;;;   record - Record da verificare
;;;   field  - Nome campo da cercare
;;; Ritorna: T se campo esiste, nil altrimenti
(defun has-field (record field)
  (if (assoc field record) T nil))
;;; MERGE-RECORDS
;;; Unisce due record (il secondo sovrascrive campi duplicati)
;;; Argomenti:
;;;   record1 - Primo record
;;;   record2 - Secondo record (prioritario)
;;; Ritorna: Record unito
(defun merge-records (record1 record2 / result pair)
  (setq result record1)
  (foreach pair record2
    (setq result (replace-field result (car pair) (cdr pair))))
  result)
;;; GET-FIELD-NAMES
;;; Estrae tutti i nomi dei campi da un record
;;; Argomenti:
;;;   record - Record da analizzare
;;; Ritorna: Lista di nomi campi
(defun get-field-names (record / result pair)
  (setq result nil)
  (foreach pair record
    (setq result (append result (list (car pair)))))
  result)
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================