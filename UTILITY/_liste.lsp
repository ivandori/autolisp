;;;=============================================================================
;;; GESTIONE LISTE
;;; Ultimo aggiornamento: 12/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per operazioni sulle liste
;;; Dipendenze esterne: STRING-SEARCH (_stringhe.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. CONVERSIONE LISTE/STRINGHE
;;; 2. AGGIUNTA E INSERIMENTO ELEMENTI
;;; 3. RICERCA ELEMENTI
;;; 4. RIMOZIONE ELEMENTI
;;; 5. ESTRAZIONE SOTTOLISTE
;;; 6. ORDINAMENTO
;;; 7. CONTEGGIO E STATISTICHE
;;; 8. CONVERSIONE TIPI
;;; 9. INPUT/OUTPUT FILE
;;; 10. LISTE ASSOCIATIVE (SEPARATE)
;;;=============================================================================
;;; 1. CONVERSIONE LISTE/STRINGHE
;;;=============================================================================
;;; LIST-TO-STRING
;;; Converte lista di stringhe in stringa con delimitatore
;;; Argomenti:
;;;   lst - Lista di stringhe
;;;   delimiter - Carattere separatore
;;; Ritorna: Stringa unica
;;; Esempio:
;;;   (list-to-string '("A" "B" "C") ",") => "A,B,C"
(defun list-to-string (lst delimiter / result)
  (setq result "")
  (foreach el lst
    (setq result (strcat result delimiter el)))
  (if (> (strlen result) 0)
    (substr result 2)
    ""))
;;; LISTS-TO-STRING-LIST
;;; Converte lista di liste in lista di stringhe
;;; Argomenti:
;;;   lst - Lista di liste di stringhe
;;;   delimiter - Carattere separatore
;;; Ritorna: Lista di stringhe
(defun lists-to-string-list (lst delimiter / result)
  (setq result nil)
  (foreach sub-list lst
    (setq result (cons (list-to-string sub-list delimiter) result)))
  (reverse result))
;;;=============================================================================
;;; 2. AGGIUNTA E INSERIMENTO ELEMENTI
;;;=============================================================================
;;; APPEND-ELEMENT
;;; Aggiunge elemento alla fine della lista (opposto di cons)
;;; Argomenti:
;;;   element - Elemento da aggiungere
;;;   lst - Lista
;;; Ritorna: Nuova lista con elemento in coda
(defun append-element (element lst)
  (reverse (cons element (reverse lst))))
;;; APPEND-UNIQUE
;;; Aggiunge elemento solo se non esiste già
;;; Argomenti:
;;;   element - Elemento da aggiungere
;;;   lst - Lista
;;; Ritorna: Nuova lista
(defun append-unique (element lst)
  (if (not (member element lst))
    (append lst (list element))
    lst))
;;; INSERT-AT-POSITION
;;; Inserisce elemento in posizione specifica
;;; Argomenti:
;;;   element - Elemento da inserire
;;;   lst - Lista
;;;   position - Posizione (0-based)
;;; Ritorna: Nuova lista
(defun insert-at-position (element lst position / pos result)
  (setq pos 0 result nil)
  (foreach el lst
    (if (= position pos)
      (setq result (cons element result)))
    (setq result (cons el result))
    (setq pos (1+ pos)))
  (if (>= position pos)
    (setq result (cons element result)))
  (reverse result))
;;;=============================================================================
;;; 3. RICERCA ELEMENTI
;;;=============================================================================
;;; FIND-POSITION
;;; Trova posizione di un elemento in lista (ottimizzata)
;;; Argomenti:
;;;   element - Elemento da cercare
;;;   lst - Lista in cui cercare
;;; Ritorna: Posizione (0-based) o nil
(defun find-position (element lst / idx)
  (setq idx 0)
  (while (and lst (not (equal element (car lst))))
    (setq lst (cdr lst)
          idx (1+ idx)))
  (if lst idx nil))
;;; FIND-POSITION-REAL
;;; Trova posizione numero reale con tolleranza
;;; Argomenti:
;;;   element - Numero da cercare
;;;   lst - Lista numeri
;;;   tolerance - Tolleranza (default 1e-6)
;;; Ritorna: Posizione o nil
(defun find-position-real (element lst tolerance / n position)
  (if (null tolerance) (setq tolerance 1e-6))
  (setq n 0 position nil)
  (foreach el lst
    (if (equal el element tolerance)
      (setq position n))
    (setq n (1+ n)))
  position)
;;; FIND-MIN
;;; Trova valore minimo in lista numerica
;;; Argomenti:
;;;   lst - Lista numeri
;;; Ritorna: Valore minimo
(defun find-min (lst)
  (apply 'min lst))
;;; FIND-MAX
;;; Trova valore massimo in lista numerica
;;; Argomenti:
;;;   lst - Lista numeri
;;; Ritorna: Valore massimo
(defun find-max (lst)
  (apply 'max lst))
;;; FIND-MAX-POSITIVE
;;; Trova massimo tra numeri positivi
;;; Argomenti:
;;;   lst - Lista numeri
;;; Ritorna: Massimo positivo o 0
(defun find-max-positive (lst / max-val)
  (setq max-val 0)
  (foreach val lst
    (if (> val max-val)
      (setq max-val val)))
  max-val)  
;;;=============================================================================
;;; 4. RIMOZIONE ELEMENTI
;;;=============================================================================
;;; REMOVE-AT-POSITION
;;; Rimuove elemento in posizione specifica
;;; Argomenti:
;;;   position - Posizione da rimuovere
;;;   lst - Lista
;;; Ritorna: Nuova lista senza elemento
(defun remove-at-position (position lst / n result)
  (setq n 0 result nil)
  (foreach el lst
    (if (/= position n)
      (setq result (cons el result)))
    (setq n (1+ n)))
  (reverse result))
;;; REMOVE-ELEMENT
;;; Rimuove ultima occorrenza elemento
;;; Argomenti:
;;;   element - Elemento da rimuovere
;;;   lst - Lista
;;; Ritorna: Nuova lista
(defun remove-element (element lst)
  (remove-at-position (find-position element lst) lst))
;;; REMOVE-ELEMENT-ALL
;;; Rimuove tutte le occorrenze elemento
;;; Argomenti:
;;;   element - Elemento da rimuovere
;;;   lst - Lista
;;; Ritorna: Nuova lista
(defun remove-element-all (element lst / result)
  (setq result nil)
  (foreach el lst
    (if (/= el element)
      (setq result (cons el result))))
  (reverse result))
;;; REMOVE-DUPLICATES
;;; Rimuove duplicati dalla lista
;;; Argomenti:
;;;   lst - Lista con possibili duplicati
;;; Ritorna: Lista senza duplicati
(defun remove-duplicates (lst / result)
  (setq result '())
  (foreach item lst
    (if (not (member item result))
      (setq result (cons item result))
    )
  )
  (reverse result)
)
;;;=============================================================================
;;; 5. ESTRAZIONE SOTTOLISTE
;;;=============================================================================
;;; EXTRACT-SUBLIST
;;; Estrae sottolista da posizioni specifiche
;;; Argomenti:
;;;   positions - Lista posizioni (come stringhe)
;;;   lst - Lista sorgente
;;; Ritorna: Sottolista
(defun extract-sublist (positions lst / result)
  (setq result nil)
  (foreach pos positions
    (setq result (append result (list (nth (atoi pos) lst)))))
  result)
;;; EXTRACT-BY-CONDITION
;;; Estrae elementi che soddisfano condizione
;;; Argomenti:
;;;   test-str - Stringa da cercare
;;;   lst - Lista stringhe
;;; Ritorna: Lista elementi che contengono test-str
(defun extract-by-condition (test-str lst / result)
  (setq result nil)
  (foreach el lst
    (if (string-search test-str el)
      (setq result (append-element el result))))
  result)
;;; CUT-LIST-AT-ELEMENT
;;; Taglia lista dall'elemento in poi
;;; Argomenti:
;;;   element - Elemento da cui tagliare
;;;   lst - Lista
;;; Ritorna: Sottolista da element in poi
(defun cut-list-at-element (element lst / result save)
  (setq result nil save nil)
  (foreach el lst
    (if save
      (setq result (cons el result)))
    (if (and (listp el) (or (= (length el) 2) (= (length el) 3)))
      (if (= (distance el element) 0)
        (setq save T))
      (if (= el element)
        (setq save T))))
  (if save
    (reverse result)
    lst))
;;; EXTRACT-NTH-FROM-SUBLISTS
;;; Estrae n-esimo elemento da lista di liste
;;; Argomenti:
;;;   position - Posizione da estrarre
;;;   lst - Lista di liste
;;; Ritorna: Lista di elementi estratti
(defun extract-nth-from-sublists (position lst / result)
  (setq result nil)
  (foreach sub-lst lst
    (setq result (append result (list (nth position sub-lst)))))
  result)
;;; EXTRACT-CARS
;;; Estrae primi elementi da lista di coppie
;;; Argomenti:
;;;   lst - Lista di coppie (a . b)
;;; Ritorna: Lista dei CAR
(defun extract-cars (lst / result)
  (setq result nil)
  (foreach pair lst
    (setq result (append result (list (car pair)))))
  result)
;;; EXTRACT-CDRS
;;; Estrae secondi elementi da lista di coppie
;;; Argomenti:
;;;   lst - Lista di coppie (a . b)
;;; Ritorna: Lista dei CDR
(defun extract-cdrs (lst / result)
  (setq result nil)
  (foreach pair lst
    (setq result (append result (list (cdr pair)))))
  result)
;;;=============================================================================
;;; 6. ORDINAMENTO
;;;=============================================================================
;;; SORT-STRING-LIST
;;; Ordina lista di stringhe alfabeticamente
;;; Argomenti:
;;;   lst - Lista stringhe
;;; Ritorna: Lista ordinata
(defun sort-string-list (lst)
  (acad_strlsort lst))
;;; SORT-NUMBER-LIST
;;; Ordina lista di numeri
;;; Argomenti:
;;;   lst - Lista numeri
;;; Ritorna: Lista ordinata
(defun sort-number-list (lst / sorted result min-val pos)
  (setq sorted lst result nil)
  (repeat (length lst)
    (setq min-val (find-min sorted)
          pos (find-position min-val sorted))
    (if (= result nil)
      (setq result (list min-val))
      (setq result (append-element min-val result)))
    (setq sorted (remove-at-position pos sorted)))
  result)
;;; SORT-STRING-NUMBER-LIST
;;; Ordina lista di stringhe numeriche
;;; Argomenti:
;;;   lst - Lista stringhe rappresentanti numeri
;;; Ritorna: Lista ordinata
(defun sort-string-number-list (lst / num-list result)
  (setq num-list (sort-number-list (string-list-to-number-list lst))
        result nil)
  (foreach num num-list
    (foreach str lst
      (if (= num (atof str))
        (setq result (cons str result)))))
  (reverse result))
;;; SORT-PAIR-LIST
;;; Ordina lista di coppie (numero . valore)
;;; Argomenti:
;;;   lst - Lista di coppie
;;; Ritorna: Lista ordinata per numero
(defun sort-pair-list (lst / keys sorted result)
  (setq keys nil)
  (foreach pair lst
    (setq keys (cons (car pair) keys)))
  (setq sorted (sort-number-list keys)
        result nil)
  (foreach key sorted
    (if (assoc key lst)
      (setq result (cons (assoc key lst) result))))
  (reverse result))
;;;=============================================================================
;;; 7. CONTEGGIO E STATISTICHE
;;;=============================================================================
;;; COUNT-OCCURRENCES
;;; Conta occorrenze elementi
;;; Argomenti:
;;;   lst - Lista elementi
;;; Ritorna: Lista associativa ((elemento . conteggio) ...)
;;; Esempio:
;;;   (count-occurrences '("A" "B" "A" "C")) => (("A" . 2) ("B" . 1) ("C" . 1))
(defun count-occurrences (lst / sorted prev count result)
  (if lst
    (setq sorted (reverse (cons nil (acad_strlsort lst)))))
  (setq count 1 result nil)
  (foreach el sorted
    (if prev
      (if (/= el prev)
        (progn
          (setq result (cons (cons prev count) result))
          (setq count 1))
        (setq count (1+ count))))
    (setq prev el))
  result)
;;; COUNT-OCCURRENCES-PAIRS
;;; Conta e somma occorrenze per coppie (stringa . numero)
;;; Argomenti:
;;;   lst - Lista coppie (stringa . numero)
;;; Ritorna: Lista associativa con somme
;;; Esempio:
;;;   (count-occurrences-pairs '(("A" . 2) ("B" . 3) ("A" . 4)))
;;;   => (("A" . 6) ("B" . 3))
(defun count-occurrences-pairs (lst / keys unique-keys result total)
  (setq keys nil result nil)
  (foreach pair lst
    (setq keys (cons (car pair) keys)))
  (setq unique-keys (remove-duplicates keys))
  (foreach key unique-keys
    (setq total 0)
    (foreach pair lst
      (if (= key (car pair))
        (setq total (+ total (cdr pair)))))
    (setq result (cons (cons key total) result)))
  result)
;;;=============================================================================
;;; 8. CONVERSIONE TIPI
;;;=============================================================================
;;; STRING-LIST-TO-NUMBER-LIST
;;; Converte lista stringhe in lista numeri
;;; Argomenti:
;;;   lst - Lista stringhe numeriche
;;; Ritorna: Lista numeri (int o real)
;;; Esempio:
;;;   (string-list-to-number-list '("10" "20.5" "30")) => (10 20.5 30.0)
(defun string-list-to-number-list (lst / result)
  (setq result nil)
  (foreach el lst
    (if (= (atoi el) (atof el))
      (setq result (cons (atoi el) result))
      (setq result (cons (atof el) result))))
  (reverse result))
;;; NUMBER-LIST-TO-STRING-LIST
;;; Converte lista numeri in lista stringhe
;;; Argomenti:
;;;   lst - Lista numeri
;;; Ritorna: Lista stringhe
;;; Esempio:
;;;   (number-list-to-string-list '(10 20.5 30)) => ("10" "20.5" "30")
(defun number-list-to-string-list (lst / result)
  (setq result nil)
  (foreach el lst
    (if (= (type el) 'int)
      (setq result (cons (itoa el) result))
      (setq result (cons (rtos el) result))))
  (reverse result))
;;; REAL-LIST-TO-INT-LIST
;;; Converte lista real in lista integer
;;; Argomenti:
;;;   lst - Lista numeri reali
;;; Ritorna: Lista numeri interi (troncati)
;;; Esempio:
;;;   (real-list-to-int-list '(10.7 20.3 30.9)) => (10 20 30)
(defun real-list-to-int-list (lst / result)
  (setq result nil)
  (foreach r lst
    (setq result (cons (fix r) result)))
  (reverse result))
;;;=============================================================================
;;; 9. INPUT/OUTPUT FILE
;;;=============================================================================
;;; LOAD-LIST-FROM-FILE
;;; Carica lista da file di testo (una riga = un elemento)
;;; Argomenti:
;;;   filepath - Percorso file
;;; Ritorna: Lista stringhe o nil
(defun load-list-from-file (filepath / file line result)
  (setq result nil)
  (if (setq file (open filepath "r"))
    (progn
      (while (setq line (read-line file))
        (setq result (cons line result)))
      (close file)
      (reverse result))
    nil))
;;; SAVE-LIST-TO-FILE
;;; Salva lista in file di testo (una riga per elemento)
;;; Argomenti:
;;;   lst - Lista stringhe
;;;   filepath - Percorso file
;;; Ritorna: T se successo
(defun save-list-to-file (lst filepath / file)
  (if (setq file (open filepath "w"))
    (progn
      (foreach el lst
        (write-line el file))
      (close file)
      T)
    nil))
;;;=============================================================================
;;; 10. LISTE ASSOCIATIVE (FUNZIONI SEPARATE)
;;;=============================================================================
;;; ALIST-GET
;;; Ottiene valore da lista associativa
;;; Argomenti:
;;;   key - Chiave da cercare
;;;   alist - Lista associativa
;;; Ritorna: Valore associato o nil
;;; Esempio:
;;;   (alist-get 'nome '((nome . "Mario") (eta . 30))) => "Mario"
(defun alist-get (key alist)
  (cdr (assoc key alist)))
;;; ALIST-PUT
;;; Aggiunge/modifica coppia in lista associativa
;;; Argomenti:
;;;   key - Chiave
;;;   value - Valore
;;;   alist - Lista associativa
;;; Ritorna: Nuova lista associativa
;;; Esempio:
;;;   (alist-put 'nome "Luca" '((nome . "Mario") (eta . 30)))
;;;   => ((nome . "Luca") (eta . 30
(defun alist-put (key value alist / pair)
  (setq pair (assoc key alist))
  (if pair
    (subst (cons key value) pair alist)
    (append alist (list (cons key value)))))
;;; ALIST-REMOVE
;;; Rimuove coppia da lista associativa
;;; Argomenti:
;;;   key - Chiave da rimuovere
;;;   alist - Lista associativa
;;; Ritorna: Nuova lista associativa
;;; Esempio:
;;;   (alist-remove 'eta '((nome . "Mario") (eta . 30)))
;;;   => ((nome . "Mario"))
(defun alist-remove (key alist / pair result)
  (setq pair (assoc key alist)
        result nil)
  (if pair
    (progn
      (foreach item alist
        (if (not (equal item pair))
          (setq result (append result (list item)))))
      result)
    alist))
;;; ALIST-HAS-KEY
;;; Verifica se chiave esiste
;;; Argomenti:
;;;   key - Chiave da cercare
;;;   alist - Lista associativa
;;; Ritorna: T se esiste, nil altrimenti
;; Esempio:
;;;   (alist-has-key 'nome '((nome . "Mario") (eta . 30))) => T
(defun alist-has-key (key alist)
  (if (assoc key alist) T nil))
;;; ALIST-KEYS
;;; Estrae tutte le chiavi
;;; Argomenti:
;;;   alist - Lista associativa
;;; Ritorna: Lista chiavi
;;; Esempio:
;;;   (alist-keys '((nome . "Mario") (eta . 30))) => (nome eta)
(defun alist-keys (alist)
  (extract-cars alist))
;;; ALIST-VALUES
;;; Estrae tutti i valori
;;; Argomenti:
;;;   alist - Lista associativa
;;; Ritorna: Lista valori
;;; Esempio:
;;;   (alist-values '((nome . "Mario") (eta . 30))) => ("Mario" 30)
(defun alist-values (alist)
  (extract-cdrs alist))
;;; ALIST-MERGE
;;; Unisce due liste associative (seconda sovrascrive prima)
;;; Argomenti:
;;;   alist1 - Prima lista
;;;   alist2 - Seconda lista (prioritaria)
;;; Ritorna: Lista unita
;;; Esempio:
;;;   (alist-merge '((nome . "Mario") (eta . 30)) '((eta . 31) (citta . "Roma")))
;;;   => ((nome . "Mario") (eta . 31) (citta . "Roma"))
(defun alist-merge (alist1 alist2 / result pair)
  (setq result alist1)
  (foreach pair alist2
    (setq result (alist-put (car pair) (cdr pair) result)))
  result)

;;;=============================================================================
;;; FINE FILE
;;;=============================================================================