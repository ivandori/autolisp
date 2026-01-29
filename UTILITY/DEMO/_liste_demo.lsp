;;;=============================================================================
;;; 11. FUNZIONI DIMOSTRATIVE
;;;=============================================================================
;;; DEMO-LIST-BASIC
;;; Dimostra operazioni base liste
(defun c:demo-list-basic (/ lst pos)
  (princ "\n=== DEMO OPERAZIONI BASE LISTE ===")
  (setq lst '("A" "B" "C" "D" "E"))
  (princ (strcat "\n\nLista iniziale: " (list-to-string lst " ")))
  (princ "\n\n1. Aggiungi elemento in coda...")
  (setq lst (append-element "F" lst))
  (princ (strcat "\n   Risultato: " (list-to-string lst " ")))
  (princ "\n\n2. Inserisci 'X' in posizione 2...")
  (setq lst (insert-at-position "X" lst 2))
  (princ (strcat "\n   Risultato: " (list-to-string lst " ")))
  (princ "\n\n3. Trova posizione 'C'...")
  (setq pos (find-position "C" lst))
  (princ (strcat "\n   Posizione: " (itoa pos)))
  (princ "\n\n4. Rimuovi elemento in posizione 2...")
  (setq lst (remove-at-position 2 lst))
  (princ (strcat "\n   Risultato: " (list-to-string lst " ")))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LIST-SORT
;;; Dimostra ordinamento
(defun c:demo-list-sort (/ lst-str lst-num)
  (princ "\n=== DEMO ORDINAMENTO LISTE ===")
  (setq lst-str '("Zebra" "Alfa" "Bravo" "Delta")
        lst-num '(42 7 99 15 3))
  (princ (strcat "\n\nLista stringhe: " (list-to-string lst-str " ")))
  (princ (strcat "\nOrdinata: " 
                (list-to-string (sort-string-list lst-str) " ")))
  (princ (strcat "\n\nLista numeri: " 
                (list-to-string (number-list-to-string-list lst-num) " ")))
  (princ (strcat "\nOrdinata: " 
                (list-to-string 
                  (number-list-to-string-list (sort-number-list lst-num)) " ")))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LIST-COUNT
;;; Dimostra conteggio
(defun c:demo-list-count (/ lst counts)
  (princ "\n=== DEMO CONTEGGIO OCCORRENZE ===")
  (setq lst '("A" "B" "A" "C" "B" "A"))
  (princ (strcat "\n\nLista: " (list-to-string lst " ")))
  (setq counts (count-occurrences lst))
  (princ "\n\nConteggio:")
  (foreach pair counts
    (princ (strcat "\n  " (car pair) ": " (itoa (cdr pair)))))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LIST-ALIST
;;; Dimostra liste associative
(defun c:demo-list-alist (/ alist value)
  (princ "\n=== DEMO LISTE ASSOCIATIVE ===")
  (setq alist '(("nome" . "Mario") ("eta" . 30) ("citta" . "Milano")))
  (princ "\n\nLista associativa iniziale:")
  (foreach pair alist
    (princ (strcat "\n  " (car pair) " = " 
                  (if (numberp (cdr pair)) 
                    (itoa (cdr pair)) 
                    (cdr pair)))))
  (princ "\n\n1. Ottieni valore 'nome'...")
  (setq value (alist-get "nome" alist))
  (princ (strcat "\n   Valore: " value))
  (princ "\n\n2. Modifica 'eta' a 31...")
  (setq alist (alist-put "eta" 31 alist))
  (princ (strcat "\n   Nuova eta: " (itoa (alist-get "eta" alist))))
  (princ "\n\n3. Aggiungi 'telefono'...")
  (setq alist (alist-put "telefono" "123-456" alist))
  (princ "\n   Lista aggiornata:")
  (foreach pair alist
    (princ (strcat "\n  " (car pair) " = " 
                  (if (numberp (cdr pair)) 
                    (itoa (cdr pair)) 
                    (cdr pair)))))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-TUTTO-LIST
;;; Esegue tutte le demo liste
(defun c:demo-list ()
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|  GESTIONE LISTE - DIMOSTRAZIONE COMPLETA              |")
  (princ "\n+========================================================+")
  (princ "\n")
  (c:demo-list-basic)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-list-sort)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-list-count)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-list-alist)
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|         TUTTE LE DEMO LISTE COMPLETATE                |")
  (princ "\n+========================================================+")
  (princ))
