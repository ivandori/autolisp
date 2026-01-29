;;;=============================================================================
;;; 8. FUNZIONI DIMOSTRATIVE
;;;=============================================================================
;;; DEMO-LINE-CREATION
;;; Dimostra creazione linee
(defun c:demo-line-create (/ pt1 pt2 line-ent)
  (princ "\n=== DEMO CREAZIONE LINEE ===")
  (princ "\n\n1. Imposta default linea...")
  (set-line-defaults)
  (princ " OK")
  (princ "\n2. Crea linea programmaticamente...")
  (setq pt1 '(0 0)
        pt2 '(100 100))
  (setq line-ent (create-line pt1 pt2 "0" 1))
  (princ (strcat "\n   Linea creata da " (punto-a-stringa pt1)
                " a " (punto-a-stringa pt2)))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LINE-INTERSECTION
;;; Dimostra calcolo intersezioni
(defun c:demo-line-inter-old (/ line1 line2 int-pt)
  (princ "\n=== DEMO INTERSEZIONE LINEE ===")
  (setq line1 (list '(0 0) '(100 100))
        line2 (list '(0 100) '(100 0)))
  (princ "\n\nLinea 1: (0,0) -> (100,100)")
  (princ "\nLinea 2: (0,100) -> (100,0)")
  (setq int-pt (calc-line-intersection line1 line2))
  (if int-pt
    (progn
      (princ (strcat "\n\nPunto intersezione: " (punto-a-stringa int-pt)))
      (command "_.circle" int-pt 5))
    (princ "\n\nNessuna intersezione trovata"))
  (princ "\n\n=== Demo completata ===")
  (princ))
(defun c:demo-line-inter (/ line1 line2 int-pt)
  (princ "\n=== DEMO INTERSEZIONE LINEE ===")
  (setq line1 (list '(0 0) '(100 100))
        line2 (list '(0 100) '(100 0)))
  (princ "\n\nLinea 1: (0,0) -> (100,100)")
  (princ "\nLinea 2: (0,100) -> (100,0)")
  (setq int-pt (calc-line-intersection line1 line2))
  (if int-pt
    (progn
      (princ (strcat "\n\nPunto intersezione: " (punto-a-stringa int-pt)))
      ; CORREZIONE: passa le coordinate X e Y separatamente
      (command "_.circle" (list (car int-pt) (cadr int-pt)) "5"))
    (princ "\n\nNessuna intersezione trovata"))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LINE-TRIM
;;; Dimostra taglio linee
(defun c:demo-line-trim (/ line1 line2)
  (princ "\n=== DEMO TAGLIO LINEE ===")
  (princ "\n\nSeleziona prima linea (da tagliare)...")
  (setq line1 (select-line-interactive "\nLinea 1: "))
  (if line1
    (progn
      (princ "\nSeleziona seconda linea (riferimento)...")
      (setq line2 (select-line-interactive "\nLinea 2: "))
      (if line2
        (progn
          (princ "\n\nEsecuzione taglio...")
          (trim-two-lines line1 line2)
          (princ " OK"))
        (princ "\nLinea 2 non selezionata")))
    (princ "\nLinea 1 non selezionata"))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LINE-BREAK
;;; Dimostra interruzione linea
(defun c:demo-line-break (/ pt)
  (princ "\n=== DEMO INTERRUZIONE LINEA ===")
  (princ "\n\nClicca su una linea per creare interruzione...")
  (setq pt (getpoint))
  (if pt
    (progn
      (princ "\n\nCreazione interruzione di 10 unita'...")
      (if (break-line-at-point pt 10.0)
        (princ " OK")
        (princ " ERRORE")))
    (princ "\nPunto non selezionato"))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LINE-JOIN
;;; Dimostra unione linee
(defun c:demo-line-join (/ pt1 pt2)
  (princ "\n=== DEMO UNIONE LINEE ===")
  (princ "\n\nSeleziona primo punto area...")
  (setq pt1 (getpoint))
  (if pt1
    (progn
      (princ "\nSeleziona secondo punto area...")
      (setq pt2 (getpoint pt1))
      (if pt2
        (progn
          (princ "\n\nUnione linee in area...")
          (if (join-lines pt1 pt2)
            (princ " OK")
            (princ " ERRORE")))
        (princ "\nSecondo punto non selezionato")))
    (princ "\nPrimo punto non selezionato"))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LINE-CONNECT
;;; Dimostra unione linee
(defun c:demo-line-connect (/ pt1 pt2)
  (princ "\n=== DEMO CONNESSIONE LINEE ===")
  (princ "\n\nSeleziona primo punto area...")
  (setq pt1 (getpoint))
  (if pt1
    (progn
      (princ "\nSeleziona secondo punto area...")
      (setq pt2 (getpoint pt1))
      (if pt2
        (progn
          (princ "\n\Connessione linee in area...")
          (if (connect-lines-with-segment pt1 pt2)
            (princ " OK")
            (princ " ERRORE")))
        (princ "\nSecondo punto non selezionato")))
    (princ "\nPrimo punto non selezionato"))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-TUTTO-LINE
;;; Esegue tutte le demo linee
(defun c:demo-line ()
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|  GESTIONE LINEE - DIMOSTRAZIONE COMPLETA              |")
  (princ "\n+========================================================+")
  (princ "\n")
  (c:demo-line-create)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-line-inter)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-line-trim)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-line-break)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-line-join)
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|         TUTTE LE DEMO LINEE COMPLETATE                |")
  (princ "\n+========================================================+")
  (princ))
