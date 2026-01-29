;;;=============================================================================
;;; 7. FUNZIONI DIMOSTRATIVE
;;;=============================================================================
;;; DEMO-XDATA-BASE
;;; Dimostra operazioni base su XDATA
(defun c:demo-xdata-base (/ ent app-name)
  (princ "\n=== DEMO XDATA BASE ===")
  (princ "\n\nSeleziona un'entita' per aggiungere XDATA...")
  (setq ent (car (entsel)))
  (if ent
    (progn
      (setq app-name "DEMO_APP")
      (princ (strcat "\n\nApplicazione XDATA: " app-name))
      (princ "\n\n1. Aggiunta dati XDATA...")
      (set-xdata-string ent app-name "Valore Test")
      (set-xdata-integer ent (strcat app-name "_NUM") 42)
      (set-xdata-real ent (strcat app-name "_REAL") 3.14159)
      (princ " OK")
      (princ "\n\n2. Lettura dati XDATA...")
      (princ
	(strcat "\n   Stringa: " (get-xdata-string ent app-name))
      )
      (princ
	(strcat	"\n   Integer: "
		(itoa (get-xdata-integer ent (strcat app-name "_NUM")))
	)
      )
      (princ
	(strcat	"\n   Real: "
		(rtos (get-xdata-real ent (strcat app-name "_REAL")))
	)
      )
      (princ "\n\n3. Lista applicazioni XDATA presenti:")
      (foreach app (list-xdata-apps ent)
	(princ (strcat "\n   - " (princ-to-string app)))
      )
      (princ (strcat "\n\n4. Spazio XDATA disponibile: "
		     (itoa (xdroom ent))
		     " bytes"
	     )
      )
      (princ (strcat "\n   Spazio utilizzato: "
		     (itoa (xdata-size ent))
		     " bytes"
	     )
      )
      (princ "\n\n5. Rimozione XDATA...")
      (if (= (getstring "\n   Vuoi rimuovere XDATA? (S/n): ") "S")
	(progn
	  (remove-all-xdata ent)
	  (princ " OK - XDATA rimosso")
	)
	(princ " XDATA mantenuto")
      )
      (princ "\n\n=== Demo completata ===")
    )
    (princ "\nNessuna entita' selezionata!")
  )
  (princ)
)
;;; DEMO-COLLEGAMENTI-XDATA
;;; Dimostra collegamenti tra entità tramite XDATA
(defun c:demo-xdata-link (/ parent children app-name count)
  (princ "\n=== DEMO COLLEGAMENTI XDATA ===")
  (princ "\n\nSeleziona entita' PADRE...")
  (setq parent (car (entsel)))
  (if parent
    (progn
      (setq app-name "LINK_APP")
      (princ "\n\nOra seleziona entita' FIGLIO da collegare...")
      (princ "\n(Premi ESC per terminare selezione)")
      (setq children nil)
      (setq count 0)
      (while (setq child (car (entsel "\nSeleziona entita' figlio: ")))
	(link-entity-to-parent child parent app-name)
	(setq children (cons child children))
	(setq count (1+ count))
	(princ (strcat "\n" (itoa count) " entita' collegate"))
      )
      (if (> count 0)
	(progn
	  (princ
	    (strcat "\n\nTotale entita' collegate: " (itoa count))
	  )
	  (princ "\n\nRicerca entita' collegate...")
	  (setq
	    linked (search-entities-by-xdata parent nil nil app-name)
	  )
	  (princ
	    (strcat "\nTrovate " (itoa (length linked)) " entita'")
	  )
	  (if (= (getstring
		   "\n\nVuoi eliminare le entita' collegate? (S/n): "
		 )
		 "S"
	      )
	    (progn
	      (setq deleted (delete-linked-entities parent app-name))
	      (princ (strcat "\n" (itoa deleted) " entita' eliminate"))
	    )
	    (princ "\nEntita' non eliminate")
	  )
	)
	(princ "\n\nNessuna entita' collegata")
      )
      (princ "\n\n=== Demo completata ===")
    )
    (princ "\nNessuna entita' padre selezionata!")
  )
  (princ)
)
;;; DEMO-RICERCA-XDATA
;;; Dimostra ricerca entità con XDATA
(defun c:demo-xdata-search (/ app-name entities)
  (princ "\n=== DEMO RICERCA XDATA ===")
  (setq app-name (getstring "\n\nInserisci nome applicazione XDATA: "))
  (if (and app-name (/= app-name ""))
    (progn
      (princ (strcat "\n\nRicerca entita' con XDATA: " app-name))
      (setq entities (find-entities-with-xdata app-name nil))
      (if entities
	(progn
	  (princ
	    (strcat "\n\nTrovate " (itoa (length entities)) " entita':")
	  )
	  (foreach ent entities
	    (princ (strcat "\n  - "
			   (get-entity-type ent)
			   " [Handle: "
			   (get-entity-handle ent)
			   "]"
		   )
	    )
	  )
	  (princ "\n\nAnalisi prima entita':")
	  (setq first-ent (car entities))
	  (princ "\n  Applicazioni XDATA:")
	  (foreach app (list-xdata-apps first-ent)
	    (princ (strcat "\n    - " (princ-to-string app)))
	  )
	)
	(princ "\n\nNessuna entita' trovata")
      )
      (princ "\n\n=== Demo completata ===")
    )
    (princ "\nNome applicazione non valido!")
  )
  (princ)
)
;;; DEMO-TUTTO-XDATA
;;; Esegue tutte le demo XDATA
(defun c:demo-xdata ()
  (princ "\n")
  (princ
    "\n+========================================================+"
  )
  (princ
    "\n|  GESTIONE XDATA - DIMOSTRAZIONE COMPLETA              |"
  )
  (princ
    "\n+========================================================+"
  )
  (princ "\n")
  (c:demo-xdata-base)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-xdata-link)
  (princ "\n\nPremi Invio per continuare...")
  (getstring)
  (c:demo-xdata-search)
  (princ "\n")
  (princ
    "\n+========================================================+"
  )
  (princ
    "\n|         TUTTE LE DEMO XDATA COMPLETATE                |"
  )
  (princ
    "\n+========================================================+"
  )
  (princ)
)

;;;=============================================================================
;;; MESSAGGI CARICAMENTO
;;;=============================================================================
(princ
  "\n+========================================================+"
)
(princ
  "\n| UTILITA' XDATA - Caricato con successo                |"
)
(princ
  "\n+========================================================+"
)
(princ
  "\n| Comandi disponibili:                                   |"
)
(princ
  "\n|   DEMO-XDATA-BASE    - Demo operazioni base XDATA     |"
)
(princ
  "\n|   DEMO-XDATA-LINK    - Demo collegamenti entita'      |"
)
(princ
  "\n|   DEMO-XDATA-SEARCH  - Demo ricerca XDATA             |"
)
(princ
  "\n|   DEMO-XDATA         - Tutte le demo                  |"
)
(princ
  "\n+========================================================+"
)
(princ
  "\n| Funzioni principali:                                   |"
)
(princ
  "\n|   set-xdata-string, set-xdata-integer, set-xdata-real  |"
)
(princ
  "\n|   get-xdata-string, get-xdata-integer, get-xdata-real  |"
)
(princ
  "\n|   search-entities-by-xdata                             |"
)
(princ
  "\n|   link-entity-to-parent                                |"
)
(princ
  "\n|   delete-linked-entities                               |"
)
(princ
  "\n|   remove-xdata, remove-all-xdata                       |"
)
(princ
  "\n|   list-xdata-apps, has-xdata                           |"
)
(princ
  "\n+========================================================+"
)
(princ
  "\n| Codici XDATA principali:                               |"
)
(princ
  "\n|   1000=String  1040=Real  1070=Integer  1010=Point3D   |"
)
(princ
  "\n+========================================================+"
)
(princ
  "\n| Compatibile con: ProgeCAD, DraftSight, AutoCAD        |"
)
(princ
  "\n| AutoLISP puro - Nessuna dipendenza esterna            |"
)
(princ
  "\n+========================================================+"
)
(princ "\n\nDigita DEMO-XDATA per vedere tutte le demo")
(princ)
