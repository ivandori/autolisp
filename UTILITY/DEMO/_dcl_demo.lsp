;;;=============================================================================
;;; 6. FUNZIONI DIMOSTRATIVE
;;;=============================================================================
(setq path_ini "c:\\LISP2025\\UTILITY\\" ;_ modificare se diverso
      path_dcl path_ini
      path_sld path_ini
)


(defun c:DEMO-DCL ()

  (defun disegna-grafica (nome-chiave)
    (start_image nome-chiave)
    (setq dimx (dimx_tile nome-chiave))
    (setq dimy (dimy_tile nome-chiave))

    ;; Sfondo blu
    (fill_image 0 0 dimx dimy 1)

    ;; Rettangolo rosso
    (fill_image 5 5 (- dimx 10) (- dimy 10) 2)

    ;; Cerchio verde (approssimato)
    (setq centroX (/ dimx 2))
    (setq centroY (/ dimy 2))
    (setq raggio (/ (min dimx dimy) 4))

    (setq angolo 0)
    (repeat 360
      (setq x1 (+ centroX (* raggio (cos angolo))))
      (setq y1 (+ centroY (* raggio (sin angolo))))
      (setq x2 (+ centroX (* raggio (cos (+ angolo 0.1)))))
      (setq y2 (+ centroY (* raggio (sin (+ angolo 0.1)))))
      (vector_image (fix x1) (fix y1) (fix x2) (fix y2) 3)
      (setq angolo (+ angolo 0.1))
    )

    (end_image)
    (set_tile "txt_status" "Stato: Grafica disegnata")
  )

  (setq dcl_id (load_dialog (strcat path_dcl "demo_dcl.dcl")))

  (if (not (new_dialog "demo_dcl" dcl_id))
    (progn
      (princ "\nErrore nel caricamento del DCL")
      (exit)
    )
  )

  ;; Inizializza liste
  (setq	*lista_citta*
	 '("Milano"	"Roma"	     "Torino"	  "Napoli"
	   "Firenze"	"Bologna"    "Palermo"	  "Genova"
	  )
  )
  (setq	*lista_opzioni*
	 '("Opzione 1" "Opzione 2" "Opzione 3" "Opzione 4" "Opzione 5")
  )



  ;; Azioni per i pulsanti test
  (action_tile
    "btn_enable"
    "(enable-controls '(\"edit_nome\" \"edit_cognome\"))"
  )
  (action_tile
    "btn_disable"
    "(disable-controls '(\"edit_nome\" \"edit_cognome\"))"
  )
(action_tile
    "img_enable"
    "(toggle-image \"simple_img\" T)"
  )
  (action_tile
    "img_disable"
    "(toggle-image \"simple_img\" nil)"
  )  
  (action_tile
    "btn_select"
    "(select-controls '(\"check_attivo\"))"
  )
  (action_tile
    "btn_deselect"
    "(deselect-controls '(\"check_attivo\"))"
  )
  (action_tile
    "btn_sel_radio_a"
    "(select-controls '(\"radio_a\")) (deselect-controls '(\"radio_b\"))"
  )
  (action_tile
    "btn_sel_radio_b"
    "(select-controls '(\"radio_b\")) (deselect-controls '(\"radio_a\"))"
  )

  (action_tile
    "btn_fill_citta"
    "(insert-list-items \"lista_citta\" *lista_citta*)"
  )
  (action_tile
    "btn_sel_roma"
    "(set-list-selection \"lista_citta\" \"Roma\" *lista_citta*)"
  )

  (action_tile
    "btn_fill_opzioni"
    "(insert-list-items \"lista_opzioni\" *lista_opzioni*)"
  )
  (action_tile
    "btn_sel_multi"
    "(set-multi-selection \"lista_opzioni\" '(0 2 4))"
  )

  (action_tile
    "btn_clear_citta"
    "(insert-list-items \"lista_citta\" nil)"
  )
  (action_tile
    "btn_clear_opzioni"
    "(insert-list-items \"lista_opzioni\" nil)"
  )

  ;; Immagine di esempio
  (disegna-grafica "slide")
  


  ;; Azioni standard
  (action_tile "accept" "(get_values) (done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (defun get_values ()
    (setq nome (get_tile "edit_nome"))
    (setq cognome (get_tile "edit_cognome"))
    (setq attivo (get_tile "check_attivo"))
    (setq radio_a (get_tile "radio_a"))
    (setq radio_b (get_tile "radio_b"))
    (setq citta_idx (get_tile "lista_citta"))
    (setq opzioni_idx (get_tile "lista_opzioni"))

    (princ (strcat "\nNome: " nome))
    (princ (strcat "\nCognome: " cognome))
    (princ (strcat "\nAttivo: " attivo))
    (princ (strcat "\nRadio A: " radio_a " Radio B: " radio_b))
    (princ (strcat "\nCittà selezionata: "
		   (if citta_idx
		     (nth (atoi citta_idx) *lista_citta*)
		     "Nessuna"
		   )
	   )
    )
    (princ (strcat "\nOpzioni selezionate: " opzioni_idx))
  )

  (start_dialog)
  (unload_dialog dcl_id)

  (princ)
)

(defun c:DEMO-DCL-INI (/ result)
  (defun @accept ()
    ;; --- lettura selezioni
    (setq nome	  (get_tile "edit_nome")
	  cognome (get_tile "edit_cognome")
    )
    (setq singole (get-list-selection "lista_citta" citta))
    (setq multiple (get-multi-selection "lista_opzioni" opzioni))

    ;; --- salvataggio controlli
    (save-dcl-ini
      (strcat path_ini "demo_dcl.ini")
      '("[Dati]"	 "edit_nome"	  "edit_cognome"
	"[Opzioni]"	 "check_attivo"	  "radio_a"
	"radio_b"
       )
    )
    (done_dialog 1)
  )
  (setq dialog-name "demo_dcl")
  (setq dcl (strcat dialog-name ".dcl"))
  (setq dcl-id (load_dialog (strcat path_dcl dcl)))
  (if (not (new_dialog dialog-name dcl-id))
    (exit)
  )

  ;; --- inizializzazione liste
  (setq	citta	'("Milano" "Roma" "Torino" "Bologna")
	opzioni	'("Opzione 1" "Opzione 2" "Opzione 3")
  )
  (populate-lists
    '("lista_citta" "lista_opzioni")
    (list citta opzioni)
  )
  ;; --- carica valori da INI nei controlli
  (if (findfile (strcat path_ini "demo_dcl.ini"))
    (load-dcl-ini (strcat path_ini "demo_dcl.ini"))
  )

  ;; --- carica variabili AutoLISP
  (load-variables-ini (strcat path_ini "demo.ini"))

  ;; --- selezioni iniziali
  (set-list-selection "lista_citta" "Roma" citta)

  ;; --- slide demo (se esiste)
  (if (findfile "demo.sld")
    (display-slide "demo.sld")
    (clear-slide 250)
  )


  (action_tile "accept" "(@accept)")
  (action_tile
    "cancel"
    "(done_dialog 0)"
  )

  (setq result (start_dialog))
  (unload_dialog dcl-id)

  (if (= result 1)
    (progn
      ;; --- output di verifica
      
	(strcat
	  "Città scelta: "
	  singole
	  "\n"
	  "Opzioni: "
	  (princ-to-string multiple)
	  "\n"
	)
      
    )
  )
)


;;; DEMO-FILE-INI
;;; Dimostra operazioni su file INI
(defun c:demo-ini (/ ini-file)
  (princ "\n=== DEMO GESTIONE FILE INI ===")
  (setq ini-file (get-temp-file-path "demo_config.ini"))
  (princ (strcat "\n\nFile INI: " ini-file))
  (princ "\n\n1. Creazione variabili di test...")
  (setq	altezza@r 2.5
	larghezza@r
	 3.0
	contatore@i
	 10
	attivo@b T
	nome "Mario Rossi"
	cognome	"Test"
  )
  (princ "\n   Variabili create:")
  (princ (strcat "\n   altezza@r = " (rtos altezza@r)))
  (princ (strcat "\n   larghezza@r = " (rtos larghezza@r)))
  (princ (strcat "\n   contatore@i = " (itoa contatore@i)))
  (princ (strcat "\n   attivo@b = "
		 (if attivo@b
		   "T"
		   "nil"
		 )
	 )
  )
  (princ (strcat "\n   nome = " nome))
  (princ "\n\n2. Salvataggio in file INI...")
  (save-variables-ini
    ini-file
    '("[Dimensioni]"	"altezza@r"	  "larghezza@r"
      "[Contatori]"	"contatore@i"	  "[Stato]"
      "attivo@b"	"[Anagrafica]"	  "nome"
     )
  )
  (princ " OK")
  (princ "\n\n3. Modifica variabili...")
  (setq	altezza@r 999.0
	contatore@i
	 0
	attivo@b nil
	nome "MODIFICATO"
  )
  (princ "\n   Variabili modificate temporaneamente")
  (princ "\n\n4. Ricaricamento da file INI...")
  (load-variables-ini ini-file)
  (princ " OK")
  (princ "\n\n5. Verifica valori ripristinati:")
  (princ (strcat "\n   altezza@r = " (rtos altezza@r)))
  (princ (strcat "\n   contatore@i = " (itoa contatore@i)))
  (princ (strcat "\n   attivo@b = "
		 (if attivo@b
		   "T"
		   "nil"
		 )
	 )
  )
  (princ (strcat "\n   nome = " nome))
  (princ "\n\n6. Pulizia file temporaneo...")
  (delete-file ini-file)
  (princ " OK")
  (princ "\n\n=== Demo completata ===")
  (princ)
)
(defun demo-dcl-banner ()
  (princ
    (strcat
      "\n=================================================="
      "\n DEMO DCL / INI - LISP2025"
      "\n=================================================="
      "\nComandi disponibili:"
      "\n"
      "\n  DEMO-DCL        ? Dimostrazione completa controlli DCL"
      "\n  DEMO-DCL-INI    ? Demo DCL con salvataggio/caricamento INI"
      "\n  DEMO-INI        ? Demo gestione variabili e file INI"
      "\n"
      "\nPercorsi attivi:"
      "\n  DCL : " path_dcl
      "\n  INI : " path_ini
      "\n  SLD : " path_sld
      "\n"
      "\nNote:"
      "\n - Il file demo_dcl.dcl deve essere presente"
      "\n - Le immagini sono disegnate via start_image"
      "\n - I valori vengono letti SOLO prima di done_dialog"
      "\n"
      "\n==================================================\n"
    )
  )
  (princ)
)
(defun demo-dcl-banner ()
  (princ
    (strcat
      "\n=================================================="
      "\n DEMO DCL / INI - LISP2025"
      "\n=================================================="
      "\nComandi disponibili:"
      "\n"
      "\n  DEMO-DCL        ? Dimostrazione completa controlli DCL"
      "\n  DEMO-DCL-INI    ? Demo DCL con salvataggio/caricamento INI"
      "\n  DEMO-INI        ? Demo gestione variabili e file INI"
      "\n"
      "\nPercorsi attivi:"
      "\n  DCL : " path_dcl
      "\n  INI : " path_ini
      "\n  SLD : " path_sld
      "\n"
      "\nNote:"
      "\n - Il file demo_dcl.dcl deve essere presente"
      "\n - Le immagini sono disegnate via start_image"
      "\n - I valori vengono letti SOLO prima di done_dialog"
      "\n"
      "\n==================================================\n"
    )
  )
  (princ)
)
(demo-dcl-banner)
