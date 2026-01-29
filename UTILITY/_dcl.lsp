;;;==============================================================================
;;; GESTIONE DIALOG BOX DCL E FILE INI
;;; Ultimo aggiornamento: 12/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: [MIT License](https://opensource.org/licenses/MIT)
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: gestione finestre di dialogo DCL e file di configurazione INI
;;; Dipendenze esterne:  SPLIT-STRING STRING-LEFT-OF, STRING-RIGHT-OF (_stringhe.lsp)
;;;			 FIND-POSITION (_liste.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. GESTIONE FILE INI
;;; 2. GESTIONE DIALOG BOX DCL
;;; 3. LISTE E SELEZIONI DCL
;;; 4. GESTIONE IMMAGINI E SLIDE
;;;=============================================================================
;;; 1. GESTIONE FILE INI
;;;=============================================================================
;;; LOAD-DCL-INI
;;; Carica valori da file INI e li imposta nei controlli DCL
;;; Da usare all'interno di un dialogo DCL attivo
;;; Argomenti:
;;;   ini-file - Percorso file INI da leggere
;;; Ritorna: T se successo, nil se errore
;;; Formato file INI:
;;;   [Sezione Commento]
;;;   nome_controllo=valore
;;;   altro_controllo=altro_valore
;;; Esempio:
;;;   (load-dcl-ini "C:\\config\\impostazioni.ini")
(defun load-dcl-ini (ini-file / file line var val)
  (if (setq file (open ini-file "r"))
    (progn
      (while (setq line (read-line file))
        (if (/= (substr line 1 1) "[")
          (progn
            (setq var (string-left-of line "=")
                  val (string-right-of line "="))
            (if (/= var "")(set_tile var val)))))
      (close file)
      (setq file nil)
      T)
    nil))
;;; SAVE-DCL-INI
;;; Salva valori dei controlli DCL in file INI
;;; Da usare all'interno di un dialogo DCL attivo
;;; Argomenti:
;;;   ini-file - Percorso file INI da creare/sovrascrivere
;;;   var-list - Lista di nomi controlli DCL (stringhe)
;;; Ritorna: T
;;; Esempio:
;;;   (save-dcl-ini "C:\\config\\impostazioni.ini"
;;;     '("[Testo]" "edit_nome" "edit_cognome" 
;;;       "[Opzioni]" "check_opzione1" "radio_scelta"))
(defun save-dcl-ini (ini-file var-list / file var)
  (setq file (open ini-file "w"))
  (foreach var var-list
    (if (/= (substr var 1 1) "[")
      (write-line (strcat var "=" (get_tile var)) file)
      (write-line var file)))
  (close file)
  (setq file nil)
  T)
;;; LOAD-VARIABLES-INI
;;; Carica variabili da file INI e le setta in memoria AutoLISP
;;; Argomenti:
;;;   ini-file - Percorso file INI
;;; Ritorna: T se successo, nil se errore
;;; Formato variabili:
;;;   nome_var@i=123        -> Integer
;;;   nome_var@r=45.67      -> Real
;;;   nome_var@b=T          -> Boolean
;;;   nome_var=testo        -> String
;;; Esempio:
;;;   File INI:
;;;     altezza@r=2.5
;;;     contatore@i=10
;;;     attivo@b=T
;;;     nome=Mario
;;;   Dopo (load-variables-ini "file.ini"):
;;;     altezza -> 2.5
;;;     contatore -> 10
;;;     attivo -> T
;;;     nome -> "Mario"
(defun load-variables-ini (ini-file / file line var val var-len)
  (if (setq file (open ini-file "r"))
    (progn
      (while (setq line (read-line file))
        (if (/= (substr line 1 1) "[")
          (progn
            (setq var (string-left-of line "=")
                  val (string-right-of line "=")
                  var-len (strlen var))
            (cond
              ((and (> var-len 3) (= (substr var (1- var-len) 2) "@i"))
               (set (read var) (atoi val)))
              ((and (> var-len 3) (= (substr var (1- var-len) 2) "@r"))
               (set (read var) (atof val)))
              ((and (> var-len 3) (= (substr var (1- var-len) 2) "@b"))
               (set (read var) (read val)))
              (t
	       (if (/= var "")
	       (set (read var) val)
		)
	       )))))
      (close file)
      (setq file nil)
      T)
    nil))
;;; SAVE-VARIABLES-INI
;;; Salva variabili AutoLISP in file INI
;;; Argomenti:
;;;   ini-file - Percorso file INI da creare
;;;   var-list - Lista di nomi variabili (stringhe con suffisso tipo)
;;; Ritorna: T
;;; Esempio:
;;;   (save-variables-ini "C:\\config.ini"
;;;     '("[Dimensioni]" "altezza@r" "larghezza@r"
;;;       "[Contatori]" "numero@i" "attivo@b"
;;;       "[Testo]" "nome" "cognome"))
(defun save-variables-ini (ini-file var-list / file var var-len)
  (setq file (open ini-file "w"))
  (foreach var var-list
    (setq var-len (strlen var))
    (if (/= (substr var 1 1) "[")
      (cond
        ((and (> var-len 3) (= (substr var (1- (strlen var)) 2) "@i"))
         (write-line (strcat var "=" (itoa (eval (read var)))) file))
        ((and (> var-len 3) (= (substr var (1- (strlen var)) 2) "@r"))
         (write-line (strcat var "=" (rtos (eval (read var)))) file))
        ((and (> var-len 3) (= (substr var (1- (strlen var)) 2) "@b"))
         (if (eval (read var))
           (write-line (strcat var "=T") file)
           (write-line (strcat var "=nil") file)))
        (t (write-line (strcat var "=" (eval (read var))) file)))
      (write-line var file)))
  (close file)
  (setq file nil)
  T)
;;;=============================================================================
;;; 2. GESTIONE DIALOG BOX DCL
;;;=============================================================================

;;; SHOW-DIALOG
;;; Visualizza un dialog DCL base (solo visualizzazione)
;;; Argomenti:
;;;   dialog-name - Nome del dialog
;;; Esempio:
;;;   (show-dialog "info_dialog")
(defun show-dialog (dialog-name / dcl dcl-id)
  (setq dcl (strcat dialog-name ".dcl"))
  (setq dcl-id (load_dialog dcl))
  (if (not (new_dialog dialog-name dcl-id))
    (exit))
  (action_tile "accept" "(done_dialog)")
  (start_dialog)
  (unload_dialog dcl-id))
;;; ENABLE-CONTROLS
;;; Abilita una lista di controlli DCL
;;; Argomenti:
;;;   control-list - Lista di nomi controlli (stringhe)
;;; Esempio:
;;;   (enable-controls '("edit1" "button1" "check1"))
(defun enable-controls (control-list)
  (foreach ctrl control-list (mode_tile ctrl 0)))
;;; DISABLE-CONTROLS
;;; Disabilita una lista di controlli DCL
;;; Argomenti:
;;;   control-list - Lista di nomi controlli
;;; Esempio:
;;;   (disable-controls '("edit2" "button2"))
(defun disable-controls (control-list)
  (foreach ctrl control-list (mode_tile ctrl 1)))
;;; SELECT-CONTROLS
;;; Seleziona (check) una lista di controlli
;;; Argomenti:
;;;   control-list - Lista di nomi controlli checkbox/radio
;;; Esempio:
;;;   (select-controls '("check1" "check2" "radio1"))
(defun select-controls (control-list)
  (foreach ctrl control-list (set_tile ctrl "1")))
;;; DESELECT-CONTROLS
;;; Deseleziona (uncheck) una lista di controlli
;;; Argomenti:
;;;   control-list - Lista di nomi controlli
;;; Esempio:
;;;   (deselect-controls '("check1" "check2"))
(defun deselect-controls (control-list)
  (foreach ctrl control-list (set_tile ctrl "0")))
;;;=============================================================================
;;; 3. LISTE E SELEZIONI DCL
;;;=============================================================================
;;; POPULATE-LISTS
;;; Popola più list_box contemporaneamente
;;; Argomenti:
;;;   key-list - Lista di nomi list_box
;;;   data-lists - Lista di liste (una per ogni list_box)
;;; Esempio:
;;;   (populate-lists '("lista1" "lista2")
;;;                   '(("Item1" "Item2") ("OpzioneA" "OpzioneB")))
(defun populate-lists (key-list data-lists / data-lists2 key lst)
  (setq data-lists2 data-lists)
  (foreach key key-list
    (progn
      (setq lst (car data-lists2))
      (setq data-lists2 (cdr data-lists2))
      (start_list key)
      (foreach s lst (add_list s))
      (end_list))))
;;; GET-LIST-SELECTION
;;; Ottiene elemento selezionato da list_box
;;; Argomenti:
;;;   key - Nome list_box
;;;   item-list - Lista originale di item
;;; Ritorna: Elemento selezionato o nil
;;; Esempio:
;;;   (setq scelta (get-list-selection "lista_citta" 
;;;                   '("Milano" "Roma" "Torino")))
(defun get-list-selection (key item-list)
  (nth (atoi (get_tile key)) item-list))
;;; SET-LIST-SELECTION
;;; Imposta selezione in list_box
;;; Argomenti:
;;;   key - Nome list_box
;;;   value - Valore da selezionare (o indice numerico)
;;;   item-list - Lista originale di item
;;; Esempio:
;;;   (set-list-selection "lista_citta" "Roma" 
;;;                       '("Milano" "Roma" "Torino"))
(defun set-list-selection (key value item-list)
  (if (numberp value)
    (set_tile key (itoa (find-position value item-list)))
    (set_tile key (itoa (find-position value item-list)))))
;;; INSERT-LIST-ITEMS
;;; Inserisce item in list_box (con clear)
;;; Argomenti:
;;;   key - Nome list_box
;;;   item-list - Lista di item da inserire
;;; Esempio:
;;;   (insert-list-items "lista_prodotti" 
;;;                      '("Prodotto A" "Prodotto B" "Prodotto C"))
(defun insert-list-items (key item-list)
  (start_list key 3)
  (mapcar 'add_list item-list)
  (end_list))

;;; GET-MULTI-SELECTION
;;; Ottiene elementi selezionati da list_box multipla
;;; Argomenti:
;;;   key - Nome list_box
;;;   item-list - Lista originale di item
;;; Ritorna: Lista di elementi selezionati
;;; Esempio:
;;;   (setq selezionati (get-multi-selection "lista_opzioni" 
;;;                       '("Opzione1" "Opzione2" "Opzione3")))
(defun get-multi-selection (key item-list / positions pos result)
  (setq positions (parse-string (get_tile key) " "))
  (foreach pos positions
    (setq result (cons (nth (atoi pos) item-list) result)))
  (reverse result))
;;; SET-MULTI-SELECTION
;;; Imposta selezione in list_box
;;; Argomenti:
;;;   key - Nome list_box
;;;   value - Valori da selezionare (o indici numerici)
;;;   item-list - Lista originale di item
;;; Esempio:
;;;   (set-list-selection "lista_citta" "Roma" 
;;;                       '(0 2 4))  
  (defun set-multi-selection (key indices)
    (set_tile key (apply 'strcat (mapcar '(lambda (x) (strcat (itoa x) " ")) indices)))
  )

;;;=============================================================================
;;; 4. GESTIONE IMMAGINI E SLIDE
;;;=============================================================================
;;; DISPLAY-SLIDE
;;; Visualizza una slide in un controllo immagine DCL
;;; Da usare all'interno di dialog DCL attivo
;;; Argomenti:
;;;   slide-name - Percorso file .sld
;;; Ritorna: T
;;; Nota: Richiede controllo immagine con key "slide" nel DCL
;;; Esempio:
;;;   (display-slide "C:\\slides\\schema01.sld")
(defun display-slide (slide-name / x y)
  (setq x (dimx_tile "slide")
        y (dimy_tile "slide"))
  (start_image "slide")
  (slide_image 0 0 x y slide-name)
  (end_image)
  T)
;;; CLEAR-SLIDE
;;; Cancella/riempie area slide con colore
;;; Argomenti:
;;;   color - Numero colore AutoCAD (default 3 = verde)
;;; Esempio:
;;;   (clear-slide 250)  ; Bianco
;;;   (clear-slide 3)    ; Verde
(defun clear-slide (color / x y)
  (setq x (dimx_tile "slide")
        y (dimy_tile "slide"))
  (start_image "slide")
  (fill_image 0 0 x y color)
  (end_image))
;;; TOGGLE-IMAGE
;;; Accende/spegne un controllo immagine
;;; Argomenti:
;;;   image-key - Nome controllo immagine
;;;   on - T per accendere, nil per spegnere
;;; Esempio:
;;;   (toggle-image "immagine1" T)   ; Accende (verde)
;;;   (toggle-image "immagine1" nil) ; Spegne (nero)
(defun toggle-image (image-key on / x y)
  (setq x (fix (* 1.0 (dimx_tile image-key)))
        y (fix (* 1.0 (dimy_tile image-key))))
  (if on
    (progn
      (start_image image-key)
      (fill_image 0 0 x y 3)
      (end_image))
    (progn
      (start_image image-key)
      (fill_image 0 0 x y 1)
      (end_image))))
;;;=============================================================================
;;; 5. FUNZIONI HELPER
;;;=============================================================================
;;; PARSE-STRING
;;; Divide stringa in lista usando delimitatore
;;; Argomenti:
;;;   str - Stringa da dividere
;;;   delimiter - Delimitatore
;;; Ritorna: Lista di sottostringhe
;;; Esempio:
;;;   (parse-string "1 2 3 4" " ") => ("1" "2" "3" "4")
(defun parse-string (str delimiter / result pos temp)
  (split-string str delimiter))
;;;
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================