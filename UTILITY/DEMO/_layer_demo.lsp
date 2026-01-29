;;;=============================================================================
;;; 8. FUNZIONI DIMOSTRATIVE
;;;=============================================================================
;;; DEMO-LAYER-BASE
;;; Dimostra operazioni base layer
(defun c:demo-layer-base (/ test-layer)
  (princ "\n=== DEMO LAYER BASE ===")
  (setq test-layer "TEST_LAYER")
  (princ (strcat "\n\n1. Creazione layer: " test-layer))
  (create-layer test-layer 1 "CONTINUOUS")
  (princ " OK")
  (princ (strcat "\n2. Verifica esistenza: " 
                (if (layer-exists test-layer) "Esiste" "Non esiste")))
  (princ (strcat "\n3. Layer acceso: " 
                (if (is-layer-on test-layer) "SI" "NO")))
  (princ "\n4. Cambio colore a verde...")
  (command "_.layer" "_c" "3" test-layer "")
  (princ " OK")
  (princ (strcat "\n5. Colore layer: " 
                (itoa (get-layer-color test-layer))))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LAYER-STATO
;;; Dimostra gestione stato layer
(defun c:demo-layer-stato (/ test-layer)
  (princ "\n=== DEMO STATO LAYER ===")
  (setq test-layer "TEST_STATO")
  (create-layer test-layer 2 "CONTINUOUS")
  (princ (strcat "\n\nLayer test: " test-layer))
  (princ "\n\n1. Spegnimento layer...")
  (turn-layer-off test-layer)
  (princ (strcat "\n   Stato: " 
                (if (is-layer-on test-layer) "Acceso" "Spento")))
  (getstring "\nPremi Invio...")
  (princ "\n2. Accensione layer...")
  (turn-layer-on test-layer)
  (princ (strcat "\n   Stato: " 
                (if (is-layer-on test-layer) "Acceso" "Spento")))
  (getstring "\nPremi Invio...")
  (princ "\n3. Blocco layer...")
  (lock-layer test-layer)
  (princ (strcat "\n   Bloccato: " 
                (if (is-layer-locked test-layer) "SI" "NO")))
  (getstring "\nPremi Invio...")
  (princ "\n4. Sblocco layer...")
  (unlock-layer test-layer)
  (princ (strcat "\n   Bloccato: " 
                (if (is-layer-locked test-layer) "SI" "NO")))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LAYER-CATTURA
;;; Dimostra cattura e ripristino stato
(defun c:demo-layer-cattura (/)
  (princ "\n=== DEMO CATTURA STATO ===")
  (princ "\n\n1. Cattura stato corrente...")
  (capture-layer-state)
  (princ " OK")
  (princ (strcat "\n   Layer corrente: " g-layer-old))
  (princ (strcat "\n   Layer accesi: " (itoa (length g-layers-on))))
  (princ (strcat "\n   Layer spenti: " (itoa (length g-layers-off))))
  (princ "\n\n2. Modifico stato (spengo tutti)...")
  (turn-all-layers-off "0")
  (princ " OK")
  (getstring "\nPremi Invio per ripristinare...")
  (princ "\n3. Ripristino stato originale...")
  (restore-layer-state)
  (princ " OK")
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-LAYER-LISTA
;;; Dimostra liste layer
(defun c:demo-layer-lista (/ all-layers)
  (princ "\n=== DEMO LISTA LAYER ===")
  (setq all-layers (list-all-layers))
  (princ (strcat "\n\nTotale layer nel disegno: " 
                (itoa (length all-layers))))
  (princ "\n\nLista layer:")
  (foreach layer all-layers
    (princ (strcat "\n  " layer 
                  " [" (if (is-layer-on layer) "ON" "OFF") "]"
                  " [" (if (is-layer-locked layer) "LOCK" "UNLOCK") "]")))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-TUTTO-LAYER
;;; Esegue tutte le demo layer
(defun c:demo-layer ()
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|  GESTIONE LAYER - DIMOSTRAZIONE COMPLETA              |")
  (princ "\n+========================================================+")
  (princ "\n")
  (c:demo-layer-base)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-layer-stato)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-layer-cattura)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-layer-lista)
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|         TUTTE LE DEMO LAYER COMPLETATE                |")
  (princ "\n+========================================================+")
  (princ))
