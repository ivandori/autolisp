;;;=============================================================================
;;; 8. FUNZIONI DIMOSTRATIVE
;;;=============================================================================
;;; DEMO-ATTRIBUTI-ENTITA
;;; Dimostra lettura attributi entità
(defun c:demo-entita-attr (/ ent etype layer color)
  (princ "\n=== DEMO ATTRIBUTI ENTITA' ===")
  (princ "\n\nSeleziona un'entita'...")
  (setq ent (car (entsel)))
  (if ent
    (progn
      (setq etype (get-entity-type ent)
            layer (get-entity-attribute ent 8)
            color (get-entity-attribute ent 62)
            handle (get-entity-handle ent))
      (princ "\n\nInformazioni entita':")
      (princ (strcat "\n  Tipo: " etype))
      (princ (strcat "\n  Layer: " layer))
      (princ (strcat "\n  Colore: " 
                    (if color (itoa color) "BYLAYER")))
      (princ (strcat "\n  Handle: " handle))
      (princ "\n\n=== Demo completata ==="))
    (princ "\nNessuna entita' selezionata!"))
  (princ))
;;; DEMO-MODIFICA-ENTITA
;;; Dimostra modifica attributi
(defun c:demo-entita-mod (/ ent old-layer new-layer)
  (princ "\n=== DEMO MODIFICA ENTITA' ===")
  (princ "\n\nSeleziona un'entita' da modificare...")
  (setq ent (car (entsel)))
  (if ent
    (progn
      (setq old-layer (get-entity-attribute ent 8))
      (princ (strcat "\n\nLayer corrente: " old-layer))
      (setq new-layer (getstring "\nNuovo layer: "))
      (if (and new-layer (/= new-layer ""))
        (progn
          (set-entity-attribute ent 8 new-layer)
          (princ (strcat "\nLayer modificato in: " new-layer))
          (princ "\n\nCambio colore in rosso...")
          (set-entity-attribute ent 62 1)
          (princ " OK"))
        (princ "\nModifica annullata"))
      (princ "\n\n=== Demo completata ==="))
    (princ "\nNessuna entita' selezionata!"))
  (princ))
;;; DEMO-BOUNDING-BOX
;;; Dimostra calcolo bounding box
(defun c:demo-entita-box (/ ent bbox min-pt max-pt)
  (princ "\n=== DEMO BOUNDING BOX ===")
  (princ "\n\nSeleziona un'entita'...")
  (setq ent (car (entsel)))
  (if ent
    (progn
      (if (setq bbox (get-entity-bounding-box ent))
        (progn
          (setq min-pt (car bbox)
                max-pt (cadr bbox))
          (princ "\n\nBounding Box:")
          (princ (strcat "\n  Min: " (punto-a-stringa min-pt)))
          (princ (strcat "\n  Max: " (punto-a-stringa max-pt)))
          (princ (strcat "\n  Larghezza: " 
                        (rtos (- (car max-pt) (car min-pt)) 2 2)))
          (princ (strcat "\n  Altezza: " 
                        (rtos (- (cadr max-pt) (cadr min-pt)) 2 2)))
          (princ "\n\nDisegno rettangolo bounding box...")
          (command "_.rectangle" min-pt max-pt)
          (princ " OK"))
        (princ "\nImpossibile calcolare bounding box!"))
      (princ "\n\n=== Demo completata ==="))
    (princ "\nNessuna entita' selezionata!"))
  (princ))
;;; DEMO-TRASFORMAZIONI
;;; Dimostra trasformazioni geometriche
(defun c:demo-entita-trasf (/ ent pt1 pt2)
  (princ "\n=== DEMO TRASFORMAZIONI ===")
  (princ "\n\nSeleziona un'entita' da trasformare...")
  (setq ent (car (entsel)))
  (if ent
    (progn
      (princ "\n\nSpecifica punto di destinazione...")
      (setq pt1 (getpoint))
      (if pt1
        (progn
          (princ "\nSpostamento entita'...")
          (move-entity-to-point ent pt1)
          (princ " OK")
          (princ "\n\nSpecifica secondo punto per centratura...")
          (setq pt2 (getpoint))
          (if pt2
            (progn
              (princ "\nCentratura tra i due punti...")
              (center-entity-between-points ent pt1 pt2)
              (princ " OK"))))
        (princ "\nOperazione annullata"))
      (princ "\n\n=== Demo completata ==="))
    (princ "\nNessuna entita' selezionata!"))
  (princ))
;;; DEMO-TUTTO-ENTITA
;;; Esegue tutte le demo entità
(defun c:demo-entita ()
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|  GESTIONE ENTITA' - DIMOSTRAZIONE COMPLETA            |")
  (princ "\n+========================================================+")
  (princ "\n")
  (c:demo-entita-attr)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-entita-mod)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-entita-box)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-entita-trasf)
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|       TUTTE LE DEMO ENTITA' COMPLETATE                |")
  (princ "\n+========================================================+")
)