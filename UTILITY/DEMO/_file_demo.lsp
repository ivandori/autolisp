;;;==============================================================================
;;; DEMO E TEST
;;;==============================================================================
(defun c:test-file-utils (/ test-file test-dir file-list)
  (princ "\n=== TEST FILE-UTILITIES (AutoLISP Puro) ===\n")
  (setq test-file (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
  (princ (strcat "\n1. File corrente: " test-file))
  (if (findfile test-file)
    (progn
      (princ (strcat "\n   Esiste: SI"))
      (princ (strcat "\n   Data: "
		     (if (get-file-date test-file)
		       (get-file-date test-file)
		       "N/A"
		     )
	     )
      )
      (princ (strcat "\n   Dimensione: "
		     (if (get-file-size test-file)
		       (itoa (get-file-size test-file))
		       "N/A"
		     )
		     " bytes"
	     )
      )
      (princ (strcat "\n   Bloccato: "
		     (if (is-file-locked test-file)
		       "SI"
		       "NO"
		     )
	     )
      )
      (princ (strcat "\n   Modificato: "
		     (if (is-drawing-modified)
		       "SI"
		       "NO"
		     )
	     )
      )
    )
    (princ "\n   File non trovato")
  )
  (setq test-dir (getvar "DWGPREFIX"))
  (princ (strcat "\n\n2. Directory corrente: " test-dir))
  (setq file-list (get-directory-files test-dir "*.*"))
  (princ (strcat "\n   File trovati: "
		 (if file-list
		   (itoa (length file-list))
		   "0"
		 )
	 )
  )
  (princ (strcat "\n   Directory bloccata: "
		 (if (is-directory-locked test-dir)
		   "SI"
		   "NO"
		 )
	 )
  )

  (setq	s
	 (getstring
	   "\n   TROVA LA DATA PIU'RECENTE DEI FILE DELLA CARTELLA (MOLTO LENTO!) SI O NO?"
	 )
  )
  (IF (= s "SI")
    (princ (strcat "\n   Data ultima modifica: "
		   (if (get-directory-date test-dir)
		     (get-directory-date test-dir)
		     "N/A"
		   )
	   )
    )
  )
  (setq	s
	 (getstring
	   "\n   TROVA LA DATA PIU'RECENTE DEI FILE DWG/DXF DELLA CARTELLA (MOLTO LENTO!) SI O NO?"
	 )
  )
  (IF (= s "SI")
    (progn
      (princ "\n\n3. File DWG/DXF nella directory:")

      (princ
	(strcat	"\n   Ultima modifica: "
		(get-directory-date-by-ext test-dir '("dwg" "dxf"))
	)
      )
      (if file-list
	(progn
	  (princ "\n\n4. Primi 5 file trovati:")
	  (setq i 0)
	  (foreach f file-list
	    (if	(< i 5)
	      (progn
		(princ (strcat "\n   - " f))
		(setq i (1+ i))
	      )
	    )
	  )
	)
      )
    )
  )
  (princ "\n\n=== TEST COMPLETATO ===\n")
  (princ)

)

(princ
  "\n* Digitare TEST-FILE-UTILS per test funzioni nuove *"
)

(princ)