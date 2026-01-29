;;;=============================================================================
;;; 9. FUNZIONI DIMOSTRATIVE
;;;=============================================================================
;;; DEMO-STRING-SPLIT
;;; Dimostra divisione stringhe
(defun c:demo-string-split (/ str parts)
  (princ "\n=== DEMO DIVISIONE STRINGHE ===")
  (setq str "Mario,Rossi,30,Milano")
  (princ (strcat "\n\nStringa: " str))
  (setq parts (split-string str ","))
  (princ "\n\nDopo split:")
  (foreach part parts
    (princ (strcat "\n  - " part)))
  (princ "\n\nEstrazione parti:")
  (princ (strcat "\n  Prima parte: " (string-left str ",")))
  (princ (strcat "\n  Seconda parte: " (string-mid str "," 1)))
  (princ (strcat "\n  Ultima parte: " (car (reverse parts))))
  (princ "\n\n=== Demo completata ===")
  (princ))

;;; SPLIT-STRING-TO-NUMBERS
(defun c:demo-split-string-to-numbers (/ s nums)
  (princ "\n=== DEMO split-string-to-numbers ===")
  (setq s "1.5,2.3,4.0")
  (princ (strcat "\nInput: " s))
  (setq nums (split-string-to-numbers s ","))
  (princ "\nNumeri parsati:")
  (foreach n nums (princ (strcat "\n  - " (rtos n 2 2))))
  (princ))

;;; 2. ESTRAZIONE PARTI
;;; STRING-LEFT-OF / RIGHT-OF / PART
(defun c:demo-string-left-of (/ s)
  (princ "\n=== DEMO string-left-of/string-right-of/string-part ===")
  (setq s "nome=valore=altro")
  (princ (strcat "\nInput: " s))
  (princ (strcat "\nLeft of '=' : " (string-left-of s "=")))
  (princ (strcat "\nRight of first '=' : " (string-right-of s "=")))
  (princ (strcat "\nPart left (l): " (string-part s "=" "l")))
  (princ (strcat "\nPart right (r): " (string-part s "=" "r")))
  (princ))

;;; STRING-LEFT / STRING-RIGHT / STRING-MID
(defun c:demo-string-left-right (/ s parts)
  (princ "\n=== DEMO string-left/string-right/string-mid ===")
  (setq s "a:b:c:d")
  (princ (strcat "\nInput: " s))
  (princ (strcat "\nstring-left (prima): " (string-left s ":")))
  (princ (strcat "\nstring-right (seconda): " (string-right s ":")))
  (princ (strcat "\nstring-mid (pos 2): " (string-mid s ":" 2)))
  (princ))

;;; STRING-TAIL / STRING-HEAD / STRING-SKIP / STRING-LAST-CHAR
(defun c:demo-string-tail-head-skip-lastchar (/ s)
  (princ "\n=== DEMO head/tail/skip/last-char ===")
  (setq s "abcdef")
  (princ (strcat "\nInput: " s))
  (princ (strcat "\nHead 3: " (string-head 3 s)))
  (princ (strcat "\nTail 3: " (string-tail 3 s)))
  (princ (strcat "\nSkip 2: " (string-skip 2 s)))
  (princ (strcat "\nLast char: " (string-last-char s)))
  (princ))

;;; 3. RICERCA E POSIZIONAMENTO
;;; FIND-CHAR-POSITION / FIND-FIRST-SUBSTRING
(defun c:demo-string-search (/ str pos)
  (princ "\n=== DEMO RICERCA IN STRINGHE ===")
      (setq str "path/to/my/file.dwg")
      (princ (strcat "\n\nStringa: " str))
      (setq pos (find-first-substring "file" str))
      (princ (strcat "\nPosizione 'file': " (if pos (itoa pos) "non trovato")))
      (setq pos (find-char-position "/" str))
      (princ (strcat "\nUltimo '/': posizione " (itoa pos)))
      (princ (strcat "\nContiene 'my': " 
                    (if (string-contains "my" str) "SI" "NO")))
      (princ (strcat "\nContiene 'xyz': " 
                    (if (string-contains "xyz" str) "SI" "NO")))
      (princ "\n\n=== Demo completata ===")
      (princ))

;;; FIND-LAST-SUBSTRING / FIND-SUBSTRING-FROM / STRING-CONTAINS
(defun c:demo-find-first-last-from (/ s)
  (princ "\n=== DEMO find-first-substring/find-last-substring/find-substring-from ===")
  (setq s "abxxabxxab")
  (princ (strcat "\nInput: " s))
  (setq pos (find-first-substring "xx" s))
  (princ (strcat "\nFirst 'xx' : " (if (and pos (numberp pos)) (itoa pos) "0")))
  (setq pos (find-last-substring "ab" s))
  (princ (strcat "\nLast 'ab' : " (if (and pos (numberp pos)) (itoa pos) "0")))
  (setq pos (find-substring-from "ab" s 4))
  (princ (strcat "\nFind 'ab' from pos 4 : " (if (and pos (numberp pos)) (itoa pos) "0")))
  (princ))

(defun c:demo-string-contains (/ s)
  (princ "\n=== DEMO string-contains ===")
  (setq s "questa e una prova")
  (princ (strcat "\nInput: " s))
  (princ (strcat "\nContains 'prova' : " (if (string-contains "prova" s) "SI" "NO")))
  (princ (strcat "\nContains 'xyz' : " (if (string-contains "xyz" s) "SI" "NO")))
  (princ))

;;; 4. RIMOZIONE CARATTERI
;;; REMOVE-CHAR / REMOVE-SPACES / TRIM-TRAILING-SPACES / TRIM-STRING / CLEAN-STRING
(defun c:demo-remove-char-spaces-trim (/ s)
  (princ "\n=== DEMO remove-char/remove-spaces/trim-trailing-spaces/trim-string ===")
  (setq s " a-b c - ")
  (princ (strcat "\nInput: [" s "]"))
  (princ (strcat "\nremove-char '-' : [" (remove-char "-" s) "]"))
  (princ (strcat "\nremove-spaces : [" (remove-spaces s) "]"))
  (princ (strcat "\ntrim-trailing-spaces : [" (trim-trailing-spaces "abc  ") "]"))
  (princ (strcat "\ntrim-string : [" (trim-string "  abc  ") "]"))
  (princ))

(defun c:demo-string-clean (/ str)
  (princ "\n=== DEMO PULIZIA STRINGHE ===")
  (setq str "  Testo  con  spazi  ")
  (princ (strcat "\n\nOriginale: [" str "]"))
  (princ (strcat "\nSenza spazi: [" (remove-spaces str) "]"))
  (princ (strcat "\nTrim: [" (trim-string str) "]"))
  (setq str "Test*File|Name<Bad>")
  (princ (strcat "\n\nFilename non valido: " str))
  (princ (strcat "\nSanitizzato: " (sanitize-filename str "_")))
  (princ "\n\n=== Demo completata ===")
  (princ))

(defun c:demo-clean-string (/ v)
  (princ "\n=== DEMO clean-string ===")
  (princ (strcat "\nclean-string with trailing: [" (clean-string "abc  ") "]"))
  (princ (strcat "\nclean-string non-string returns original: " (if (not (is-string 123)) "OK" "NO")))
  (princ))

;;; 5. TRONCAMENTO
;;; TRUNCATE-STRING / TRUNCATE-N-CHARS / REMOVE-EXTENSION / CUT-AFTER-LAST-CHAR
(defun c:demo-truncate-removeext-cut (/ s)
  (princ "\n=== DEMO truncate-string/truncate-n-chars/remove-extension/cut-after-last-char ===")
  (setq s "filename.txt")
  (princ (strcat "\nInput: " s))
  (princ (strcat "\ntruncate-string : " (truncate-string "abcd")))
  (princ (strcat "\ntruncate-n-chars 2 : " (truncate-n-chars 2 "abcd")))
  (princ (strcat "\nremove-extension : " (remove-extension s)))
  (princ (strcat "\ncut-after-last-char 'a/b/c' '/': " (cut-after-last-char "a/b/c" "/")))
  (princ))

;;; 6. FORMATTAZIONE E PADDING
;;; REVERSE-STRING / PAD-STRING-RIGHT / PAD-STRING-LEFT / PAD-WITH-CHAR / FORMAT-CELL
(defun c:demo-reverse-pad-format (/ s)
  (princ "\n=== DEMO reverse-string/string-reverse/pad-string-left/pad-with-char/pad-string-right/format-cell ===")
  (setq s "abc")
  (princ (strcat "\nInput: " s))
  (princ (strcat "\nreverse-string : " (reverse-string s)))
  (princ (strcat "\nstring-reverse : " (string-reverse s)))
  (princ (strcat "\npad-left (5) : ['" (pad-string-left s 5) "']"))
  (princ (strcat "\npad-with-char (5,'0') : ['" (pad-with-char s "0" 5) "']"))
  (princ (strcat "\npad-right (5) : ['" (pad-string-right s 5) "']"))
  (princ (strcat "\nformat-cell (len 4, '.') : ['" (format-cell s "." 4) "']"))
  (princ))

;;; 7. VALIDAZIONE E PULIZIA
;;; IS-STRING / SANITIZE-FILENAME / REPLACE-CHAR / REPLACE-SUBSTRING
(defun c:demo-is-string-replace (/ s)
  (princ "\n=== DEMO is-string/replace-char/replace-substring ===")
  (setq s "a-b--c")
  (princ (strcat "\nInput: " s))
  (princ (strcat "\nis-string 'abc' : " (if (is-string "abc") "T" "NIL")))
  (princ (strcat "\nreplace-char '-'->'_' : " (replace-char s "-" "_")))
  (princ (strcat "\nreplace-substring '--'->':' : " (replace-substring s "--" ":")))
  (princ))

;;; 8. CONVERSIONE
;;; STRING-TO-UPPER / STRING-TO-LOWER / STRING-REVERSE
(defun c:demo-case (/ s)
  (princ "\n=== DEMO string-to-upper/string-to-lower ===")
  (setq s "AbCd")
  (princ (strcat "\nInput: " s))
  (princ (strcat "\nUpper: " (string-to-upper s)))
  (princ (strcat "\nLower: " (string-to-lower s)))
  (princ))

;;; DEMO-TUTTO-STRING
;;; Esegue tutte le demo stringhe in ordine
(defun c:demo-string ()
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|  GESTIONE STRINGHE - DIMOSTRAZIONE COMPLETA           |")
  (princ "\n+========================================================+")
  (princ "\n")
  (c:demo-string-split)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-split-string-to-numbers)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-string-left-of)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-string-left-right)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-string-tail-head-skip-lastchar)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-string-search)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-find-first-last-from)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-string-contains)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-remove-char-spaces-trim)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-string-clean)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-clean-string)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-truncate-removeext-cut)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-reverse-pad-format)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-is-string-replace)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-case)
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|       TUTTE LE DEMO STRINGHE COMPLETATE               |")
  (princ "\n+========================================================+")
  (princ))

