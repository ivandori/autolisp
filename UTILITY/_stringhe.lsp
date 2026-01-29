;;;=============================================================================
;;; GESTIONE STRINGHE  
;;; Ultimo aggiornamento: 15/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per  manipolare stringhe
;;; Dipendenze esterne: -
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. DIVISIONE E PARSING
;;; 2. ESTRAZIONE PARTI
;;; 3. RICERCA E POSIZIONAMENTO
;;; 4. RIMOZIONE CARATTERI
;;; 5. TRONCAMENTO
;;; 6. FORMATTAZIONE E PADDING
;;; 7. VALIDAZIONE E PULIZIA
;;; 8. CONVERSIONE
;;;=============================================================================
;;; 1. DIVISIONE E PARSING
;;;=============================================================================
;;; SPLIT-STRING
;;; Divide stringa in lista usando delimitatore
;;; Argomenti:
;;;   str - Stringa da dividere
;;;   delimiter - Carattere separatore
;;; Ritorna: Lista di sottostringhe
;;; Esempio:
;;;   (split-string "A,B,C" ",") => ("A" "B" "C")
(defun split-string (str delimiter / ch char-str result)
  (if str
    (progn
      (while (/= str "")
	(setq ch  (substr str 1 1)
	      str (substr str 2)
	)
	(cond
	  ((= ch delimiter)
	   (if char-str
	     (setq result   (cons char-str result)
		   char-str nil
	     )
	   )
	  )
	  (t
	   (if char-str
	     (setq char-str (strcat char-str ch))
	     (setq char-str ch)
	   )
	  )
	)
      )
      (if char-str
	(reverse (cons char-str result))
	(if result
	  (reverse result)
	  '("")
	)
      )
    )
    nil
  )
)
;;; SPLIT-STRING-TO-NUMBERS
;;; Divide stringa di numeri in lista numerica
;;; Argomenti:
;;;   str - Stringa numeri separati
;;;   delimiter - Carattere separatore
;;; Ritorna: Lista di numeri
;;; Esempio:
;;;   (split-string-to-numbers "1.5,2.3,4.0" ",") => (1.5 2.3 4.0)
(defun split-string-to-numbers (str delimiter / parts result)
  (setq	parts  (split-string str delimiter)
	result nil
  )
  (foreach part	parts
    (setq result (cons (atof part) result))
  )
  (reverse result)
)
;;;=============================================================================
;;; 2. ESTRAZIONE PARTI
;;;================================0=============================================
;;; STRING-LEFT-OF
;;; Estrae parte sinistra di stringa rispetto a delimitatore
;;; Argomenti:
;;;   str - Stringa da processare
;;;   delimiter - Delimitatore
;;; Ritorna: Parte sinistra
;;; Esempio:
;;;   (string-left-of "nome=valore" "=") => "nome"
(defun string-left-of (str delimiter / pos)
  (setq pos (find-first-substring delimiter str))
  (if pos
    (substr str 1 (1- pos))
    str
  )
)
;;; STRING-RIGHT-OF
;;; Estrae parte destra di stringa rispetto a delimitatore
;;; Argomenti:
;;;   str - Stringa da processare
;;;   delimiter - Delimitatore
;;; Ritorna: Parte destra
;;; Esempio:
;;;   (string-right-of "nome=valore" "=") => "valore"
(defun string-right-of (str delimiter / pos)
  (setq pos (find-first-substring delimiter str))
  (if pos
    (substr str (+ pos (strlen delimiter)))
    ""
  )
)
;;; STRING-PART
;;; Estrae parte sinistra o destra rispetto a delimitatore
;;; Argomenti:
;;;   str - Stringa da processare
;;;   delimiter - Delimitatore
;;;   side - "l" per sinistra, "r" per destra
;;; Ritorna: Parte richiesta
;;; Esempio:
;;;   (string-part "a=b=c" "=" "l") => "a"
(defun string-part (str delimiter side)
  (if (= side "l")
    (string-left-of str delimiter)
    (string-right-of str delimiter)
  )
)
;;; STRING-LEFT
;;; Estrae parte sinistra fino a delimitatore
;;; Argomenti:
;;;   str - Stringa
;;;   delimiter - Delimitatore
;;; Ritorna: Parte sinistra
;;; Esempio:
;;;   (string-left "nome=valore" "=") => "nome"
(defun string-left (str delimiter)
  (car (split-string str delimiter))
)
;;; STRING-RIGHT
;;; Estrae parte destra da delimitatore
;;; Argomenti:
;;;   str - Stringa
;;;   delimiter - Delimitatore
;;; Ritorna: Parte destra
;;; Esempio:
;;;   (string-right "nome=valore" "=") => "valore"
(defun string-right (str delimiter / parts)
  (setq parts (split-string str delimiter))
  (if (> (length parts) 1)
    (cadr parts)
    ""
  )
)
;;; STRING-MID
;;; Estrae sottostringhe da posizione specifica dopo delimitatore
;;; Argomenti:
;;;   str - Stringa
;;;   delimiter - Delimitatore
;;;   position - Posizione nella lista risultante
;;; Ritorna: Sottostringa
;;; Esempio:
;;;   (string-mid "a:b:c" ":" 1) => "b"
(defun string-mid (str delimiter position)
  (nth position (split-string str delimiter))
)
;;; STRING-TAIL
;;; Mantiene ultimi N caratteri
;;; Argomenti:
;;;   n - Numero caratteri
;;;   str - Stringa
;;; Ritorna: Ultimi N caratteri
;;; Esempio:
;;;   (string-tail 3 "abcdef") => "def"
(defun string-tail (n str / len)
  (setq len (strlen str))
  (if (> len n)
    (substr str (1+ (- len n)))
    str
  )
)
;;; STRING-HEAD
;;; Mantiene primi N caratteri
;;; Argomenti:
;;;   n - Numero caratteri
;;;   str - Stringa
;;; Ritorna: Primi N caratteri
;;; Esempio:
;;;   (string-head 3 "abcdef") => "abc"
(defun string-head (n str / result temp)
  (setq	result ""
	temp str
  )
  (repeat n
    (if	(/= temp "")
      (progn
	(setq result (strcat result (substr temp 1 1))
	      temp   (substr temp 2)
	)
      )
    )
  )
  result
)
;;; STRING-SKIP
;;; Salta primi N caratteri
;;; Argomenti:
;;;   n - Numero caratteri da saltare
;;;   str - Stringa
;;; Ritorna: Stringa rimanente
;;; Esempio:
;;;   (string-skip 2 "abcdef") => "cdef"
(defun string-skip (n str)
  (if (zerop n)
    str
    (string-skip (1- n) (substr str 2))
  )
)
;;; STRING-LAST-CHAR
;;; Ottiene ultimo carattere
;;; Argomenti:
;;;   str - Stringa
;;; Ritorna: Ultimo carattere
;;; Esempio:
;;;   (string-last-char "abc") => "c"
(defun string-last-char	(str / len)
  (if (/= str "")
    (progn
      (setq len (strlen str))
      (substr str len)
    )
    ""
  )
)
;;;=============================================================================
;;; 3. RICERCA E POSIZIONAMENTO
;;;=============================================================================
;;; FIND-CHAR-POSITION
;;; Trova ultima posizione carattere in stringa (ricorsiva)
;;; Argomenti:
;;;   char - Carattere da cercare
;;;   str - Stringa
;;; Ritorna: Posizione o 0
;;; Esempio:
;;;   (find-char-position "." "a.b.c") => 3
(defun find-char-position (char str)
  (cond
    ((= (strlen str) 0) (strlen str))
    ((= char (substr str (strlen str))) (strlen str))
    ((find-char-position
       char
       (string-head (1- (strlen str)) str)
     )
    )
  )
)
;;; FIND-FIRST-SUBSTRING
;;; Trova prima posizione di una sottostringa in una stringa (sostituisce vl-string-search)
;;; Argomenti:
;;;   pattern - Sottostringa da cercare 
;;;   str - Stringa in cui cercare
;;; Ritorna: Posizione (1-based) o nil
;;; Esempio:
;;; (find-first-substring "test" "questo è un test") ; => 13
;;; (find-first-substring "xyz" "questa stringa") ; => nil
(defun find-first-substring (pattern str / i found pos len-pat len-str)
  (setq	len-pat	(strlen pattern)
	len-str	(strlen str)
	i	1
	found	nil
	pos	nil
  )
  (while (and (<= (+ i len-pat -1) len-str) (not found))
    (if	(= (substr str i len-pat) pattern)
      (setq found T
	    pos	i
      )
    )
    (setq i (1+ i))
  )
  pos
)
;;; FIND-LAST-SUBSTRING
;;; Cerca l'ULTIMA occorrenza di una sottostringa in una stringa
;;; Argomenti:
;;;   substr - Sottostringa da cercare
;;;   str - Stringa in cui cercare
;;; Ritorna: Posizione o nil
;;; Esempio:
;;;   (find-last-substring "ab" "abxxab") => 5
(defun find-last-substring (pattern str / pos count len-substr)
  (setq	count 1
	len-substr
	 (strlen pattern)
	pos nil
  )
  (while (/= (substr str count len-substr) "")
    (if	(= (substr str count len-substr) pattern)
      (setq pos count)
    )
    (setq count (1+ count))
  )
  pos
)
;;; FIND-SUBSTRING-FROM
;;; Trova la prima occorrenza della sottostringa a partire da una posizione
;;; Argomenti:
;;;   substring  - Sottostringa da cercare
;;;   str        - Stringa in cui effettuare la ricerca
;;;   start-pos - Posizione iniziale di ricerca (1-based)
;;; Ritorna: 
;;;   Posizione (1-based) della prima occorrenza trovata, oppure nil se non trovata.
;;; Esempio:
;;;   (find-substring-from "test" "this is a test test" 10) => 15
(defun find-substring-from
       (substring str start-pos / pos count len-substr len-str)
  "Trova sottostringa da posizione start-pos (1-based). Ritorna posizione 1-based o nil."
  (setq	len-substr (strlen substring)
	len-str	   (strlen str)
	pos	   nil
  )
  (if (and (> len-substr 0)
	   (> len-str 0)
	   (>= start-pos 1)
	   (<= start-pos len-str)
      )
    (progn
      (setq count start-pos)
      (while (<= count (- len-str len-substr -1))
	(if (= (substr str count len-substr) substring)
	  (progn
	    (setq pos count)
	    (setq count (1+ len-str))
	  )
	)
	(setq count (1+ count))
      )
    )
  )
  pos
)
;;; STRING-CONTAINS
;;; Verifica se stringa contiene sottostringa
;;; Argomenti:
;;;   substr - Sottostringa da cercare
;;;   str - Stringa
;;; Ritorna: T se contiene, nil altrimenti
;;; Esempio:
;;;   (string-contains "test" "this is a test") => T
(defun string-contains (substring str)
  (if (find-first-substring substring str)
    T
    nil
  )
)
;;;=============================================================================
;;; 4. RIMOZIONE CARATTERI
;;;=============================================================================
;;; REMOVE-CHAR
;;; Rimuove tutti i caratteri specifici
;;; Argomenti:
;;;   char - Carattere da rimuovere
;;;   str - Stringa
;;; Ritorna: Stringa pulita
;;; Esempio:
;;;   (remove-char "-" "a-b-c") => "abc"
(defun remove-char (char str / len pos current result)
  (setq	len    (strlen str)
	pos    0
	result ""
  )
  (repeat len
    (setq pos	  (1+ pos)
	  current (substr str pos 1)
    )
    (if	(/= current char)
      (if result
	(setq result (strcat result current))
	(setq result current)
      )
    )
  )
  result
)
;;; REMOVE-SPACES
;;; Rimuove tutti gli spazi
;;; Argomenti:
;;;   str - Stringa
;;; Ritorna: Stringa senza spazi
;;; Esempio:
;;;   (remove-spaces "a b c") => "abc"
(defun remove-spaces (str / n result current)
  (setq	n 1
	result ""
  )
  (repeat (strlen str)
    (setq current (substr str n 1)
	  n	  (1+ n)
    )
    (if	(/= current " ")
      (setq result (strcat result current))
    )
  )
  result
)
;;; TRIM-TRAILING-SPACES
;;; Rimuove spazi finali
;;; Argomenti:
;;;   str - Stringa
;;; Ritorna: Stringa senza spazi finali
;;; Esempio:
;;;   (trim-trailing-spaces "abc  ") => "abc"
(defun trim-trailing-spaces (str / n current)
  (setq n (strlen str))
  (while (and (> n 0)
	      (= (setq current (substr str n 1)) " ")
	      (setq n (1- n))
	 )
  )
  (if (> n 0)
    (substr str 1 n)
    ""
  )
)
;;; TRIM-STRING
;;; Rimuove spazi iniziali e finali
;;; Argomenti:
;;;   str - Stringa
;;; Ritorna: Stringa trimmed
;;; Esempio:
;;;   (trim-string "  abc  ") => "abc"
(defun trim-string (str / result)
  (setq result (trim-trailing-spaces str))
  (while (and (> (strlen result) 0)
	      (= (substr result 1 1) " ")
	 )
    (setq result (substr result 2))
  )
  result
)
;;; CLEAN-STRING
;;; Se stringa rimuove spazi finali, altrimenti ritorna input
;;; Argomenti:
;;;   input - Qualsiasi tipo
;;; Ritorna: Stringa pulita o input originale
;;; Esempio:
;;;   (clean-string "abc  ") => "abc"
(defun clean-string (input)
  (if (and input (= (type input) 'str))
    (trim-trailing-spaces input)
    input
  )
)
;;;=============================================================================
;;; 5. TRONCAMENTO
;;;=============================================================================
;;; TRUNCATE-STRING
;;; Rimuove ultimo carattere
;;; Argomenti:
;;;   str - Stringa
;;; Ritorna: Stringa senza ultimo carattere
;;; Esempio:
;;;   (truncate-string "abcd") => "abc"
(defun truncate-string (str)
  (if (> (strlen str) 0)
    (substr str 1 (1- (strlen str)))
    ""
  )
)
;;; TRUNCATE-N-CHARS
;;; Rimuove ultimi N caratteri
;;; Argomenti:
;;;   n - Numero caratteri da rimuovere
;;;   str - Stringa
;;; Ritorna: Stringa troncata
;;; Esempio:
;;;   (truncate-n-chars 2 "abcd") => "ab"
(defun truncate-n-chars	(n str)
  (if (> n 0)
    (repeat n
      (setq str (truncate-string str))
    )
  )
  str
)
;;; REMOVE-EXTENSION
;;; Rimuove estensione (ultimi 4 caratteri)
;;; Argomenti:
;;;   str - Stringa con estensione
;;; Ritorna: Stringa senza estensione
;;; Esempio:
;;;   (remove-extension "file.txt") => "file"
(defun remove-extension	(str)
  (if (>= (strlen str) 4)
    (substr str 1 (- (strlen str) 4))
    str
  )
)
;;; CUT-AFTER-LAST-CHAR
;;; Taglia dopo ultima occorrenza carattere
;;; Argomenti:
;;;   str - Stringa
;;;   char - Carattere
;;; Ritorna: Parte prima dell'ultimo carattere
;;; Esempio:
;;;   (cut-after-last-char "a/b/c" "/") => "a/b"
(defun cut-after-last-char (str char / pos)
  (setq pos (find-char-position char str))
  (if (> pos 0)
    (substr str 1 (1- pos))
    str
  )
)
;;;=============================================================================
;;; 6. FORMATTAZIONE E PADDING
;;;=============================================================================
;;; REVERSE-STRING
;;; Inverte una stringa (funzione helper)
;;; Esempio:
;;;   (reverse-string "abc") => "cba"
(defun reverse-string (str / result i)
  (setq	result ""
	i      (strlen str)
  )
  (while (> i 0)
    (setq result (strcat result (substr str i 1)))
    (setq i (1- i))
  )
  result
)
;;; PAD-STRING-RIGHT
;;; Aggiunge spazi a destra fino a lunghezza N
;;; Argomenti:
;;;   str - Stringa
;;;   n - Lunghezza totale desiderata
;;; Ritorna: Stringa con padding
;;; Esempio:
;;;   (pad-string-right "a" 3) => "a  "
(defun pad-string-right	(str n / len padding)
  (setq	len	(strlen str)
	padding	""
  )
  (if (< len n)
    (repeat (- n len)
      (setq padding (strcat padding " "))
    )
  )
  (strcat str padding)
)
;;; PAD-STRING-LEFT
;;; Aggiunge spazi a sinistra fino a lunghezza N
;;; Argomenti:
;;;   str - Stringa
;;;   n - Lunghezza totale desiderata
;;; Ritorna: Stringa con padding
;;; Esempio:
;;;   (pad-string-left "a" 3) => "  a"
(defun pad-string-left (str n / len padding)
  (setq	len	(strlen str)
	padding	""
  )
  (if (< len n)
    (repeat (- n len)
      (setq padding (strcat padding " "))
    )
  )
  (strcat padding str)
)
;;; PAD-WITH-CHAR
;;; Aggiunge carattere specifico fino a lunghezza N
;;; Argomenti:
;;;   str - Stringa
;;;   char - Carattere per padding
;;;   n - Lunghezza totale
;;; Ritorna: Stringa con padding
;;; Esempio:
;;;   (pad-with-char "a" "0" 3) => "a00"
(defun pad-with-char (str char n / len padding)
  (setq	len	(strlen str)
	padding	""
  )
  (if (< len n)
    (repeat (- n len)
      (setq padding (strcat padding char))
    )
  )
  (strcat str padding)
)
;;; FORMAT-CELL
;;; Tronca o completa stringa per lunghezza fissa
;;; Argomenti:
;;;   str - Stringa
;;;   char - Carattere per padding
;;;   n - Lunghezza fissa
;;; Ritorna: Stringa formattata
;;; Esempio:
;;;   (format-cell "abc" " " 5) => "abc  "
(defun format-cell (str char n / len)
  (setq len (strlen str))
  (if (> len n)
    (truncate-n-chars (- len n) str)
    (pad-with-char str char n)
  )
)
;;;=============================================================================
;;; 7. VALIDAZIONE E PULIZIA
;;;=============================================================================
;;; IS-STRING
;;; Verifica se elemento è stringa
;;; Argomenti:
;;;   element - Elemento da verificare
;;; Ritorna: T se stringa, nil altrimenti
;;; Esempio:
;;;   (is-string "abc") => T
(defun is-string (element)
  (= (type element) 'str)
)
;;; SANITIZE-FILENAME
;;; Rimuove caratteri non validi per nomi file (senza DOSLIB)
;;; Argomenti:
;;;   str - Stringa da pulire
;;;   replacement - Carattere sostitutivo (default "_")
;;; Ritorna: Stringa valida per filename
;;; Caratteri non validi: * | \ : " < > ? /
;;; Esempio:
;;;   (sanitize-filename "inva*lid?.txt" "_") => "inva_lid_.txt"
(defun sanitize-filename (str replacement / result invalid-chars)
  (if (null replacement)
    (setq replacement "_")
  )
  (setq	invalid-chars
	 '("*" "|" "\\" ":" "\"" "<" ">" "?" "/")
	result str
  )
  (foreach char	invalid-chars
    (setq result (replace-char result char replacement))
  )
  result
)
;;; REPLACE-CHAR
;;; Sostituisce carattere in stringa
;;; Argomenti:
;;;   str - Stringa
;;;   old-char - Carattere da sostituire
;;;   new-char - Carattere nuovo
;;; Ritorna: Stringa modificata
;;; Esempio:
;;;   (replace-char "a-b" "-" "_") => "a_b"
(defun replace-char (str old-char new-char / result pos current)
  (setq	result ""
	pos 1
  )
  (repeat (strlen str)
    (setq current (substr str pos 1))
    (setq result (strcat result
			 (if (= current old-char)
			   new-char
			   current
			 )
		 )
    )
    (setq pos (1+ pos))
  )
  result
)
;;; REPLACE-SUBSTRING
;;; Sostituisce sottostringa in stringa
;;; Argomenti:
;;;   str - Stringa originale
;;;   old-str - Sottostringa da sostituire
;;;   new-str - Sottostringa nuova
;;; Ritorna: Stringa modificata
;;; Esempio:
;;;   (replace-substring "a--b--c" "--" ":") => "a:b:c"
(defun replace-substring (str old-str new-str / pos parts result)
  ;; Supporta anche sottostringhe di lunghezza > 1 come separatore.
  (if (or (null str) (null old-str) (= (strlen old-str) 0))
    str
    (progn
      (setq result ""
	    pos	   (find-first-substring old-str str)
      )
      (while pos
	(setq result (strcat result (substr str 1 (1- pos)) new-str))
	(setq str (substr str (+ pos (strlen old-str))))
	(setq pos (find-first-substring old-str str))
      )
      (strcat result str)
    )
  )
)
;;;=============================================================================
;;; 8. CONVERSIONE
;;;=============================================================================
;;; STRING-TO-UPPER
;;; Converte in maiuscolo
;;; Argomenti:
;;;   str - Stringa
;;; Ritorna: Stringa maiuscola
;;; Esempio:
;;;   (string-to-upper "abc") => "ABC"
(defun string-to-upper (str)
  (strcase str T)
)
;;; STRING-TO-LOWER
;;; Converte in minuscolo
;;; Argomenti:
;;;   str - Stringa
;;; Ritorna: Stringa minuscola
;;; Esempio:
;;;   (string-to-lower "ABC") => "abc"
(defun string-to-lower (str)
  (strcase str)
)
;;; STRING-REVERSE
;;; Inverte stringa
;;; Argomenti:
;;;   str - Stringa
;;; Ritorna: Stringa invertita
;;; Esempio:
;;;   (string-reverse "abc") => "cba"
(defun string-reverse (str / result i)
  (setq	result ""
	i      (strlen str)
  )
  (while (> i 0)
    (setq result (strcat result (substr str i 1))
	  i	 (1- i)
    )
  )
  result
)
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================