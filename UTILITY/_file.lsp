;;;=============================================================================
;;; GESTIONE FILE E DIRECTORY
;;; Ultimo aggiornamento: 26/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per operazioni su file e directory
;;; Dipendenze esterne: STRING-RIGHT, STRING-RIGHT (_stringhe.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. CONTROLLI DISEGNO / STATO
;;; 2. DATA / ORA E CONFRONTI
;;; 3. DIRECTORY / ELENCO FILE
;;; 4. OPERAZIONI FILE (READ/WRITE/COPY/MOVE/DELETE/SIZE)
;;; 5. FUNZIONI HELPER
;;;=============================================================================
;;; 1. CONTROLLI DISEGNO / STATO
;;;=============================================================================
;;; IS-DRAWING-MODIFIED
;;; Verifica se il disegno corrente è modificato (DBMOD bit0)
;;; Argomenti: -
;;; Ritorna: T o nil
(defun is-drawing-modified (/ dbmod)
  (setq dbmod (getvar "DBMOD"))
  (if (and dbmod (/= (logand dbmod 1) 0)) T nil))
;;;=============================================================================
;;; 2. DATA / ORA E CONFRONTI
;;;=============================================================================
;;; CAD-DATE-NUMBER-TO-TIME-LIST
;;; Converte numero CAD YYYYMMDD.HHMM in lista (year month day hour minute second)
;;; Argomenti: cad-date - numero CAD
;;; Ritorna: lista time o nil
;;; Esempio:
;;;   (cad-date-number-to-time-list 20240115.1030)
;;;   => (2024 1 15 10 30 0)
(defun cad-date-number-to-time-list (cad-date / year month day hour minute)
  (if cad-date
    (progn
      (setq year (fix (/ cad-date 10000.0))
            month (fix (/ (rem cad-date 10000.0) 100.0))
            day (fix (rem cad-date 100.0))
            hour (fix (* (rem cad-date 1.0) 100.0))
            minute (fix (* (rem cad-date 1.0) 10000.0)))
      (list year month day hour minute 0)) ; secondi impostati a 0
    nil))
;;; GET-FILE-DATE
;;; Ottiene data/ora file come stringa "YYYY-MM-DD HH:MM:SS"
;;; Argomenti: filename - percorso file
;;; Ritorna: stringa data o nil
(defun get-file-date (filename / temp-file cmd-output date-str year month day hour minute)
  (if (and filename (findfile filename))
    (progn
      (setq temp-file (get-temp-file-path "filedate.txt"))
      (execute-command-with-output (strcat "for %I in (\"" filename "\") do @echo %~tI") temp-file)
      (sleep 1)
      (if (setq cmd-output (read-text-file temp-file))
        (progn
          (delete-file temp-file)
          (setq date-str (car cmd-output))
          (if (and date-str (> (strlen date-str) 16)) ; Assumendo formato "DD/MM/YYYY HH:MM"
            (progn
              (setq day (atoi (substr date-str 1 2))
                    month (atoi (substr date-str 4 2))
                    year (atoi (substr date-str 7 4))
                    hour (atoi (substr date-str 12 2))
                    minute (atoi (substr date-str 15 2)))
              (strcat (itoa year) "-"
                      (if (< month 10) "0" "") (itoa month) "-"
                      (if (< day 10) "0" "") (itoa day) " "
                      (if (< hour 10) "0" "") (itoa hour) ":"
                      (if (< minute 10) "0" "") (itoa minute) ":00"))
            nil))
        (progn
          (delete-file temp-file)
          nil)))
    nil))
;;; GET-FILE-DATE-NUMBER
;;; Data in formato numerico CAD YYYYMMDD.HHMM
;;; Argomenti: filename - percorso file
;;; Ritorna: numero o nil
(defun get-file-date-number (filename / temp-file cmd-output date-str year month day hour minute)
  (if (and filename (findfile filename))
    (progn
      (setq temp-file (get-temp-file-path "filedate.txt"))
      (execute-command-with-output (strcat "for %I in (\"" filename "\") do @echo %~tI") temp-file)
      (sleep 1)
      (if (setq cmd-output (read-text-file temp-file))
        (progn
          (delete-file temp-file)
          (setq date-str (car cmd-output))
          (if (and date-str (> (strlen date-str) 16)) ; Assumendo formato "DD/MM/YYYY HH:MM"
            (progn
              (setq day (atoi (substr date-str 1 2))
                    month (atoi (substr date-str 4 2))
                    year (atoi (substr date-str 7 4))
                    hour (atoi (substr date-str 12 2))
                    minute (atoi (substr date-str 15 2)))
              (+ (* year 10000.0) (* month 100.0) day (* hour 0.01) (* minute 0.0001)))
            nil))
        (progn
          (delete-file temp-file)
          nil)))
    nil))
;;; FORMAT-DATE-STRING
;;; Formatta lista time in "GG/MM/AAAA   HH:MM"
;;; Argomenti: time-list - lista (year month day hour minute second)
;;; Ritorna: stringa formattata o nil
;;; Esempio:
;;;   (format-date-string '(2024 1 15 10 30 0)) => "15/01/2024   10:30"
(defun format-date-string (time-list / year month day hour minute)
  (if time-list
    (progn
      (setq year (nth 0 time-list) month (nth 1 time-list) day (nth 2 time-list)
            hour (nth 3 time-list) minute (nth 4 time-list))
      (strcat (if (< day 10) "0" "") (itoa day) "/"
              (if (< month 10) "0" "") (itoa month) "/" (itoa year) "   "
              (if (< hour 10) "0" "") (itoa hour) ":"
              (if (< minute 10) "0" "") (itoa minute)))
    nil))
;;; FORMAT-CAD-DATE-NUMBER
;;; Formatta numero CAD YYYYMMDD.HHMM in "GG/MM/AAAA   HH:MM"
;;; Argomenti: data - numero CAD
;;; Ritorna: stringa formattata o nil
;;; Esempio:
;;;   (format-cad-date-number 20240115.1030) => "15/01/2024   10:30"
(defun format-cad-date-number (data / data$ anno$ mese$ giorno$ ora$ minuti$)
  (if data
    (progn
      (setq data$ (rtos data 2 4)
            anno$ (substr data$ 1 4)
            mese$ (substr data$ 5 2)
            giorno$ (substr data$ 7 2)
            ora$ (substr data$ 10 2)
            minuti$ (substr data$ 12 2))
      (if (= ora$ "") (setq ora$ "00"))
      (if (= minuti$ "") (setq minuti$ "00"))
      (if (= (strlen minuti$) 1) (setq minuti$ (strcat minuti$ "0")))
      (strcat giorno$ "/" mese$ "/" anno$ "   " ora$ ":" minuti$))
    nil))
;;; COMPARE-FILE-TIMES
;;; Confronta due liste time
;;; Argomenti: time1 time2 - liste (year month day hour minute second)
;;; Ritorna: 1 se time1 > time2, -1 se <, 0 se =
;; Esempio:
;;   (compare-file-times '(2024 1 15 10 30 0) '(2024 1 14 12 0 0)) => 1
(defun compare-file-times (time1 time2 / i result)
  (setq result 0 i 0)
  (while (and (< i 6) (= result 0))
    (cond ((> (nth i time1) (nth i time2)) (setq result 1))
          ((< (nth i time1) (nth i time2)) (setq result -1)))
    (setq i (1+ i)))
  result)
;;; GET-LATEST-FILE-DATE
;;; Trova la data più recente in lista file
;;; Argomenti: file-list - lista percorsi file
;;; Ritorna: lista time o nil
(defun get-latest-file-date (file-list / max-date file date max-time)
  (setq max-date 0)
  (foreach file file-list
    (if (setq date (get-file-date-number file))
      (if (> date max-date) (setq max-date date))))
  (if (> max-date 0)
    (cad-date-number-to-time-list max-date)
    nil))
;;; GET-LATEST-FILE-DATE-NUMBER
;;; Data più recente come numero CAD
;;; Argomenti: file-list - lista percorsi file
;;; Ritorna: numero CAD o 0
(defun get-latest-file-date-number (file-list / max-date file date)
  (setq max-date 0)
  (foreach file file-list
    (if (setq date (get-file-date-number file))
      (if (> date max-date) (setq max-date date))))
  max-date)
;;;=============================================================================
;;; 3. DIRECTORY / ELENCO FILE
;;;=============================================================================
;;; IS-FILE-NEWER
;;; Confronta le date di modifica di due file
;;; Argomenti:
;;;   file1 - Percorso del primo file (stringa)
;;;   file2 - Percorso del secondo file (stringa)
;;; Restituisce:
;;;   T se file1 è più recente di file2 o se file2 non esiste
;;;   nil se file2 è più recente o uguale a file1
;;; Esempio:
;;;   (is-file-newer "C:/temp/new.dwg" "C:/temp/old.dwg") => T
(defun is-file-newer (file1 file2 / date1 date2)
  (setq date1 (get-file-date-number file1)
        date2 (get-file-date-number file2))
  (cond
    ((null date1) nil)
    ((null date2) T)
    ((> date1 date2) T)
    (T nil))
)
;;;------------------------------------------------------------------------------
;;; IS-FILE-LOCKED
;;; Verifica se un file è bloccato (in uso da altro processo)
;;; Argomenti:
;;;   filename - Percorso completo del file da verificare (stringa)
;;; Restituisce:
;;;   T se il file è bloccato, nil se è disponibile o non esiste
;;; Note:
;;;   Tenta di aprire il file in modalità append. Se fallisce, è bloccato.
(defun is-file-locked (filename / f result)
  (if (and filename (findfile filename))
    (progn
      (setq f (open filename "a"))
      (if f
        (progn
          (close f)
          (setq result nil))
        (setq result T)))
    nil)
  result
)
;;; IS-DIRECTORY-LOCKED
;;; Verifica se una directory è bloccata/aperta da un processo
;;; Argomenti:
;;;   dirname - Percorso della directory (stringa)
;;; Restituisce:
;;;   T se la directory è bloccata, nil altrimenti
;;; Note:
;;;   Tenta di creare un file temporaneo nella directory per verificare accesso
(defun is-directory-locked (dirname / test-file f result)
  (if (and dirname (folder-exists dirname))
    (progn
      (setq test-file (strcat (trim-string(string-right dirname "\\")) "\\.locktest"))
      (setq f (open test-file "w"))
      (if f
        (progn
          (close f)
          (delete-file test-file)
          (setq result nil))
        (setq result T)))
    nil)
  result
)
;;; GET-DIRECTORY-FILES
;;; Lista file in directory con pattern (default "*.*")
;;; Argomenti: dir-path pattern - percorso e pattern ricerca
;;; Ritorna: lista percorsi file
(defun get-directory-files
       (dir-path pattern / temp-file result lines full-path)
  (if (not pattern)
    (setq pattern "*.*")
  )
  (if (= (string-last-char dir-path) "\\")
    (setq full-path dir-path)
    (setq full-path (strcat dir-path "\\"))
  )
  (setq temp-file (get-temp-file-path "dirfiles.txt"))
  (execute-command-with-output
    (strcat "dir /b \"" full-path "\"")
    temp-file
  )
  (sleep 1)
  (if (setq lines (read-text-file temp-file))
    (progn
      (delete-file temp-file)
      (setq result (mapcar '(lambda (f) (strcat full-path f)) lines))
    )
    (progn
      (delete-file temp-file)
      nil
    )
  )
  result
)
;;; LIST-FOLDERS
;;; Elenca cartelle in directory (usa dir batch)
;;; Argomenti: folderpath - percorso cartella
;;; Ritorna: lista nomi cartelle
(defun list-folders (folderpath / temp-file result)
  (setq temp-file (get-temp-file-path "folderlist.txt"))
  (execute-command-with-output (strcat "dir /b /ad \"" folderpath "\"") temp-file)
  (sleep 1)
  (setq result (read-text-file temp-file))
  (delete-file temp-file)
  result)
;;; LIST-FILES
;;; Elenca file in directory con estensione specifica
;;; Argomenti: folderpath extension - percorso e estensione (senza punto)
;;; Ritorna: lista nomi file
(defun list-files (folderpath extension / temp-file result pattern)
  (setq temp-file (get-temp-file-path "filelist.txt"))
  (setq pattern (strcat folderpath "\\*." extension))
  (execute-command-with-output 
    (strcat "dir /b \"" pattern "\"")
    temp-file)
  (sleep 1)
  (setq result (read-text-file temp-file))
  (delete-file temp-file)
  result
)
;;; LIST-FILES-FULL-PATH
;;; Elenca file in directory con estensione specifica (percorso completo)
;;; Argomenti: folderpath extension - percorso e estensione (senza punto)
;;; Ritorna: lista percorsi file  
(defun list-files-full-path (folderpath extension / files result)
  (setq files (list-files folderpath extension))
  (setq result '())
  (foreach file files
    (setq result (append result (list (strcat folderpath "\\" file)))))
  result
)
;;; SEARCH-FILES
;;; Ricerca ricorsiva file con pattern (usa dir /s)
;;; Argomenti: folderpath pattern - percorso e pattern
;;; Ritorna: lista file trovati
(defun search-files (folderpath pattern / temp-file result)
  (setq temp-file (get-temp-file-path "searchresult.txt"))
  (execute-command-with-output (strcat "dir /s /b \"" folderpath "\\" pattern "\"") temp-file)
  (sleep 1)
  (setq result (read-text-file temp-file))
  (delete-file temp-file)
  result)
;;; COUNT-FILES
;;; Conta file in directory con estensione specifica
;;; Argomenti: folderpath extension - percorso e estensione (senza punto)
;;; Ritorna: numero file
(defun count-files (folderpath extension / files)
  (setq files (list-files folderpath extension))
  (if files (length files) 0)
)
;;; GET-DIRECTORY-DATE
;;; Data del file più recente in directory (formattata)
;;; Argomenti: path - percorso cartella
;;; Ritorna: stringa data o nil
(defun get-directory-date (path / file-list max-time)
  (setq file-list (get-directory-files path "*.*"))
  (if file-list
    (progn
      (setq max-time (get-latest-file-date file-list))
      (if max-time (format-date-string max-time) nil))
    nil))
;;; GET-DIRECTORY-DATE-BY-EXT
;;; Data più recente filtrando per estensioni
;;; Argomenti: path ext-list - percorso e lista estensioni
;;; Ritorna: stringa data formattata
;; Esempio: 
;;;   (get-directory-date-by-ext "C:/Temp" '("dwg" "dxf")) => "15/01/2024   10:30"
(defun get-directory-date-by-ext (path ext-list / max-time ext file-list ftime)
  (setq max-time nil)
  (foreach ext ext-list
    (setq file-list (get-directory-files path (strcat "*." ext)))
    (if file-list
      (setq ftime (get-latest-file-date file-list))
      (if (and ftime (or (null max-time) (> (compare-file-times ftime max-time) 0)))
        (setq max-time ftime))))
  (if max-time (format-date-string max-time) " - "))
;;;=============================================================================
;;; 4. OPERAZIONI FILE (READ/WRITE/COPY/MOVE/DELETE/SIZE)
;;;=============================================================================
;;; READ-TEXT-FILE
;;; Legge file di testo e ritorna lista righe
;;; Argomenti: filepath - percorso file
;;; Ritorna: lista stringhe o nil
(defun read-text-file (filepath / file line result)
  (setq result '())
  (if (setq file (open filepath "r"))
    (progn
      (while (setq line (read-line file))
        (setq result (append result (list line))))
      (close file)
      result)
    nil))
;;; WRITE-TEXT-FILE
;;; Scrive lista stringhe su file
;;; Argomenti: filepath lines - percorso e lista righe
;;; Ritorna: T se successo
;;; Esempio:
;;;   (write-text-file "C:/temp/test.txt" '("Riga 1" "Riga 2" "Riga 3")) => T
(defun write-text-file (filepath lines / file)
  (if (setq file (open filepath "w"))
    (progn
      (foreach line lines
        (write-line line file))
      (close file)
      T)
    nil))
;;; DELETE-FILE
;;; Elimina file tramite comando shell
;;; Argomenti: filepath - percorso file
;;; Ritorna: -
(defun delete-file (filepath)
  (execute-command (strcat "del /f /q \"" filepath "\"")))
;;; COPY-FILE
;;; Copia file
;;; Argomenti: source dest - percorsi sorgente e destinazione
;;; Ritorna: -
(defun copy-file (source dest)
  (execute-command (strcat "copy /y \"" source "\" \"" dest "\"")))
;;; MOVE-FILE
;;; Sposta file
;;; Argomenti: source dest - percorsi sorgente e destinazione
;;; Ritorna: -
(defun move-file (source dest)
  (execute-command (strcat "move /y \"" source "\" \"" dest "\"")))
;;; RENAME-FILE
;;; Rinomina file
;;; Argomenti: filepath new-name - percorso e nuovo nome
;;; Ritorna: -
(defun rename-file (filepath new-name / folder)
  (setq folder (get-folder-from-path filepath))
  (execute-command (strcat "rename \"" filepath "\" \"" new-name "\"")))
;;; GET-FILE-SIZE
;;; Dimensione file in bytes (usa comando batch)
;;; Argomenti: filepath - percorso file
;;; Ritorna: numero bytes o nil
(defun get-file-size (filepath / temp-file result lines size-line)
  (setq temp-file (get-temp-file-path "filesize.txt"))
  (execute-command-with-output (strcat "for %I in (\"" filepath "\") do @echo %~zI") temp-file)
  (sleep 1)
  (setq result nil)
  (if (setq lines (read-text-file temp-file))
    (progn
      (setq size-line (car lines))
      (if size-line (setq result (atoi size-line))))
    (delete-file temp-file))
  result)
;;; GET-TEMP-FILE-PATH
;;; Restituisce percorso file temporaneo (env TEMP)
;;; Argomenti: nome - nome file temporaneo
;;; Ritorna: percorso completo
(defun get-temp-file-path (nome / temp-dir)
  (setq temp-dir (getenv "TEMP"))
  (if (not temp-dir) (setq temp-dir "C:\\Temp"))
  (strcat temp-dir "\\" nome))
;;;=============================================================================
;;; 5. COMANDI ESTERNI E UTILITÀ TEMPORANEE
;;;=============================================================================
;;; EXECUTE-COMMAND
;;; Esegue comando shell (startapp)
;;; Argomenti: cmd - comando da eseguire
;;; Ritorna: T
(defun execute-command (cmd)
  (startapp "cmd.exe" (strcat "/c " cmd))
  T)
;;; EXECUTE-COMMAND-WITH-OUTPUT
;;; Esegue comando e reindirizza output su file
;;; Argomenti: cmd output-file - comando e file output
;;; Ritorna: T
(defun execute-command-with-output (cmd output-file)
  (startapp "cmd.exe" (strcat "/c " cmd " > \"" output-file "\" 2>&1"))
  T)
;;; FOLDER-EXISTS
;;; Verifica esistenza cartella (usa if exist batch)
;;; Argomenti: folderpath - percorso cartella
;;; Ritorna: T o nil
(defun folder-exists (folderpath / temp-file result lines)
  (setq temp-file (get-temp-file-path "foldercheck.txt"))
  (execute-command-with-output (strcat "if exist \"" folderpath "\\*\" (echo EXISTS) else (echo NOTFOUND)") temp-file)
  (setq result nil)
  (sleep 1)
  (if (setq lines (read-text-file temp-file))
    (if (= (car lines) "EXISTS") (setq result T)))
  (delete-file temp-file)
  result)
;;; CREATE-FOLDER
;;; Crea cartella (shell mkdir)
;;; Argomenti: folderpath - percorso cartella da creare
;;; Ritorna: -
(defun create-folder (folderpath)
  (execute-command (strcat "mkdir \"" folderpath "\"")))
;;; DELETE-FOLDER
;;; Elimina cartella ricorsiva (shell rmdir)
;;; Argomenti: folderpath - percorso cartella da eliminare
;;; Ritorna: -
(defun delete-folder (folderpath)
  (execute-command (strcat "rmdir /s /q \"" folderpath "\"")))
;;; COPY-FOLDER
;;; Copia cartella ricorsiva (xcopy)
;;; Argomenti: source dest - percorsi sorgente e destinazione
;;; Ritorna: -
(defun copy-folder (source dest)
  (execute-command (strcat "xcopy \"" source "\" \"" dest "\" /e /i /h /y")))
;;;=============================================================================
;;; 5. FUNZIONI HELPER
;;;=============================================================================
;;; SLEEP
;;; Pausa esecuzione per n secondi
;;; Argomenti: sec - secondi di attesa
;;; Ritorna: nil
(defun sleep (sec / t0)
  (setq t0 (getvar "DATE"))
  (while (< (- (getvar "DATE") t0) (/ sec 86400.0))
    ;; attesa interna, nessuna finestra esterna
  )
  nil)
;;;=============================================================================
;;; INIZIALIZZAZIONE
(princ "\n_file.lsp: funzioni file caricate.") (princ)
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================