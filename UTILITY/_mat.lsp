;;;=============================================================================
;;; FUNZIONI MATEMATICHE
;;; Ultimo aggiornamento: 12/01/2026
;;; AUTORE: Ivano Dorigatti
;;; LICENZA: MIT License
;;;=============================================================================
;;; Compatibile: ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)
;;; Descrizione: funzioni per operazioni matematiche
;;; Dipendenze esterne: STRING-CONTAINS (_stringhe.lsp) SORT-NUMBER-LIST (_liste.lsp)
;;; Tutte le funzioni sono documentate. Evitate funzioni VL non compatibili con ProgeCAD.
;;;=============================================================================
;;; INDICE
;;;=============================================================================
;;; 1. CONTROLLO SEGNO E TIPO
;;; 2. ARROTONDAMENTO
;;; 3. ESTRAZIONE PARTI NUMERICHE
;;; 4. PARITÀ E MODULO
;;; 5. INTERPOLAZIONE
;;; 6. CONFRONTO E SELEZIONE
;;;=============================================================================
;;; 1. CONTROLLO SEGNO E TIPO
;;;=============================================================================
;;; SAME-SIGN
;;; Verifica se due numeri hanno stesso segno
;;; Argomenti:
;;;   a - Primo numero
;;;   b - Secondo numero
;;; Ritorna: T se stesso segno, nil altrimenti
(defun same-sign (a b)
  (or (and (minusp a) (minusp b))
      (and (not (minusp a)) (not (minusp b)))))
;;; IS-POSITIVE
;;; Verifica se numero è positivo (>0)
;;; Argomenti:
;;;   n - Numero da verificare
;;; Ritorna: T se positivo, nil altrimenti
(defun is-positive (n)
  (not (or (minusp n) (zerop n))))
;;; IS-REAL-TYPE
;;; Verifica se numero è tipo REAL (ha decimali)
;;; Argomenti:
;;;   n - Numero da verificare
;;; Ritorna: T se ha decimali, nil se intero
(defun is-real-type (n)
  (if (numberp n)
    (if (string-contains "." (rtos n))
      T
      nil)
    nil))
;;; HAS-DECIMAL-PART
;;; Verifica se numero ha parte decimale non zero
;;; Argomenti:
;;;   n - Numero da verificare
;;; Ritorna: T se ha decimali, nil altrimenti
(defun has-decimal-part (n)
  (if (< n 1)
    T
    (if (/= n (fix n))
      T
      nil)))
;;;=============================================================================
;;; 2. ARROTONDAMENTO
;;;=============================================================================
;;; ROUND-NUMBER
;;; Arrotonda numero reale all'intero più vicino
;;; Argomenti:
;;;   r - Numero reale
;;; Ritorna: Numero intero arrotondato
;;; Esempio:
;;;   (round-number 3.4) => 3
;;;   (round-number 3.6) => 4
(defun round-number (r / n d)
  (setq n (fix r)
        d (- r n))
  (if (>= d 0.5)
    (1+ n)
    n))
;; Arrotondamento simmetrico (away from zero)
(defun round-sym (num)
  (if (minusp num)
    (fix (- num 0.5))
    (fix (+ num 0.5))
  )
)
;;; ROUND-UP
;;; Arrotonda sempre per eccesso
;;; Argomenti:
;;;   r - Numero reale
;;; Ritorna: Intero superiore
;;; Esempio:
;;;   (round-up 3.1) => 4
;;;   (round-up 3.0) => 3
(defun round-up (r / n d)
  (setq n (fix r)
        d (- r n))
  (if (> d 0)
    (1+ n)
    n))
;;; ROUND-DOWN
;;; Arrotonda sempre per difetto
;;; Argomenti:
;;;   r - Numero reale
;;; Ritorna: Intero inferiore
(defun round-down (r)
  (fix r))
;; Arrotondamento verso l'infinito negativo (floor)
(defun round-floor (num)
  (if (= num (fix num))
    (fix num)
    (if (minusp num)
      (1- (fix num))
      (fix num)
    )
  )
)
;;; ROUND-TO-DECIMAL
;;; Arrotonda a N decimali
;;; Argomenti:
;;;   num - Numero
;;;   decimals - Numero decimali
;;; Ritorna: Numero arrotondato
;;; Esempio:
;;;   (round-to-decimal 3.14159 2) => 3.14
(defun round-to-decimal (num decimals / factor)
  (setq factor (expt 10.0 decimals))
  (/ (float (round-number (* num factor))) factor))
;;;=============================================================================
;;; 3. ESTRAZIONE PARTI NUMERICHE
;;;=============================================================================
;;; GET-DECIMAL-PART
;;; Estrae parte decimale di un numero
;;; Argomenti:
;;;   r - Numero reale
;;; Ritorna: Parte decimale
;;; Esempio:
;;;   (get-decimal-part 3.75) => 0.75
(defun get-decimal-part (r / i)
  (setq i (fix r))
  (- r i))
;;; GET-INTEGER-PART
;;; Estrae parte intera
;;; Argomenti:
;;;   r - Numero reale
;;; Ritorna: Parte intera
(defun get-integer-part (r)
  (fix r))
;;;=============================================================================
;;; 4. PARITÀ E MODULO
;;;=============================================================================
;;; IS-EVEN
;;; Verifica se numero è pari
;;; Argomenti:
;;;   n - Numero intero
;;; Ritorna: T se pari, nil se dispari
(defun is-even (n)
  (if (= (rem n 2) 0)
    T
    nil))
;;; IS-ODD
;;; Verifica se numero è dispari
;;; Argomenti:
;;;   n - Numero intero
;;; Ritorna: T se dispari, nil se pari
(defun is-odd (n)
  (not (is-even n)))
;;;=============================================================================
;;; 5. INTERPOLAZIONE
;;;=============================================================================
;;; LINEAR-INTERPOLATE
;;; Interpolazione lineare tra due punti
;;; Argomenti:
;;;   x - Valore X da interpolare
;;;   x1 - X punto 1
;;;   y1 - Y punto 1
;;;   x2 - X punto 2
;;;   y2 - Y punto 2
;;; Ritorna: Valore Y interpolato
;;; Esempio:
;;;   (linear-interpolate 5 0 0 10 100) => 50.0
(defun linear-interpolate (x x1 y1 x2 y2 / y)
  (if (and (numberp x) (numberp x1) (numberp y1) 
           (numberp x2) (numberp y2))
    (if (and (/= y2 y1) (/= x x1) (/= x2 x1))
      (setq y (+ y1 (/ (* (- y2 y1) (- x x1)) (- x2 x1))))
      y1)
    nil))
;;; INTERPOLATE-LIST
;;; Interpola valore in lista di coppie (x . y)
;;; Argomenti:
;;;   x - Valore da interpolare
;;;   pairs - Lista coppie ((x1 . y1) (x2 . y2) ...)
;;; Ritorna: Y interpolato
(defun interpolate-list (x pairs / prev-pair result)
  (setq result nil
        prev-pair nil)
  (foreach pair pairs
    (if (and prev-pair (null result))
      (if (and (<= (car prev-pair) x) (<= x (car pair)))
        (setq result (linear-interpolate x
                       (car prev-pair) (cdr prev-pair)
                       (car pair) (cdr pair)))))
    (setq prev-pair pair))
  result)
;;;=============================================================================
;;; 6. CONFRONTO E SELEZIONE
;;;=============================================================================
;;; FIND-NEXT-HIGHER
;;; Trova numero immediatamente superiore in lista
;;; Argomenti:
;;;   num - Numero di riferimento
;;;   num-list - Lista numeri
;;; Ritorna: Numero più vicino superiore
;;; Esempio:
;;;   (find-next-higher 5 '(1 3 7 9)) => 7
(defun find-next-higher (num num-list / result found)
  (setq result (find-max num-list)
        found nil)
  (foreach n num-list
    (if (and (null found) (> n num))
      (setq result n
            found T)))
  result)
;;; FIND-NEXT-LOWER
;;; Trova numero immediatamente inferiore in lista
;;; Argomenti:
;;;   num - Numero di riferimento
;;;   num-list - Lista numeri
;;; Ritorna: Numero più vicino inferiore
;;; Esempio:
;;;   (find-next-lower 5 '(1 3 7 9)) => 3
(defun find-next-lower (num num-list / result found sorted)
  (setq sorted (sort-number-list num-list)
        result (car sorted)
        found nil)
  (foreach n (reverse sorted)
    (if (and (null found) (< n num))
      (setq result n
            found T)))
  result)
;;; CLAMP
;;; Limita numero tra min e max
;;; Argomenti:
;;;   value - Valore
;;;   min-val - Valore minimo
;;;   max-val - Valore massimo
;;; Ritorna: Valore clampato
;;; Esempio:
;;;   (clamp 15 0 10) => 10
(defun clamp (value min-val max-val)
  (cond
    ((< value min-val) min-val)
    ((> value max-val) max-val)
    (t value)))
;;; MAP-RANGE
;;; Mappa valore da un range ad altro range
;;; Argomenti:
;;;   value - Valore da mappare
;;;   in-min, in-max - Range input
;;;   out-min, out-max - Range output
;;; Ritorna: Valore mappato
;;; Esempio:
;;;   (map-range 5 0 10 0 100) => 50
;;;   (map-range 75 50 100 0 1) => 0.5
;;;   (map-range 25 0 100 200 300) => 225
(defun map-range (value in-min in-max out-min out-max)
  (+ out-min 
     (* (/ (- value in-min) (- in-max in-min))
        (- out-max out-min))))

(princ)
;;;=============================================================================
;;; FINE FILE
;;;=============================================================================