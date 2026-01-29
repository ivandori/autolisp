;;;=============================================================================
;;; 7. FUNZIONI DIMOSTRATIVE
;;;=============================================================================
;;; DEMO-MATH-ROUND
;;; Dimostra arrotondamenti
(defun c:demo-math-round (/ nums)
  (princ "\n=== DEMO ARROTONDAMENTI ===")
  (setq nums '(3.2 3.5 3.8 -2.3 -2.7))
  (princ "\n\nNumeri: 3.2  3.5  3.8  -2.3  -2.7")
  (princ "\n\nRound standard:")
  (foreach n nums
    (princ (strcat " " (itoa (round-number n)))))
  (princ "\n\nRound sym")
  (foreach n nums
    (princ (strcat " " (itoa (round-sym n)))))
  (princ "\n\nRound up:")
  (foreach n nums
    (princ (strcat " " (itoa (round-up n)))))
  (princ "\n\nRound down:")
  (foreach n nums
    (princ (strcat " " (itoa (round-down n)))))
  (princ "\n\nRound floor:")
  (foreach n nums
    (princ (strcat " " (itoa (round-floor n)))))  
  (princ "\n\nArrotonda 3.14159 a 2 decimali:")
  (princ (strcat " " (rtos (round-to-decimal 3.14159 2) 2 4)))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-MATH-INTERP
;;; Dimostra interpolazione
(defun c:demo-math-interp (/ y)
  (princ "\n=== DEMO INTERPOLAZIONE ===")
  (princ "\n\nPunti: (0, 0) e (10, 100)")
  (princ "\n\nInterpolazioni:")
  (setq y (linear-interpolate 0 0 0 10 100))
  (princ (strcat "\n  x=0  => y=" (rtos y 2 2)))
  (setq y (linear-interpolate 5 0 0 10 100))
  (princ (strcat "\n  x=5  => y=" (rtos y 2 2)))
  (setq y (linear-interpolate 10 0 0 10 100))
  (princ (strcat "\n  x=10 => y=" (rtos y 2 2)))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-MATH-COMPARE
;;; Dimostra confronti
(defun c:demo-math-compare (/ nums next-high next-low)
  (princ "\n=== DEMO CONFRONTI ===")
  (setq nums '(1 5 10 15 20 25 30))
  (princ (strcat "\n\nLista: " 
                (list-to-string (number-list-to-string-list nums) " ")))
  (setq next-high (find-next-higher 12 nums))
  (princ (strcat "\n\nProssimo > 12: " (itoa next-high)))
  (setq next-low (find-next-lower 12 nums))
  (princ (strcat "\nProssimo < 12: " (itoa next-low)))
  (princ "\n\nClamp 17 tra 10 e 20:")
  (princ (strcat " " (itoa (clamp 17 10 20))))
  (princ "\nClamp 5 tra 10 e 20:")
  (princ (strcat " " (itoa (clamp 5 10 20))))
  (princ "\nClamp 25 tra 10 e 20:")
  (princ (strcat " " (itoa (clamp 25 10 20))))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-MATH-PARITY
;;; Dimostra parità
(defun c:demo-math-parity (/ nums)
  (princ "\n=== DEMO PARITA' ===")
  (setq nums '(1 2 3 4 5 6 7 8 9 10))
  (princ "\n\nNumeri pari:")
  (foreach n nums
    (if (is-even n)
      (princ (strcat " " (itoa n)))))
  (princ "\n\nNumeri dispari:")
  (foreach n nums
    (if (is-odd n)
      (princ (strcat " " (itoa n)))))
  (princ "\n\n=== Demo completata ===")
  (princ))
;;; DEMO-TUTTO-MATH
;;; Esegue tutte le demo matematiche
(defun c:demo-math ()
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|  FUNZIONI MATEMATICHE - DIMOSTRAZIONE COMPLETA        |")
  (princ "\n+========================================================+")
  (princ "\n")
  (c:demo-math-round)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-math-interp)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-math-compare)
  (princ "\n\nPremi Invio per continuare...") (getstring)
  (c:demo-math-parity)
  (princ "\n")
  (princ "\n+========================================================+")
  (princ "\n|    TUTTE LE DEMO MATEMATICHE COMPLETATE               |")
  (princ "\n+========================================================+")
  (princ))
