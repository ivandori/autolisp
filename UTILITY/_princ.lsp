(defun princ-to-string (val / f str)
  (setq f (open "ptos.tmp" "w"))
  (princ val f)
  (close f)

  (setq f (open "ptos.tmp" "r"))
  (setq str (read-line f))
  (close f)

  ;; opzionale: cancella il file manualmente con DOS se serve
  str
)
