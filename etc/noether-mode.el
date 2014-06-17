(require 'generic-x) ;; we need this

(define-generic-mode
    'noether-mode                     ;; name of the mode to create
  '() ;; no comments!
  '("if" "match" "with"
    "end" "define" "->" ":=" ":"
    "->" "=>")                  ;; some keywords
  '(("=" . 'font-lock-operator)     ;; '=' is an operator
    ("*" . 'font-lock-operator)
    ("@" . 'font-lock-operator)
    ("/" . 'font-lock-operator)
    ("-" . 'font-lock-operator)
    ("+" . 'font-lock-operator))
  '("\\.sp$" "\\.emmy$")            ;; files for which to activate this mode
  nil                               ;; other functions to call
  "A mode for noether files"          ;; doc string for this mode
  )

(provide 'noether-mode)
