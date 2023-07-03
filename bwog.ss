;;;; top level program
(import (wetd) (html) (rnrs))

(define generate
  (lambda (repo)
    '()))

(generate (cadr (command-line)))
