(library (wetd)
  (export read-wetd)
  (import (util) (rnrs))

  ;; read one inline element from port
  (define read-inline
    (lambda (port end)
      (let ((c (get-char port)))
        (cond ((eof-object? c) c)
              ((equal? end c)  (eof-object))
              ((char=? c #\@) (let ((c (lookahead-char port)))
                                (case c
                                  ((#\@ #\` #\*) c)
                                  (else (let* ((datum (get-datum port))
                                               (c (lookahead-char port)))
                                          (case c
                                            ((#\`)
                                             (let ((count (cfb-port port #\`)))
                                               (append datum (list (read-until-k port #\` count)))))
                                            ((#\*)
                                             (get-char port)
                                             (append datum
                                                     (list (read-inline-from-port port #\*))))
                                            (else datum)))))))
              (else c)))))

  (define read-inline-from-port
    (lambda (port end)
      (letrec ((iter (lambda (acc)
                       (let ((datum (read-inline port end)))
                         (cond ((eof-object? datum) (reverse acc))
                               (else (iter (cons datum acc))))))))
        (iter '()))))

  (define read-inline-from-string
    (lambda (s)
      (let* ((port (open-string-input-port s))
             (data (read-inline-from-port port #f)))
        (close-input-port port)
        data)))

  (define make-startp
    (lambda (c)
      (lambda (line)
        (> (count-from-beginning line c) 0))))

  (define make-endline
    (lambda (c)
      (lambda (line)
        (make-string (count-from-beginning line c) c))))

  (define get-param
    (lambda (line-w/o-start)
      (let* ((port (open-string-input-port line-w/o-start))
             (datum (get-datum port))
             (s (get-string-all port)))
        (close-input-port port)
        (cons datum s))))

  (define container-start? (make-startp #\:))
  (define container-end-line (make-endline #\:))
  (define raw-start? (make-startp #\`))
  (define raw-end-line (make-endline #\`))
  (define leaf? (make-startp #\!))

  (define make-block-param
    (lambda (c)
      (lambda (line)
        (car (get-param (substring line
                                   (count-from-beginning line c)
                                   (string-length line)))))))

  (define container-param (make-block-param #\:))
  (define raw-param (make-block-param #\`))

  (define read-raw
    (lambda (port end-line param)
      (letrec ((read-raw-iter
                (lambda (acc)
                  (let ((line (get-line port)))
                    (cond ((eof-object? line) (append param (list (join-lines (reverse acc)))))
                          ((string=? line end-line) (append param (list (join-lines (reverse acc)))))
                          (else (read-raw-iter (cons line acc))))))))
        (read-raw-iter '()))))

  (define read-leaf
    (lambda (line)
      (let* ((line-w/o-start (substring line
                                        (count-from-beginning line #\!)
                                        (string-length line)))
             (data (get-param line-w/o-start))
             (param (car data))
             (children (read-inline-from-string (cdr data))))
        (append param (list children)))))

  (define read-par
    (lambda (port line)
      (letrec ((read-par-iter (lambda (acc)
                                (let ((c (lookahead-char port)))
                                  (case c
                                    ((#\: #\` #\! #\newline)
                                     `(par
                                       ,(read-inline-from-string (join-lines (reverse acc)))))
                                    (else (read-par-iter (cons (get-line port) acc))))))))
        (read-par-iter (list line)))))
  
  (define read-container
    (lambda (port end-line param)
      (letrec ((read-container-iter
                (lambda (acc)
                  (let ((line (get-line port)))
                    (cond ((eof-object? line) (append param (list (reverse acc))))
                          ((equal? line end-line) (append param (list (reverse acc))))
                          ((container-start? line)
                           (read-container-iter (cons (read-container port
                                                                      (container-end-line line)
                                                                      (container-param line))
                                                      acc)))
                          ((raw-start? line)
                           (read-container-iter (cons (read-raw port
                                                                (raw-end-line line)
                                                                (raw-param line))
                                                      acc)))
                          ((leaf? line)
                           (read-container-iter (cons (read-leaf line) acc)))
                          ((string=? line "") (read-container-iter acc))
                          (else (read-container-iter (cons (read-par port line) acc))))))))
        (read-container-iter '()))))

  (define read-wetd
    (lambda (port)
      (read-container port #f '(document)))))
