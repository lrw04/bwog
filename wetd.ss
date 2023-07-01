(library (wetd)
  (export read-wetd)
  (import (util) (rnrs))

  ;; TODO
  (define special?
    (lambda (c) (memq c '(#\\ #\$ #\` #\@ #\*))))

  (define read-math
    (lambda (port)
      (let ((c (lookahead-char port)))
        (if (or (eof-object? c) (not (char=? c #\$)))
            #f
            (let ((type (cfb-port port #\$)))
              (if (or (< type 1) (> type 2))
                  (error 'read-math "invalid math opener")
                  (let ((text (read-until-k port #\$ type)))
                    `(math
                      ,(if (= type 1) 'inline 'display)
                      text))))))))

  (define read-code
    (lambda (port)
      (let ((c (lookahead-char port)))
        (if (or (eof-object? c) (not (char=? c #\`)))
            #f
            (let ((backticks (cfb-port port #\`)))
              (let ((text (read-until-k port #\` backticks)))
                `(code ,text)))))))

  (define read-escaped
    (lambda (port)
      (let ((c (lookahead-char port)))
        (if (or (eof-object? c) (not (char=? c #\\)))
            #f
            (let* ((directive (get-char port))
                   (c (get-char port)))
              (string c))))))
  
  (define read-link
    (lambda (port)
      #f))

  (define read-emph
    (lambda (port)
      #f))
  
  (define read-text
    (lambda (port)
      (define read-text-iter
        (lambda (port acc)
          (let ((c (lookahead-char port)))
            (if (or (eof-object? c) (special? c))
                (if (null? acc)
                    #f
                    (apply string (reverse acc)))
                (read-text-iter port (cons (get-char port) acc))))))
      (read-text-iter port '())))

  (define read-inline
    (lambda (port)
      (define read-inline-iter
        (lambda (port acc)
          (cond
           ((read-math port) => (lambda (node) (read-inline-iter port (cons node acc))))
           ((read-code port) => (lambda (node) (read-inline-iter port (cons node acc))))
           ((read-escaped port) => (lambda (node) (read-inline-iter port (cons node acc))))
           ((read-link port) => (lambda (node) (read-inline-iter port (cons node acc))))
           ((read-emph port) => (lambda (node) (read-inline-iter port (cons node acc))))
           ((read-text port) => (lambda (node) (read-inline-iter port (cons node acc))))
           (else (reverse acc)))))
      (read-inline-iter port '())))

  (define title?
    (lambda (line)
      (let ((hash-count (count-from-beginning line #\#)))
        (and (> hash-count 0)
             (> (string-length line) hash-count)
             (char=? #\space (string-ref line hash-count))))))

  (define read-title
    (lambda (line)
      (let* ((hash-count (count-from-beginning line #\#))
             (title (substring line (+ 1 hash-count) (string-length line)))
             (port (open-string-input-port title))
             (node `(title ,hash-count ,(read-inline port))))
        (close-input-port port)
        node)))

  (define codeblock?
    (lambda (line)
      (>= (count-from-beginning line #\`) 3)))

  (define codeblock-end?
    (lambda (line backticks)
      (string=? line (make-string #\` backticks))))

  (define codeblock-lang
    (lambda (line)
      (substring line (count-from-beginning line #\`) (string-length line))))

  (define read-codeblock
    (lambda (port type)
      (let ((backticks (count-from-beginning type #\`)))
        (define rcb-iter
          (lambda (port type acc)
            (let ((current-line (get-line port)))
              (if (eof-object? current-line)
                  `(codeblock ,(codeblock-lang type) ,(join-lines (reverse acc)))
                  (if (codeblock-end? current-line backticks)
                      `(codeblock ,(codeblock-lang type) ,(join-lines (reverse acc)))
                      (rcb-iter port type (cons current-line acc)))))))
        (rcb-iter port type '()))))

  (define div?
    (lambda (line)
      (and (> (string-length line) 0)
           (not (string=? line ":end"))
           (char=? (string-ref line 0) #\:))))

  (define div-end?
    (lambda (line)
      (string=? line ":end")))

  (define read-par
    (lambda (port line)
      (define read-par-iter
        (lambda (port acc)
          (let* ((c (lookahead-char port))
                 (special (memq c '(#\: #\# #\` #\newline))))
            (if (or (eof-object? c) (special))
                (let* ((port (open-string-input-port (join-lines (reverse acc))))
                       (node `(par ,(read-inline port))))
                  (close-input-port port)
                  node)
                (read-par-iter port (cons (get-line port) acc))))))
      (read-par-iter port (list line))))
  
  (define read-div
    (lambda (port type)
      (define read-div-iter
        (lambda (port type acc)
          (let ((current-line (get-line port)))
            (if (eof-object? current-line)
                `(div ,(substring type 1 (string-length type)) ,(reverse acc))
                (cond
                 ((title? current-line)
                  (read-div-iter port
                                 type
                                 (cons (read-title current-line) acc)))
                 ((codeblock? current-line)
                  (read-div-iter port
                                 type
                                 (cons (read-codeblock port current-line)
                                       acc)))
                 ((div-end? current-line)
                  `(div ,(substring type 1 (string-length type)) ,(reverse acc)))
                 ((div? current-line)
                  (read-div-iter port type (cons (read-div port current-line)
                                                 acc)))
                 ((string=? current-line "") (read-div-iter port type acc))
                 (else
                  (read-div-iter port
                                 type
                                 (cons (read-par port current-line) acc))))))))
      (read-div-iter port type '())))
  
  (define read-wetd
    (lambda (port)
      (read-div port ":document"))))
