(library (util)
  (export xml-escape tex-escape count-from-beginning join-lines)
  (import (rnrs))
  (define escape                        
    (lambda (c table)                   
      (let ((r (assq c table)))         
        (if r                           
            (cdr r)                     
            (string c)))))

  (define escape-string
    (lambda (s table)
      (apply string-append
             (map (lambda (c) (escape c table))
                  (string->list s)))))
  
  (define xml-escape                    
    (lambda (s)                           
      (escape-string s '((#\< . "&lt;") 
                         (#\> . "&gt;")                        
                         (#\& . "&amp;")))))
  
  (define tex-escape                    
    (lambda (s)                           
      (escape-string s '((#\# . "\\#") 
                         (#\$ . "\\$")                        
                         (#\% . "\\%")
                         (#\& . "\\&")
                         (#\{ . "\\{")
                         (#\} . "\\}")
                         (#\_ . "\\_")
                         (#\^ . "\\^{}")
                         (#\~ . "\\~{}")
                         (#\\ . "\\textbackslash{}")))))
  
  (define count-from-beginning
    (lambda (line c)
      (define cfb-iter
        (lambda (line c beginning count)
          (if (> (string-length line) beginning)
              (if (char=? (string-ref line beginning) c)
                  (cfb-iter line c (+ beginning 1) (+ count 1))
                  count)
              count)))
      (cfb-iter line c 0 0)))

  (define join-lines
    (lambda (lines)
      (apply string-append (map (lambda (s) (string-append s (string #\newline))) lines)))))

(library (wetd)
  (export read-wetd)
  (import (util) (rnrs))

  ;; TODO
  (define (read-inline port) (get-string-all port))

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

(library (html)
  (export html->string wetd->html)
  (import (rnrs))
  
  ;; TODO
  (define html->string
    (lambda (s) s))

  (define wetd->html
    (lambda (s) s)))
