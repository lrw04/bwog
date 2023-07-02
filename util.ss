(library (util)
  (export xml-escape
          tex-escape
          count-from-beginning
          join-lines
          cfb-port
          read-until
          assert-with-msg
          read-until-k)
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

  (define cfb-port
    (lambda (port c)
      (define cfb-port-iter
        (lambda (port c count)
          (let ((e (lookahead-char port)))
            (if (char=? c e)
                (begin (get-char port) (cfb-port-iter port c (+ count 1)))
                count))))
      (cfb-port-iter port c 0)))

  (define join-lines
    (lambda (lines)
      (apply string-append (map (lambda (s) (string-append s (string #\newline))) lines))))

  (define read-until
    (lambda (port c)
      (define read-until-iter
        (lambda (port c acc)
          (let ((e (lookahead-char port)))
            (if (char=? e c)
                (apply string (reverse acc))
                (read-until-iter port c (cons (read-char port) acc))))))
      (read-until-iter port c '())))

  (define assert-with-msg
    (lambda (p who message . irritants)
      (if (not p)
          (apply assertion-violation who message irritants))))

  (define apply-k
    (lambda (proc k arg)
      (if (= k 0) arg (proc (apply-k proc (- k 1) arg)))))
  
  (define read-until-k
    (lambda (port c k)
      (define read-until-k-iter
        (lambda (port c k acc count)
          (let ((e (lookahead-char port)))
            (cond ((= k count) (apply string (reverse (apply-k cdr k acc))))
                  ((eof-object? e) (apply string (reverse acc)))
                  ((char=? e c) (read-until-k-iter port c k (cons (get-char port) acc) (+ count 1)))
                  (else (read-until-k-iter port c k (cons (get-char port) acc) 0))))))
      (read-until-k-iter port c k '() 0))))
