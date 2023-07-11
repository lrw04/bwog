(library (html)
  (export html>> xml>>)
  (import (util) (rnrs))

  (define html-void-elements
    '(area base br col embed hr img input link meta param source track wbr))

  (define alist->props
    (lambda (alist)
      (apply string-append
             (map (lambda (p)
                    (string-append " "
                                   (symbol->string (car p))
                                   "=\""
                                   (string-escape (cdr p))
                                   "\""))
                  alist))))

  (define make-ml-writer
    (lambda (void-elements)
      (letrec ((ml-writer (lambda (tree port)
                            (cond ((char? tree)
                                   (display (xml-escape (string tree)) port))
                                  ((string? tree) ; "string"
                                   (display (xml-escape tree) port))
                                  ((and (pair? tree) (eq? (car tree) 'raw)) ; (raw "string")
                                   (display (cadr tree) port))
                                  ((and (pair? tree) (memq (car tree) void-elements)) ; (void-element props)
                                   (let ((tag (car tree))
                                         (props (alist->props (cadr tree))))
                                     (display "<" port)
                                     (display tag port)
                                     (display props port)
                                     (display ">" port)))
                                  ((pair? tree) ; (element props children)
                                   (let ((tag (car tree))
                                         (props (alist->props (cadr tree)))
                                         (children (caddr tree)))
                                     (display "<" port)
                                     (display tag port)
                                     (display props port)
                                     (display ">" port)
                                     (for-each (lambda (t) (ml-writer t port)) children)
                                     (display "</" port)
                                     (display tag port)
                                     (display ">" port)))
                                  (else (error 'html>> "not a character, string, or tree node" tree))))))
        ml-writer)))
            
  (define html>> (make-ml-writer html-void-elements))
  (define xml>> (make-ml-writer '())))
