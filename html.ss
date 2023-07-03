(library (html)
  (export void-elements
          alist->props
          html>>)
  (import (util) (rnrs))

  (define void-elements
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
  
  (define html>>
    (lambda (tree port)
      (cond ((string? tree)             ; "string"
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
            ((pair? tree); (element props children)
             (let ((tag (car tree))
                   (props (alist->props (cadr tree)))
                   (children (caddr tree)))
               (display "<" port)
               (display tag port)
               (display props port)
               (display ">" port)
               (for-each html>> children (make-list (length children) port))
               (display "</" port)
               (display tag port)
               (display ">" port)))
            (else (error 'html>> "not a string or tree node"))))))
