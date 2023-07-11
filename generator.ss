(library (generator)
  (export read-repo
          generate-repo
          read-config
          use-parent
          use-child
          use-appended
          use-string-append
          make-tag-pred
          default-config
          default-base
          file-metadata
          file-filename
          file-date
          file-title
          config-title
          make-latex-macros
          make-katex-macros)
  (import (wetd) (rnrs) (html) (util) (rnrs eval))

  (define use-parent
    (lambda (parent child) (or parent child)))

  (define use-child
    (lambda (parent child) (or child parent)))

  (define use-appended
    (lambda (parent child) (append (or child '()) (or parent '()))))

  (define use-string-append
    (lambda (parent child)
      (string-append (or child "") (or parent ""))))

  (define read-config
    (lambda (alist key)
      (cdr (or (assoc key alist) (cons #f #f)))))

  ;; rule function: parent -> child -> config 
  (define compose-config
    (lambda (parent child rules)
      (if (null? rules)
          '()
          (let* ((rule (car rules))
                 (other (compose-config parent child (cdr rules)))
                 (rule-f (cdr rule))
                 (key (car rule))
                 (merged (apply rule-f (read-config parent key) (read-config child key) '())))
            (cons (cons key merged) other)))))
  
  (define default-propagation-rules
    `((latex-macros . ,use-appended)
      (title . ,use-child)
      (processors . ,use-appended)
      (template . ,use-child)
      (index-template . ,use-child)
      (link . ,use-child)
      (desc . ,use-child)
      (preamble . ,use-string-append)
      (date . ,use-child)))

  (define make-tag-pred
    (lambda (tag)
      (lambda (x)
        (and (pair? x) (equal? (car x) tag)))))

  (define process-subtree
    (lambda (config l)
      (if (string? l) (list l) (map (lambda (x) (process config x)) l))))

  (define make-simple-tag
    (lambda (tag)
      (lambda (config t)
        `(,tag () ,(process-subtree config (cadr t))))))

  (define h-proc
    (lambda (config t)
      `(,(string->symbol (string-append "h" (number->string (cadr t))))
        ()
        ,(process-subtree config (caddr t)))))

  (define ol-proc (make-simple-tag 'ol))
  (define document-proc (make-simple-tag 'article))

  (define hr-proc
    (lambda (config t)
      `(hr ())))

  (define figure-proc (make-simple-tag 'figure))
  (define par-proc (make-simple-tag 'p))

  (define img-proc
    (lambda (config t)
      `(img ((src . ,(cadr t))
             (alt . ,(caddr t))))))

  (define code-proc
    (lambda (config t)
      `(pre ()
            ((code ((class . ,(if (null? (cddr t)) "" (string-append "language-" (symbol->string (cadr t))))))
                   ,(process-subtree config (if (null? (cddr t)) (cadr t) (caddr t))))))))

  (define quote-proc (make-simple-tag 'blockquote))
  (define item-proc (make-simple-tag 'li))
  (define ul-proc (make-simple-tag 'ul))
  (define sub-proc (make-simple-tag 'sub))
  (define sup-proc (make-simple-tag 'sup))
  (define st-proc (make-simple-tag 'del))

  (define link-proc
    (lambda (config t)
      `(a ((href . ,(cadr t))) ,(process-subtree config (caddr t)))))

  (define verb-proc (make-simple-tag 'code))
  (define uline-proc (make-simple-tag 'u))
  (define em-proc (make-simple-tag 'em))
  (define s-proc (make-simple-tag 'strong))
  
  (define math-proc
    (lambda (config t)
      `(span ((class . "math inline"))
             ,(process-subtree config (cadr t)))))

  (define dmath-proc
    (lambda (config t)
      `(span ((class . "math display"))
             ,(process-subtree config (cadr t)))))

  (define default-config
    `((title . "default title")
      (processors . ((,(make-tag-pred 'h) . ,h-proc)
                     (,(make-tag-pred 'ol) . ,ol-proc)
                     (,char? . ,(lambda (proc x) (string x)))
                     (,string? . ,(lambda (proc x) x))
                     (,(make-tag-pred 'document) . ,document-proc)
                     (,(make-tag-pred 'hr) . ,hr-proc)
                     (,(make-tag-pred 'figure) . ,figure-proc)
                     (,(make-tag-pred 'par) . ,par-proc)
                     (,(make-tag-pred 'img) . ,img-proc)
                     (,(make-tag-pred 'code) . ,code-proc)
                     (,(make-tag-pred 'quote) . ,quote-proc)
                     (,(make-tag-pred 'item) . ,item-proc)
                     (,(make-tag-pred 'ul) . ,ul-proc)
                     (,(make-tag-pred 'sub) . ,sub-proc)
                     (,(make-tag-pred 'sup) . ,sup-proc)
                     (,(make-tag-pred 'link) . ,link-proc)
                     (,(make-tag-pred 'verb) . ,verb-proc)
                     (,(make-tag-pred 'st) . ,st-proc)
                     (,(make-tag-pred 'uline) . ,uline-proc)
                     (,(make-tag-pred 'em) . ,em-proc)
                     (,(make-tag-pred 's) . ,s-proc)
                     (,(make-tag-pred 'math) . ,math-proc)
                     (,(make-tag-pred 'dmath) . ,dmath-proc)))))
 
  (define read-repo
    (lambda (path)
      (let* ((metadata
              (eval (read-datum-from-file (join-path path
                                                     "metadata.ss"))
                    (environment '(rnrs) '(generator) '(wetd) '(util) '(html) '(chezscheme))))
             (propagation-rules (append (or (read-config metadata 'propagation-rules) '())
                                        default-propagation-rules))
             (config (compose-config default-config (cdr (assoc 'config metadata)) propagation-rules))
             (files (cdr (assoc 'files metadata))))
        `((config . ,config)
          (files . ,(map (lambda (file)
                           `((filename . ,file)
                             (document-tree . ,(read-wetd-from-file (join-path path
                                                                               (string-append file
                                                                                              ".wetd"))))
                             (metadata . ,(compose-config config
                                                          (eval
                                                           (read-datum-from-file (join-path path
                                                                                            (string-append file
                                                                                                           ".wetd")))
                                                           (environment '(rnrs)
                                                                        '(generator)
                                                                        '(wetd)
                                                                        '(util)
                                                                        '(html)
                                                                        '(chezscheme)))
                                                          propagation-rules))))
                         files))))))

  (define file-filename
    (lambda (file)
      (cdr (assoc 'filename file))))

  (define file-metadata
    (lambda (file)
      (cdr (assoc 'metadata file))))

  (define file-date
    (lambda (file)
      (cdr (assoc 'date (file-metadata file)))))

  (define file-title
    (lambda (file)
      (cdr (assoc 'title (file-metadata file)))))

  (define file-template
    (lambda (file)
      (cdr (assoc 'template (file-metadata file)))))

  (define file-document-tree
    (lambda (file)
      (cdr (assoc 'document-tree file))))

  (define config-title
    (lambda (config)
      (cdr (assoc 'title config))))

  (define default-base
    (lambda (head body)
      `(html ()
             ((head ()
                    ((meta ((charset . "utf-8")))
                     (meta ((name . "viewport")
                            (content . "width=device-width, initial-scale=1")))
                     ,@head))
              (body ()
                    ,body)))))

  (define generate-index
    (lambda (config files path)
      (let ((port (open-file-output-port (join-path path "index.html")
                                         (file-options no-fail)
                                         'block
                                         (make-transcoder (utf-8-codec)))))
        (display "<!doctype html>" port)
        (html>> ((cdr (assoc 'index-template config)) config files)
                port)
        (close-output-port port))))

  (define generate-rss
    (lambda (config files path)
      (let ((port (open-file-output-port (join-path path "feed.xml")
                                         (file-options no-fail)
                                         'block
                                         (make-transcoder (utf-8-codec)))))
        (xml>> `(rss ((version . "2.0"))
                     ((channel ()
                               ((title () (,(cdr (assoc 'title config))))
                                (link () (,(cdr (assoc 'link config))))
                                (description () (,(cdr (assoc 'desc config))))
                                ,@(map (lambda (file)
                                         `(item ()
                                                ((title ()
                                                        (,(file-title file)))
                                                 (link ()
                                                       (,(string-append (cdr (assoc 'link config))
                                                                        (file-filename file)
                                                                        ".html"))))))
                                       files)))))
               port)
        (close-output-port port))))

  (define process
    (lambda (config tree)
      ((cdr (or (assp (lambda (pred) (pred tree)) (cdr (assoc 'processors config)))
                (error 'process "unknown directive" tree))) config tree)))

  (define generate-page
    (lambda (config file path)
      (let ((port (open-file-output-port (join-path path (string-append (file-filename file) ".html"))
                                         (file-options no-fail)
                                         'block
                                         (make-transcoder (utf-8-codec)))))
        (display "<!doctype html>" port)
        (html>> ((file-template file)
                 config
                 (process config (file-document-tree file)))
                port)
        (close-output-port port))))

  (define generate-repo
    (lambda (repo path)
      (let ((config (cdr (assoc 'config repo)))
            (files (cdr (assoc 'files repo))))
        (generate-index config files path)
        (generate-rss config files path)
        (for-each (lambda (file) (generate-page config file path)) files))))

  (define make-latex-macros
    (lambda (macros)
      (apply string-append
             (map (lambda (macro)
                    (if (= (cadr macro) 0)
                        (string-append "\\newcommand{"
                                       (car macro)
                                       "}{"
                                       (caddr macro)
                                       "}")
                        (string-append "\\newcommand{"
                                       (car macro)
                                       "}["
                                       (number->string (cadr macro))
                                       "]{"
                                       (caddr macro)
                                       "}")))
                  macros))))

  
  
  (define make-katex-macros
    (lambda (macros)
      (string-append "{"
                     (join ", " (map (lambda (macro)
                                      (string-append "\""
                                                     (string-escape (car macro))
                                                     "\": \""
                                                     (string-escape (caddr macro))
                                                     "\""))
                                    macros))
                     "}"))))
