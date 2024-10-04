;;; a bunch of utilities
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

(define string-escape
  (lambda (s)
    (escape-string s '((#\newline . "\\n")
                       (#\" . "\\\"")
                       (#\\ . "\\\\")))))

(define count-from-beginning
  (lambda (line c)
    (define iter
      (lambda (line c beginning count)
        (if (> (string-length line) beginning)
            (if (char=? (string-ref line beginning) c)
                (iter line c (+ beginning 1) (+ count 1))
                count)
            count)))
    (iter line c 0 0)))

(define cfb-port
  (lambda (port c)
    (define cfb-port-iter
      (lambda (port c count)
        (let ((e (lookahead-char port)))
          (if (char=? c e)
              (begin (get-char port)
                     (cfb-port-iter port c (+ count 1)))
              count))))
    (cfb-port-iter port c 0)))

(define join-lines
  (lambda (lines)
    (apply string-append
           (map (lambda (s)
                  (string-append s (string #\newline)))
                lines))))

(define read-until
  (lambda (port c)
    (define iter
      (lambda (port c acc)
        (let ((e (lookahead-char port)))
          (if (char=? e c)
              (list->string (reverse acc))
              (iter port c (cons (read-char port) acc))))))
    (iter port c '())))

(define assert-with-message
  (lambda (p who message . irritants)
    (if (not p)
        (apply assertion-violation who message irritants))))

(define apply-k
  (lambda (proc k arg)
    (if (= k 0) arg (proc (apply-k proc (- k 1) arg)))))

(define read-until-k
  (lambda (port c k)
    (define iter
      (lambda (port c k acc count)
        (let ((e (lookahead-char port)))
          (cond ((= k count)
                 (list->string (reverse (apply-k cdr k acc))))
                ((eof-object? e)
                 (list->string (reverse acc)))
                ((char=? e c)
                 (iter port c k (cons (get-char port) acc) (+ count 1)))
                (else
                 (iter port c k (cons (get-char port) acc) 0))))))
    (iter port c k '() 0)))

(define open-file-input
  (lambda (path)
    (open-file-input-port path
                          (file-options)
                          'block
                          (make-transcoder (utf-8-codec)))))

(define open-file-output
  (lambda (path)
    (open-file-output-port path
                           (file-options no-fail)
                           'block
                           (make-transcoder (utf-8-codec)))))

(define read-datum-from-file
  (lambda (path)
    (let* ((port (open-file-input path))
           (datum (get-datum port)))
      (close-input-port port)
      datum)))

;; yes it's quadratic time! i got lazy
(define join
  (lambda (sep strings)
    (cond ((< (length strings) 2)
           (apply string-append strings))
          (else
           (string-append (car strings)
                          (apply string-append
                                 (apply append
                                        (map (lambda (s)
                                               (list sep s))
                                             (cdr strings)))))))))

(define path-stem
  (lambda (p)
    (let* ((ext (path-extension p))
           (has-ext (> (string-length ext) 0))
           (stem-length (- (string-length p)
                           (string-length ext)
                           (if has-ext 1 0))))
      (substring p 0 stem-length))))

(define ends-with?
  (lambda (s t)
    (let ((sl (string-length s))
          (tl (string-length t)))
      (if (< sl tl)
          #f
          (string=? t (substring s (- sl tl) sl))))))

;; chez scheme 9.5 doesn't have this
(if (not (top-level-bound? 'path-build))
    (define-top-level-value 'path-build
      (let ((sep (directory-separator)))
        (lambda (a b)
          (cond ((zero? (string-length a)) b)
                ((zero? (string-length b)) a)
                ((or (char=? (string-ref a (- (string-length a) 1)) sep)
                     (char=? (string-ref b 0) sep))
                 (string-append a b))
                (else (string-append a (string sep) b)))))))

;;; dirty lil parser for the markup language
(define read-inline
  (lambda (port end)
    (let ((c (get-char port)))
      (cond ((eof-object? c)
             c)
            ((equal? end c)
             (eof-object))
            ((char=? c #\@)
             (let ((c (lookahead-char port)))
               (case c
                 ((#\@ #\` #\*)
                  (get-char port)
                  c)
                 (else
                  (let* ((datum (get-datum port))
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
    (if (eof-object? s) '()
        (let* ((port (open-string-input-port s))
               (data (read-inline-from-port port #f)))
          (close-input-port port)
          data))))

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
                  (cond ((eof-object? line)
                         (append param (list (join-lines (reverse acc)))))
                        ((string=? line end-line)
                         (append param (list (join-lines (reverse acc)))))
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
    (letrec ((read-par-iter
              (lambda (acc)
                (let ((c (lookahead-char port)))
                  (if (eof-object? c)
                      `(par ,(read-inline-from-string (join-lines (reverse acc))))
                      (case c
                        ((#\: #\` #\! #\newline) ; end of paragraph
                         `(par
                           ,(read-inline-from-string (join-lines (reverse acc)))))
                        (else (read-par-iter (cons (get-line port) acc)))))))))
      (read-par-iter (list line)))))

(define read-container
  (lambda (port end-line param)
    (letrec ((read-container-iter
              (lambda (acc)
                (let ((line (get-line port)))
                  (cond ((eof-object? line)
                         (append param (list (reverse acc))))
                        ((equal? line end-line)
                         (append param (list (reverse acc))))
                        ((container-start? line)
                         (read-container-iter
                          (cons (read-container port
                                                (container-end-line line)
                                                (container-param line))
                                acc)))
                        ((raw-start? line)
                         (read-container-iter
                          (cons (read-raw port
                                          (raw-end-line line)
                                          (raw-param line))
                                acc)))
                        ((leaf? line)
                         (read-container-iter (cons (read-leaf line) acc)))
                        ((string=? line "")
                         (read-container-iter acc))
                        (else
                         (read-container-iter (cons (read-par port line) acc))))))))
      (read-container-iter '()))))

(define read-wetd
  (lambda (port)
    (read-container port #f '(document))))

(define read-wetd-from-file
  (lambda (file)
    (let ((port (open-file-input file)))
      (letrec ((iter (lambda ()
                       (let ((line (get-line port)))
                         (if (equal? line "---") #f (iter))))))
        (iter)
        (let ((tree (read-wetd port)))
          (close-input-port port)
          tree)))))

(define read-header-from-file
  (lambda (file)
    (let ((port (open-file-input file)))
      (letrec ((iter (lambda (acc)
                       (let ((line (get-line port)))
                         (if (equal? line "---")
                             (apply string-append (reverse acc))
                             (iter (cons* "\n" line acc)))))))
        (let ((res (iter '())))
          (close-input-port port)
          res)))))

;;; half-baked html/xml writer
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
    (letrec ((ml-writer
              (lambda (tree port)
                (cond ((char? tree)
                       (display (xml-escape (string tree)) port))
                      ;; "string"
                      ((string? tree)
                       (display (xml-escape tree) port))
                      ;; (raw "string")
                      ((and (pair? tree) (eq? (car tree) 'raw))
                       (display (cadr tree) port))
                      ;; (void-element props)
                      ((and (pair? tree) (memq (car tree) void-elements))
                       (let ((tag (car tree))
                             (props (alist->props (cadr tree))))
                         (display "<" port)
                         (display tag port)
                         (display props port)
                         (display ">" port)))
                      ;; (element props children)
                      ((pair? tree)
                       (let ((tag (car tree))
                             (props (alist->props (cadr tree)))
                             (children (caddr tree)))
                         (display "<" port)
                         (display tag port)
                         (display props port)
                         (display ">" port)
                         (for-each (lambda (t)
                                     (ml-writer t port))
                                   children)
                         (display "</" port)
                         (display tag port)
                         (display ">" port)))
                      (else (error 'html>>
                                   "not a character, string, or tree node"
                                   tree))))))
      ml-writer)))

(define html>> (make-ml-writer html-void-elements))
(define xml>> (make-ml-writer '()))

;;; composing configuration at different levels
(define config-read
  (lambda (cfg k)
    (cdr (or (assq k cfg) (cons #f #f)))))

(define config-compose
  (lambda (parent child rules)
    (if (null? rules)
        '()
        (let* ((rule (car rules))
               (rest (config-compose parent child (cdr rules)))
               (k (car rule))
               (comp (cdr rule)))
          (cons (cons k
                      (comp (config-read parent k)
                            (config-read child k)))
                rest)))))

(define use-parent
  (lambda (parent child) (or parent child)))
(define use-child
  (lambda (parent child) (or child parent)))
(define use-append
  (lambda (parent child) (append (or child '()) (or parent '()))))
(define use-string-append
  (lambda (parent child) (string-append (or child "") (or parent ""))))

(define *composition-rules*
  `((markup-processors . ,use-append)
    ;; index-template: repo * path -> html
    (index-template . ,use-child)
    ;; file-template: repo * path * node -> html
    (file-template . ,use-child)
    ;; after: repo * root * path -> unit
    (after . ,use-child)))

(define-syntax define-config-field
  (syntax-rules ()
    ((_ field comp-rule)
     (set-top-level-value!
      '*composition-rules*
      (cons (cons 'field comp-rule)
            (top-level-value '*composition-rules*))))))

;; NEVER use *config* and *composition-rules* directly! access it
;;   through (interaction-environment).
;; the value of (interaction-environment) will be updated when you
;;   evaluate an expression in another environment, because eval is
;;   redefined to do that further down
;; add-local-config and config are written to do this, and they are
;;   the intended way of using/modifying local config anyways
(define *config* '())

(define add-local-config
  (lambda (c)
    (set-top-level-value! '*config*
                          (config-compose (top-level-value '*config*)
                                          c
                                          (top-level-value '*composition-rules*)))))

(define config
  (lambda (k)
    (config-read (top-level-value '*config*) k)))

(add-local-config `((after . ,(lambda (repo root path) #f))))
(define add-after-hook
  (lambda (f)
    (let ((cur (config 'after)))
      (add-local-config `((after . ,(lambda (repo root path)
                                      (f repo root path)
                                      (cur repo root path))))))))

;;; converter for the markup to html sexp
(define make-tag-predicate
  (lambda (tag)
    (lambda (x)
      (and (pair? x) (eq? (car x) tag)))))
(define process-node
  (lambda (t)
    ((cdr (or (assp (lambda (p) (p t))
                    (config 'markup-processors))
              (error 'process "unknown node type" t)))
     t)))
(define process-subtree
  (lambda (l)
    (if (string? l)
        (list l)
        (map process-node l))))
(define make-direct-processor
  (lambda (html-tag)
    (lambda (t)
      `(,html-tag () ,(process-subtree (cadr t))))))

(define add-markup-processor
  (lambda (pred proc)
    (add-local-config `((markup-processors . ((,pred . ,proc)))))))
(define add-direct-processor
  (lambda (wetd-tag html-tag)
    (add-markup-processor (make-tag-predicate wetd-tag)
                          (make-direct-processor html-tag))))
(define add-direct-processors
  (lambda (tags)
    (for-each (lambda (tag)
                (let ((wetd-tag (car tag))
                      (html-tag (cadr tag)))
                  (add-direct-processor wetd-tag html-tag)))
              tags)))
(define add-trivial-processors
  (lambda (tags)
    (for-each (lambda (tag)
                (add-direct-processor tag tag))
              tags)))
(add-trivial-processors '(ol ul
                          figure
                          sub sup
                          em))
(add-direct-processors '((document article)
                         (par p)
                         (quote blockquote)
                         (item li)
                         (verb code)
                         (st del)
                         (uline u)
                         (s strong)))
(add-markup-processor (make-tag-predicate 'h)
                      (lambda (t)
                        `(,(string->symbol (string-append
                                            "h"
                                            (number->string (cadr t))))
                          ()
                          ,(process-subtree (caddr t)))))
(add-markup-processor char? string)
(add-markup-processor string? (lambda (x) x))
(add-markup-processor (make-tag-predicate 'hr)
                      (lambda (t) `(hr ())))
(add-markup-processor (make-tag-predicate 'img)
                      (lambda (t)
                        `(img ((src . ,(cadr t))
                               (alt . ,(caddr t))))))
(add-markup-processor (make-tag-predicate 'link)
                      (lambda (t)
                        `(a ((href . ,(cadr t)))
                            ,(process-subtree (caddr t)))))

(define read-data
  (lambda (port)
    (letrec ((iter
              (lambda (acc)
                (let ((o (get-datum port)))
                  (if (eof-object? o)
                      (reverse acc)
                      (iter (cons o acc)))))))
      (iter '()))))

(define read-data-from-file
  (lambda (path)
    (let* ((port (open-file-input path))
           (data (read-data port)))
      (close-input-port port)
      data)))

(define read-data-from-string
  (lambda (s)
    (read-data (open-input-string s))))

(define eval
  (lambda (expr env)
    (parameterize ((interaction-environment env))
      ((top-level-value 'eval (environment '(chezscheme))) expr env))))

(define load-in-environment
  (lambda (exprs env)
    (let ((new-env (copy-environment env)))
      (eval (cons* 'begin
                   exprs)
            new-env)
      new-env)))

;; (file env wetd html)
(define gather-file
  (lambda (path parent-env)
    (let* ((header (read-header-from-file path))
           (wetd (read-wetd-from-file path))
           (env (load-in-environment (read-data-from-string header) parent-env)))
      #;(pretty-print wetd)
      `(file ,env
             ,wetd
             ,(eval `(process-node ',wetd) env)))))

;; (dir env files-alist subdir-alist)
(define gather-directory
  (lambda (path parent-env)
    (let* ((listing (map (lambda (file)
                           (path-build path file))
                         (directory-list path)))
           (subdirectories (filter file-directory? listing))
           (files (filter (lambda (file)
                            (and (file-regular? file)
                                 (ends-with? file ".wetd")
                                 (not (char=? #\- (string-ref (path-last file) 0)))))
                          listing))
           (env (load-in-environment (read-data-from-file
                                      (path-build path
                                                  "config.ss"))
                                     parent-env)))
      `(dir ,env
            ,(map (lambda (file)
                    (cons (path-stem (path-last file))
                          (gather-file file env)))
                  (list-sort string<? files))
            ,(map (lambda (subdir)
                    (cons (path-last subdir)
                          (gather-directory subdir env)))
                  (list-sort string<? subdirectories))))))

(define-syntax match-list
  (syntax-rules ()
    ((_ (((var ...) list) ...)
        body ...)
     (let-values (((var ...) (apply values list)) ...)
       body ...))))

(define *root-repo* #f)
(define output-artifacts
  (lambda (repo root path)
    (let ((fs-path (path-build root (join (string (directory-separator)) path))))
      ;; index
      (let ((port (open-file-output (path-build fs-path "index.html")))
            (content ((eval '(config 'index-template) (cadr repo))
                      *root-repo*
                      path)))
        (put-string port "<!doctype html>\n")
        (html>> content port)
        (close-output-port port))
      ;; files
      (for-each (lambda (file)
                  (let* ((filename (car file))
                         (node (cdr file))
                         (file-path (append path (list filename)))
                         (file-fs-path (path-build fs-path
                                                   (string-append filename
                                                                  ".html")))
                         (port (open-file-output file-fs-path))
                         (content ((eval '(config 'file-template) (cadr node))
                                   *root-repo*
                                   file-path
                                   node)))
                    (put-string port "<!doctype html>\n")
                    (html>> content port)
                    (close-output-port port)))
                (caddr repo))
      ;; subdirs
      (for-each (lambda (subdir)
                  (let* ((filename (car subdir))
                         (node (cdr subdir))
                         (subdir-path (append path (list filename)))
                         (subdir-fs-path (path-build fs-path filename)))
                    (output-artifacts node subdir-fs-path subdir-path)))
                (cadddr repo))
      ;; run after
      (let ((after (eval '(config 'after) (cadr repo))))
        (parameterize ((interaction-environment (cadr repo)))
          (after *root-repo* root path))))))

(let* ((root (if (> (length (command-line)) 1)
                 (cadr (command-line))
                 "."))
       (repo (gather-directory root (interaction-environment))))
  (set! *root-repo* repo)
  (output-artifacts repo root '()))
