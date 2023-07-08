;;;; top level program
(import (wetd) (html) (xml) (filters) (generators) (util) (rnrs))

(define propagation-rules '((title . none)
                            (filters . append)
                            (files . none)
                            (template . propagate)
                            (generators . append)
                            (files . propagate)))

(define read-datum-from-file
  (lambda (path)
    (let* ((port (open-file-input-port path
                                       (file-options)
                                       'block
                                       (make-transcoder (utf-8-codec))))
           (datum (get-datum port)))
      (close-input-port port)
      datum)))

(define read-string-from-file
  (lambda (path)
    (let* ((port (open-file-input-port path
                                       (file-options)
                                       'block
                                       (make-transcoder (utf-8-codec))))
           (s (get-string-all port)))
      (close-input-port port)
      s)))

(define read-wetd-from-file
  (lambda (path)
    (let* ((port (open-file-input-port path
                                       (file-options)
                                       'block
                                       (make-transcoder (utf-8-codec))))
           (tree (read-wetd port)))
      (close-input-port port)
      tree)))

(define write-file
  (lambda (path content)
    (let* ((port (open-file-output-port path
                                        (file-options)
                                        'block
                                        (utf-8-codec))))
      (display content port)
      (close-output-port port))))

(define join-path
  (lambda components
    (cond ((null? components) ".")
          ((= (length components) 1) (car components))
          (else (string-append (car components)
                               "/"
                               (apply join-path (cdr components)))))))

(define access-config
  (lambda (key config default)
    (cdr (or (assq key config) (cons key default)))))

(define merge-config-aux
  (lambda (child key)
    (if (assq key child)
        (list (cons key (access-config key child #f)))
        '())))

(define merge-config
  (lambda (parent child)
    (apply append
           (map (lambda (pr)
                  (let ((key (car pr))
                        (rule (cdr pr)))
                    (case rule
                      ((none) (merge-config-aux child key))
                      ((append) (list (cons key
                                            (append (access-config key child '())
                                                    (access-config key parent '())))))
                      ((propagate) (if (assq key child)
                                       (list (assq key child))
                                       (merge-config-aux parent key)))
                      (else (error 'merge-config
                                   "unrecognized propagation type"
                                   (cdr pr))))))
                propagation-rules))))

(define wetd->html
  (lambda (tree)
    (if (string? tree)
        tree
        ;; cons
        (case (car tree)
          ((math) `(span
                    ((class . ,(string-append "math "
                                              (if (eq? (cadr tree) 'display)
                                                  "display"
                                                  "inline"))))
                    (,(caddr tree))))
          ((code) (let ((text (cadr tree)))
                    `(code () (,text))))
          ((link) (let ((href (cadr tree))
                        (text (caddr tree)))
                    `(a ((href . ,href)) ,(map wetd->html text))))
          ((emph) `(em () ,(map wetd->html (cadr tree))))
          ((title) (let ((level (cadr tree))
                         (children (caddr tree)))
                     `(,(string->symbol (string-append "h" (number->string level)))
                       ()
                       ,(map wetd->html children))))
          ((codeblock) (let ((language (cadr tree))
                             (text (caddr tree)))
                         `(pre ,(if (> (string-length language) 0)
                                    `((class . ,(string-append "language-" language)))
                                    '())
                               ((code () (,text))))))
          ((par) (let ((children (cadr tree)))
                   `(p () ,(map wetd->html children))))
          ((div) (let ((type (cadr tree))
                       (children (caddr tree)))
                   `(div ((class . ,type)) ,(map wetd->html children))))
          ((html) (let ((children (cadr tree)))
                    children))
          (else (error 'wetd->html "unrecognized wetd tag"))))))

(define read-folder
  (lambda (path)
    '()))

(define read-repo
  (lambda (path)
    '()))

(define generate
  (lambda (repo path)
    '()))

(html>> (wetd->html (read-wetd-from-file "test.wetd")) (current-output-port))
(newline)

'(let ((path (if (> (length (command-line)) 1)
                 (cadr (command-line))
                 ".")))
   (generate (read-repo path) path))
