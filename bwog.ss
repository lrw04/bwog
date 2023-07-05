;;;; top level program
(import (wetd) (html) (xml) (filters) (util) (rnrs))

(define propagation-rules '((title . none)
                            (filters . append)
                            (files . none)
                            (subdirs . none)
                            (index . propagate)
                            (template . propagate)
                            (feed . propagate)
                            (files . propagate)))

(define read-datum-from-file
  (lambda (path)
    (let* ((port (open-file-input-port path
                                       (file-options)
                                       'block
                                       (utf-8-codec)))
           (datum (get-datum port)))
      (close-input-port port)
      datum)))

(define write-file
  (lambda (path content)
    (let* ((port (open-file-output-port path
                                        (file-options)
                                        'block
                                        (utf-8-codec))))
      (display content port)
      (close-output-port port))))

(define join-path
  (lambda (a b)
    (string-append a "/" b)))

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
  (lambda (tree filters)
    '()))

(define read-folder
  (lambda (path)
    '()))

(define read-repo
  (lambda (path)
    '()))

(define generate
  (lambda (repo path)
    '()))

(let ((path (if (> (length (command-line)) 1)
                (cadr (command-line))
                ".")))
  (generate (read-repo path) path))
