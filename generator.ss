(library (generator)
  (export read-repo
          generate-repo
          use-parent
          use-child
          use-appended
          use-string-append
          default-config
          file-metadata
          file-date
          config-title)
  (import (wetd) (rnrs) (html) (util) (rnrs eval))

  (define use-parent
    (lambda (parent child) parent))

  (define use-child
    (lambda (parent child) child))

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
          (let ((rule (car rules))
                (other (compose-config parent child (cdr rules))))
            (cons (cons (car rule)
                        (apply (cdr rule)
                               (read-config parent (car rule))
                               (read-config child (car rule))
                               '()))
                  other)))))
  
  (define default-propagation-rules
    `((latex-macros . ,use-appended)
      (title . ,use-child)
      (processors . ,use-appended)
      (template . ,use-child)
      (index-template . ,use-child)
      (link . ,use-child)
      (desc . ,use-child)
      (preamble . ,use-parent)))

  (define default-config
    `((title . "default title")
      (processors . ())))
 
  (define read-repo
    (lambda (path)
      (let* ((metadata
              (eval (read-datum-from-file (join-path path
                                                     "metadata.ss"))
                    (environment '(rnrs) '(generator) '(wetd) '(util) '(html))))
             (propagation-rules (append (or (read-config metadata 'propagation-rules) '())
                                        default-propagation-rules))
             (config (cdr (assoc 'config metadata)))
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
                                                                        '(html)))
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

  (define generate-repo
    (lambda (repo path)
      (let ((config (cdr (assoc 'config repo)))
            (files (cdr (assoc 'files repo))))
        (generate-index config files path)
        (generate-rss config files path)))))
