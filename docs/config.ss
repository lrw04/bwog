(define-config-field title use-child)
(add-local-config '((title . "bwog docs")))

;;; html templates
(define html-defaults
  (lambda (head body)
    `(html ((lang . "en"))
           ((head ()
                  ((meta ((charset . "utf-8")))
                   (meta ((name . "viewport")
                          (content . "width=device-width, initial-scale=1")))
                   ,@head))
            (body ()
                  ,body)))))

;; fixme: add semantic tags
(define index-template
  (lambda (repo path)
    (let ((files (caddr repo))
          (title (eval '(config 'title) (cadr repo))))
      (html-defaults `((link ((rel . "stylesheet")
                              (href . "style.css")))
                       (title () (,title)))
                     `((p () ("bwog is a static blog generator written in Chez Scheme. This is its documentation website."))
                       (p () ("Document listing:"))
                       (ul ()
                           ,(map (lambda (file)
                                   (let* ((filename (car file))
                                          (node (cdr file))
                                          (title (eval '(config 'title)
                                                       (cadr node))))
                                     `(li ()
                                          ((a ((href . ,(string-append filename
                                                                       ".html")))
                                              (,title))))))
                                 files)))))))

(define file-template
  (lambda (repo path node)
    (let ((title (eval '(config 'title) (cadr node)))
          (html (cadddr node)))
      (html-defaults
       `((link ((rel . "stylesheet")
                (href . "style.css")))
         (link ((rel . "stylesheet")
                (href . "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.10.0/build/styles/default.min.css")))
         (link ((rel . "stylesheet")
                (href . "https://cdn.jsdelivr.net/npm/katex@0/dist/katex.min.css")))
         (title () (,title)))
       `((p () ((a ((href . "index.html")) ("Home"))))
         ,html)))))

(add-local-config `((index-template . ,index-template)
                    (file-template . ,file-template)))

;;; math/code/latex markup processors
(define kill-process
  (case (machine-type)
    ((i3nt ti3nt a6nt ta6nt arm64nt tarm64nt)
     (lambda (pid)
       (system (string-append "taskkill /f /pid " (number->string pid)))))
    (else
     (lambda (pid)
       (system (string-append "kill -s KILL " (number->string pid)))))))

(define run-process
  (lambda (cmdline input)
    (let-values (((stdin stdout stderr pid)
                  (open-process-ports cmdline
                                      'block
                                      (make-transcoder (utf-8-codec)))))
      (put-string stdin input)
      (close-output-port stdin)
      (let ((out (get-string-all stdout)))
        (kill-process pid)
        out))))

(add-markup-processor
 (make-tag-predicate 'code)
 (lambda (t)
   (let* ((lang-absent (null? (cddr t)))
          (lang (if lang-absent "txt" (symbol->string (cadr t))))
          (code ((if lang-absent cadr caddr) t))
          (js (string-append "import hljs from 'npm:highlight.js@11';
console.log(hljs.highlight(\""
                             (string-escape code)
                             "\", {language: \""
                             (string-escape lang)
                             "\"}).value);")))
     `(pre ()
           ((code ((class . ,(string-append "language-" lang)))
                  ((raw ,(run-process "deno run -" js)))))))))

(define latex-macros
  '(("\\Gal" 0 "\\operatorname{Gal}")
    ("\\tr" 0 "\\operatorname{tr}")
    ("\\GL" 0 "\\operatorname{GL}")
    ("\\SL" 0 "\\operatorname{SL}")
    ("\\PSL" 0 "\\operatorname{PSL}")
    ("\\SO" 0 "\\operatorname{SO}")
    ("\\SU" 0 "\\operatorname{SU}")
    ("\\im" 0 "\\operatorname{im}")
    ("\\cof" 0 "\\operatorname{cof}")
    ("\\End" 0 "\\operatorname{End}")
    ("\\Tor" 0 "\\operatorname{Tor}")
    ("\\rk" 0 "\\operatorname{rk}")
    ("\\Hom" 0 "\\operatorname{Hom}")
    ("\\diag" 0 "\\operatorname{diag}")
    ("\\vspan" 0 "\\operatorname{span}")
    ("\\lcm" 0 "\\operatorname{lcm}")
    ("\\id" 0 "\\operatorname{id}")
    ("\\Ab" 0 "\\textsf{Ab}")
    ("\\Fld" 0 "\\textsf{Fld}")
    ("\\Mod" 1 "#1\\textsf{-Mod}")
    ("\\Grp" 0 "\\textsf{Grp}")
    ("\\dSet" 1 "#1\\textsf{-Set}")
    ("\\Set" 0 "\\textsf{Set}")
    ("\\SetStar" 0 "\\textsf{Set*}")
    ("\\Vect" 1 "#1\\textsf{-Vect}")
    ("\\Alg" 1 "#1\\textsf{-Alg}")
    ("\\Ring" 0 "\\textsf{Ring}")
    ("\\R" 0 "\\mathbb{R}")
    ("\\C" 0 "\\mathbb{C}")
    ("\\N" 0 "\\mathbb{N}")
    ("\\Z" 0 "\\mathbb{Z}")
    ("\\Q" 0 "\\mathbb{Q}")
    ("\\F" 0 "\\mathbb{F}")
    ("\\sfC" 0 "\\mathsf{C}")
    ("\\vphi" 0 "\\varphi")))
(define latex-macro-decls
  (apply string-append
         (map (lambda (macro)
                (string-append "\\providecommand"
                               (car macro)
                               "{}\\renewcommand"
                               (car macro)
                               "["
                               (number->string (cadr macro))
                               "]{"
                               (caddr macro)
                               "}"))
              latex-macros)))

(add-markup-processor
 (make-tag-predicate 'math)
 (lambda (t)
   (let* ((latex (cadr t))
          (str (string-append latex-macro-decls latex))
          (js (string-append "import katex from 'npm:katex@0.16';
console.log(katex.renderToString(\""
                             (string-escape str)
                             "\"));")))
     `(raw ,(run-process "deno run -" js)))))
(add-markup-processor
 (make-tag-predicate 'dmath)
 (lambda (t)
   (let* ((latex (cadr t))
          (str (string-append latex-macro-decls latex))
          (js (string-append "import katex from 'npm:katex@0.16';
console.log(katex.renderToString(\""
                             (string-escape str)
                             "\", {displayMode: true}));")))
     `(raw ,(run-process "deno run -" js)))))

(define latex-preamble
  "\\usepackage{amsmath, amssymb, amsthm, latexsym, mathrsfs, eucal, ctex}
\\usepackage[dvipsnames]{xcolor}
\\usepackage{tabularx, tikz-cd, tikz, bm}")

(define latex->svg
  (lambda (latex)
    (if (file-exists? "tmp.tex") (error 'latex->svg "tmp.tex exists"))
    (let ((port (open-file-output "tmp.tex")))
      (put-string port latex)
      (close-output-port port)
      (system "tectonic tmp.tex")
      (system "dvisvgm --pdf -f woff2 tmp.pdf")
      (let* ((port (open-file-input "tmp.svg"))
             (svg (get-string-all port)))
        (close-input-port port)
        (delete-file "tmp.tex")
        (delete-file "tmp.pdf")
        (delete-file "tmp.svg")
        svg))))

(add-markup-processor
 (make-tag-predicate 'latex)
 (lambda (t)
   (let* ((text (cadr t))
          (latex (string-append "\\documentclass{standalone}"
                                latex-preamble
                                latex-macro-decls
                                "\\begin{document}"
                                text
                                "\\end{document}"))
          (svg (latex->svg latex)))
     `(raw ,svg))))

;;; rss
(define link "https://lrw04.github.io/bwog/")
(define files->rss
  (lambda (files)
    `(rss ((version . "2.0")
           (xmlns:atom . "http://www.w3.org/2005/Atom"))
          ((channel ()
                    ((title ()
                            (,(config 'title)))
                     (link ()
                           (,link))
                     (description ()
                                  ("Documentation for bwog"))
                     (atom:link ((href . ,(string-append link "feed.xml"))
                                 (rel . "self")
                                 (type . "application/rss+xml"))
                                ())
                     ,@(map (lambda (file)
                              (let* ((path (car file))
                                     (file (cdr file))
                                     (env (cadr file))
                                     (url (string-append link
                                                         (join "/" path)
                                                         ".html")))
                                `(item ()
                                       ((title ()
                                               (,(eval '(config 'title) env)))
                                        (link ()
                                              (,url))
                                        (description ()
                                                     ;; use the title again
                                                     (,(eval '(config 'title) env)))
                                        (guid ()
                                              (,url))))))
                            files)))))))

(define-config-field rss-enable use-child)
(add-local-config '((rss-enable . #t)))
(add-after-hook (lambda (repo root path)
                  (if (null? path)
                      (letrec ((all-files
                                (lambda (repo path)
                                  (let ((files (caddr repo))
                                        (subdirs (cadddr repo)))
                                    (apply append
                                           (map (lambda (file)
                                                  (let ((name (car file))
                                                        (node (cdr file)))
                                                    (cons (append path (list name))
                                                          node)))
                                                files)
                                           (map (lambda (subdir)
                                                  (all-files (cdr subdir)
                                                             (append path
                                                                     (list (car subdir)))))
                                                subdirs))))))
                        (let* ((files (all-files repo '()))
                               (files (filter (lambda (file)
                                                (let ((env (caddr file)))
                                                  (eval '(config 'rss-enable) env)))
                                              files))
                               (xml (files->rss files))
                               (port (open-file-output (path-build root "feed.xml"))))
                          (put-string port "<?xml version=\"1.0\"?>")
                          (xml>> xml port)
                          (close-output-port port))))))
