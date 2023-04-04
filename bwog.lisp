(ql:quickload :alexandria)
(ql:quickload :uiop)

;;; escaping utils
(defun escape (char table)
  (or (cdr (assoc char table)) (make-string 1 :initial-element char)))

(defun xml-escape (str)
  (reduce (lambda (a b) (concatenate 'string a b))
          (map 'list
               (lambda (x) (escape x '((#\< . "&lt;")
                                       (#\> . "&gt;")
                                       (#\& . "&amp;"))))
               str)))

(defun tex-escape (str)
  (reduce (lambda (a b) (concatenate 'string a b))
          (map 'list
               (lambda (x) (escape x '((#\% . "\\%")
                                       (#\\ . "\\textbackslash")
                                       (#\~ . "\\~{}"))))
               str)))

;;; wetd language parser
(defun count-from-beginning (line ch)
  (labels ((count-from-beginning-iter (line ch cnt)
             (if (> (length line) 0)
                 (if (char= (elt line 0) ch)
                     (count-from-beginning-iter (subseq line 1) ch (1+ cnt))
                     cnt)
                 cnt)))
    (count-from-beginning-iter line ch 0)))

(defun read-inline (content)
  (labels ((read-inline-iter (content acc)
             (if (= (length content) 0)
                 (reverse acc)
                 (let* ((sp (or (position-if (lambda (c) (member c '(#\\ #\$ #\` #\@ #\*) :test #'char=)) content)
                                (length content))))
                     (if (= sp 0)
                         (let ((directive (elt content 0)))
                           (cond
                             ((char= directive #\\) (read-inline-iter (subseq content 2) (cons (subseq content 1 2) acc)))
                             ((char= directive #\$)
                              (if (char= (elt content 1) #\$)
                                  ;; display math
                                  (let ((pos (search "$$" content :start2 2)))
                                    (read-inline-iter
                                     (subseq content (+ pos 2))
                                     (cons `(math display ,(subseq content 2 pos)) acc)))
                                  ;; inline math
                                  (let ((pos (search "$" content :start2 1)))
                                    (read-inline-iter (subseq content (1+ pos))
                                                      (cons `(math inline ,(subseq content 1 pos)) acc)))))
                             ((char= directive #\`)
                              (let* ((backticks (count-from-beginning content #\`))
                                     (end (search (make-string backticks :initial-element #\`) content :start2 backticks)))
                                (read-inline-iter (subseq content (+ end backticks))
                                                  (cons `(code ,(subseq content backticks end)) acc))))
                             ((char= directive #\@)
                              (if (char= (elt content 1) #\()
                                  (let ((href-end (position #\) content)))
                                    (if (char= (elt content (1+ href-end)) #\()
                                        (let ((text-end (position #\) content :start (1+ href-end))))
                                          (read-inline-iter (subseq content (1+ text-end))
                                                            (cons `(link
                                                                    ,(subseq content 2 href-end)
                                                                    ,(read-inline (subseq content (+ href-end 2) text-end)))
                                                                  acc)))
                                        (read-inline-iter (subseq content 1)
                                                          (cons "@" acc))))
                                  (read-inline-iter (subseq content 1)
                                                    (cons "@" acc))))
                             ((char= directive #\*)
                              (let* ((end (position #\* content :start 1))
                                     (emphasized (subseq content 1 end))
                                     (rest (subseq content (1+ end))))
                                (read-inline-iter rest (cons `(emph ,emphasized) acc))))))
                         (read-inline-iter (subseq content sp) (cons (subseq content 0 sp) acc)))))))
    (read-inline-iter content nil)))

(defun titlep (line)
  (let ((hash-count (count-from-beginning line #\#)))
    (and (> hash-count 0) (char= #\Space (elt line hash-count)))))

(defun read-title (line)
  (let* ((hash-count (count-from-beginning line #\#))
         (title (subseq line (1+ hash-count))))
    `(title ,hash-count ,(read-inline title))))

(defun codeblockp (line)
  (> (count-from-beginning line #\`) 2))

(defun codeblock-endp (cur backticks)
  (string= cur (make-string backticks :initial-element #\`)))

(defun codeblock-language (line)
  (subseq line (count-from-beginning line #\`)))

(defun read-codeblock (in line)
  (let ((backticks (count-from-beginning line #\`)))
    (labels ((read-codeblock-iter (in line acc)
               (let ((cur (read-line in nil)))
                 (if cur
                     (if (codeblock-endp cur backticks)
                         `(codeblock ,(codeblock-language line) ,(format nil "~{~a~%~}" (reverse acc)))
                         (read-codeblock-iter in line (cons cur acc)))
                     `(codeblock ,(codeblock-language line) ,(format nil "~{~a~%~}" (reverse acc)))))))
      (read-codeblock-iter in line nil))))

(defun divp (cur)
  (and (> (length cur) 0) (char= (elt cur 0) #\:)))

(defun div-endp (cur)
  (string= cur ":end"))

(defun read-paragraph (in line)
  (labels ((read-paragraph-iter (in acc)
             (let* ((cur (read-char in nil))
                    (sp (member cur '(#\: #\# #\` #\Newline) :test #'char=)))
               (unread-char cur in)
               (if (or (not cur) sp)
                   `(paragraph ,(read-inline (format nil "~{~a~%~}" (reverse acc))))
                   (read-paragraph-iter in (cons (read-line in) acc))))))
    (read-paragraph-iter in (list line))))

(defun read-div (in line)
  (labels ((read-div-iter (in type acc)
             (let ((cur (read-line in nil)))
               (if cur
                   (cond
                     ((titlep cur) (read-div-iter in type (cons (read-title cur) acc)))
                     ((codeblockp cur) (read-div-iter in type (cons (read-codeblock in cur) acc)))
                     ((div-endp cur) `(div ,(subseq type 1) ,(reverse acc)))
                     ((divp cur) (read-div-iter in type (cons (read-div in cur) acc)))
                     ((string= cur "") (read-div-iter in type acc))
                     (t (read-div-iter in type (cons (read-paragraph in cur) acc))))
                   `(div ,(subseq type 1) ,(reverse acc))))))
    (read-div-iter in line nil)))

(defun to-latex-subtree (lst)
  (format nil "~{~a~}" (map 'list #'to-latex lst)))

(defun to-latex (tree)
  (if (stringp tree)
      (tex-escape tree)
      (let ((type (car tree)))
        (cond
          ((eql type 'div)
           (cond
             ;; ul and ol
             ((string= (cadr tree) "ul")
              (format nil "\\begin{itemize}~%~{\\item ~a~}\\end{itemize}" (map 'list #'to-latex (caddr tree))))
             ((string= (cadr tree) "ol")
              (format nil "\\begin{enumerate}~%~{\\item ~a~}\\end{enumerate}" (map 'list #'to-latex (caddr tree))))
             (t (format nil "\\begin{~a}~%~a\\end{~a}" (cadr tree) (to-latex-subtree (caddr tree)) (cadr tree)))))
          ((eql type 'codeblock)
           (let ((language (cadr tree))
                 (code (caddr tree)))
             (if (string= language "!latex")
                 code
                 (format nil "\\begin{verbatim}~%~a\\end{verbatim}" code))))
          ((eql type 'paragraph)
           (format nil "~%~a~%" (to-latex-subtree (cadr tree))))
          ((eql type 'code)
           (format nil "\\verb`~a`" (cadr tree)))
          ((eql type 'math)
           (if (eql (cadr tree) 'display)
               (format nil "\\[~a\\]" (caddr tree))
               (format nil "$~a$" (caddr tree))))
          ((eql type 'link)
           (let ((url (cadr tree))
                 (text (caddr tree)))
             (if (string= "img:" (subseq url 0 4))
                 (format nil "\\begin{center}~%\\includegraphics{~a}\\\\ ~a~%\\end{center}"
                         (subseq url 4)
                         (to-latex-subtree text))
                 (format nil "\\href{~a}{~a}" url (to-latex-subtree text)))))
          ((eql type 'emph)
           (format nil "\\emph{~a}" (to-latex-subtree (list (cadr tree)))))
          ((eql type 'title)
           (cond
             ((= (cadr tree) 1) "")
             ((= (cadr tree) 2) (format nil "\\section{~a}" (to-latex-subtree (caddr tree))))
             ((= (cadr tree) 3) (format nil "\\subsection{~a}" (to-latex-subtree (caddr tree))))
             ((= (cadr tree) 4) (format nil "\\subsubsection{~a}" (to-latex-subtree (caddr tree))))
             (t (error "Title is too deep for LaTeX format"))))
          (t (error "Unknown node type: ~a" type))))))

(defun to-html-subtree (lst preamble macros)
  (format nil "~{~a~}" (map 'list (lambda (x) (to-html x preamble macros)) lst)))

(defun image-html (url text)
  (format nil "<figure>~%<img src=\"~a\"><figcaption>~a</figcaption></figure>" url text))

(defun build-latex-macro (macro)
  (if (> (cadr macro) 0)
      ;; with arguments
      (format nil "\\newcommand{~a}[~a]{~a}" (car macro) (cadr macro) (caddr macro))
      ;; without arguments
      (format nil "\\newcommand{~a}{~a}" (car macro) (caddr macro))))

(defun assemble-latex-source (src preamble macros)
  (format nil
          "\\documentclass{standalone}~%~a~{~a~}~%\\begin{document}~a\\end{document}"
          preamble
          (map 'list #'build-latex-macro macros)
          src))

(defun latex-in-html (src preamble macros)
  (if (or (probe-file "texput.pdf") (probe-file "texput.svg"))
      (error "Found texput files in current directory"))
  (let ((src (assemble-latex-source src preamble macros)))
    (with-input-from-string (in src)
      (uiop:run-program '("tectonic" "-")
                        :input in))
    (uiop:run-program '("pdftocairo" "-svg" "texput.pdf" "texput.svg"))
    (let ((result (rest-lines (alexandria:read-file-into-string "texput.svg"))))
      (delete-file "texput.pdf")
      (delete-file "texput.svg")
      (format nil "<figure>~a</figure>" result))))

(defun to-html (tree preamble macros)
  (if (stringp tree)
      (xml-escape tree)
      (let ((type (car tree)))
        (cond
          ((eql type 'div)
           (cond
             ;; ul and ol
             ((string= (cadr tree) "ul")
              (format nil "<ul>~{<li>~a</li>~}</ul>" (map 'list (lambda (x) (to-html x preamble macros)) (caddr tree))))
             ((string= (cadr tree) "ol")
              (format nil "<ol>~{<li>~a</li>~}</ul>" (map 'list (lambda (x) (to-html x preamble macros)) (caddr tree))))
             (t (format nil "<div class=\"~a\">~a</div>" (cadr tree) (to-html-subtree (caddr tree) preamble macros)))))
          ((eql type 'codeblock)
           (cond
             ((string= (cadr tree) "!latex")
              (latex-in-html (caddr tree) preamble macros))
             (t (format nil "<pre class=\"~a\"><code>~a</code></pre>" (cadr tree) (caddr tree)))))
          ((eql type 'title)
           (format nil "<h~a>~a</h~a>" (cadr tree) (to-html-subtree (caddr tree) preamble macros) (cadr tree)))
          ((eql type 'emph)
           (format nil "<em>~a</em>" (to-html-subtree (list (cadr tree)) preamble macros)))
          ((eql type 'math)
           (if (eql (cadr tree) 'display)
               (format nil "<span class=\"math display\">~a</span>" (xml-escape (caddr tree)))
               (format nil "<span class=\"math inline\">~a</span>" (xml-escape (caddr tree)))))
          ((eql type 'paragraph)
           (format nil "<p>~a</p>" (to-html-subtree (cadr tree) preamble macros)))
          ((eql type 'code) (format nil "<code>~a</code>" (xml-escape (cadr tree))))
          ((eql type 'link)
           (let ((url (cadr tree))
                 (text (caddr tree)))
             (if (string= (subseq url 0 4) "img:")
                 (image-html url (to-html-subtree text preamble macros))
                 (format nil "<a href=\"~a\">~a</a>" url (to-html-subtree text preamble macros)))))))))

(defun or-func (a b)
  (or a b))

(defun title (tree)
  (if (stringp tree)
      nil
      (let ((type (car tree)))
        (cond
          ((eql type 'div) (reduce #'or-func (map 'list #'title (caddr tree))))
          ((eql type 'title)
           (if (= (cadr tree) 1)
               (caddr tree)
               nil))
          (t nil)))))

(defun read-wetd (in)
  (read-div in ":document"))

(defun parse (s)
  (with-input-from-string (in s)
    (read-wetd in)))

;;; metadata extension
(defun read-post-meta (path)
  (let ((in (open path)))
    (read-line in)
    (read in)))

(defun first-line (str)
  (read-line (make-string-input-stream str)))

(defun rest-lines (str)
  (let ((fl (first-line str)))
    (subseq str (1+ (length fl)))))

(defun extract-post-content (file)
  (let* ((fl (first-line file))
         (file-without-first-line (subseq file (1+ (length fl)))))
    (labels ((extract-iter (str)
               (let ((fl (first-line str))
                     (str-without-fl (subseq str (1+ (length fl)))))
                 (if (string= fl "---")
                     str-without-fl
                     (extract-iter str-without-fl)))))
      (extract-iter file-without-first-line))))

(defvar *latex-macros* nil)
(defvar *preamble* "\\usepackage{tikz}")
(defvar *tag-titles* nil)
(defvar *title* "default title")

(defvar *tags* (make-hash-table))
(defvar *posts* nil)

(defun without-wetd-extension (str)
  (subseq str 0 (- (length str) 5)))

(defun read-post (path)
  (let* ((meta (read-post-meta path))
         (content (extract-post-content (alexandria:read-file-into-string path)))
         (tree (parse content))
         (pagep (getf meta :page))
         (tags (getf meta :tags))
         (date (getf meta :date))
         (name (without-wetd-extension (file-namestring path))))
    (push (list name pagep tags date (title tree) tree) *posts*)
    (map nil (lambda (x) (push name (gethash x *tags*))) tags)))

(defun index-page ()
  (let ((sorted (sort *posts* #'string< :key #'cadddr)))
    ()))
(defun post-page ())
(defun tag-page ())

(defun generate (repo)
  (load (format nil "~a/blog.lisp" repo))  ; redefine index-page, post-page, tag-page, and global variables
  (map nil #'read-post (directory (format nil "~a/*.wetd" repo))))
