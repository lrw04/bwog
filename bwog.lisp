;;; XML escape utils
(defun escape (char table)
  (or (cdr (assoc char table)) (make-string 1 :initial-element char)))

(defun xml-escape (str)
  (reduce (lambda (a b) (concatenate 'string a b))
          (map 'list
               (lambda (x) (escape x '((#\< . "&lt;")
                                       (#\> . "&gt;")
                                       (#\& . "&amp;"))))
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

;; TODO
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
                                    (read-inline-iter
                                     (subseq content (1+ pos))
                                     (cons `(math inline ,(subseq content 1 pos)) acc)))))
                             ((char= directive #\`) nil)
                             ((char= directive #\@) nil)
                             ((char= directive #\*) nil)))
                         (read-inline-iter (subseq content sp) (cons (subseq content 0 sp) acc)))))))
    (read-inline-iter content nil)))

(defun titlep (line)
  (let ((hash-count (count-from-beginning line #\#)))
    (and (> hash-count 0) (char= #\Space (elt line hash-count)))))

(defun read-title (line)
  (let* ((hash-count (count-from-beginning line #\#))
         (title (subseq line (1+ hash-count))))
    `(title ,(read-inline title))))

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
                    (sp (member cur '(#\: #\# #\`) :test #'char=)))
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

(defun read-wetd (in)
  (read-div in ":document"))

;;; metadata extension
(defun read-post-meta (path)
  (let ((in (open path)))
    (read-line in)
    (read in)))

(defun first-line (str)
  (read-line (make-string-input-stream str)))

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
