(defun count-from-beginning (line ch)
  (labels ((count-from-beginning-iter (line ch cnt)
             (if (> (length line) 0)
                 (if (char= (elt line 0) ch) (count-from-beginning-iter (subseq line 1) ch (1+ cnt))
                     cnt)
                 cnt)))
    (count-from-beginning-iter line ch 0)))

(defun read-inline (content)
  content)

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
                         (read-codeblock-iter in line (cons cur acc)))))))
      (read-codeblock-iter in line nil))))

(defun divp (cur)
  (and (> (length cur) 0) (char= (elt cur 0) #\:)))

(defun div-endp (cur)
  (string= cur ":end"))

(defun read-paragraph (in line)
  (labels ((read-paragraph-iter (in acc)
             (let ((cur (read-char in nil)))
               (if (or (not cur) (member cur '(#\: #\# #\`) :test #'char=))
                   (progn
                     (unread-char cur in)
                     `(paragraph ,(format nil "~{~a~%~}" (reverse acc))))
                   (read-paragraph-iter in (cons cur acc))))))
    (read-paragraph-iter in (list line))))

(defun read-div (in line)
  (labels ((read-div-iter (in type acc)
             (let ((cur (read-line in nil)))
               (if cur
                   (cond
                     ((titlep cur) (read-div-iter in type (cons (read-title cur) acc)))
                     ((codeblockp cur) (read-div-iter in type (cons (read-codeblock in cur) acc)))
                     ((div-endp cur) `(div ,type ,(reverse acc)))
                     ((divp cur) (read-div-iter in type (cons (read-div in cur) acc)))
                     ((string= cur "") (read-div-iter in type acc))
                     (t (read-div-iter in type (cons (read-paragraph in cur) acc))))
                   `(div ,type ,(reverse acc))))))
    (read-div-iter in line nil)))

(defun read-wetd (in)
  (read-div in "document"))
