(import (util) (wetd) (xml) (rnrs))

(define-syntax test
  (syntax-rules ()
    ((_ pred)
     (begin
       (display "testing ")
       (write 'pred)
       (assert pred)
       (display " PASS\n")))))

(test (equal? (xml-escape "<not-a-&-tag>") "&lt;not-a-&amp;-tag&gt;"))
(test (equal? (tex-escape "some $$'s do not denote mathematics.") "some \\$\\$'s do not denote mathematics."))
(test (equal? (count-from-beginning "## title" #\#) 2))
(test (equal? (count-from-beginning "nothing to see here" #\$) 0))
(test (equal? (join-lines (list "a b" "c d")) "a b\nc d\n"))
(test (equal? (cfb-port (open-string-input-port "$$ display math $$") #\$) 2))
(test (equal? (read-until-k (open-string-input-port "some content $$$$") #\$ 2) "some content "))
(test (equal? (read-until-k (open-string-input-port "some content $$$$") #\$ 5) "some content $$$$"))

(display "all tests passed.\n")
