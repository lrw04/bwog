(import (util) (wetd) (html) (rnrs))

(define-syntax test
  (syntax-rules ()
    ((_ pred)
     (begin
       (display "testing ")
       (write 'pred)
       (assert pred)
       (display " PASS\n\n")))))

(test (equal? (xml-escape "<not-a-&-tag>") "&lt;not-a-&amp;-tag&gt;"))
(test (equal? (tex-escape "some $$'s do not denote mathematics.") "some \\$\\$'s do not denote mathematics."))
(test (equal? (string-escape "text \"text enclosed in quotes\" with
endlines") "text \\\"text enclosed in quotes\\\" with\\nendlines"))
(test (equal? (count-from-beginning "## title" #\#) 2))
(test (equal? (count-from-beginning "nothing to see here" #\$) 0))
(test (equal? (join-lines (list "a b" "c d")) "a b\nc d\n"))
(test (equal? (cfb-port (open-string-input-port "$$ display math $$") #\$) 2))
(test (equal? (read-until-k (open-string-input-port "some content $$$$") #\$ 2) "some content "))
(test (equal? (read-until-k (open-string-input-port "some content $$$$") #\$ 5) "some content $$$$"))
(test (equal? (read-until-k (open-string-input-port "math$") #\$ 1) "math"))

(test (equal? '(document
                ((title (#\T #\i #\t #\l #\e #\space #\1))
                 (container
                  ((par (#\t #\e #\x #\t #\space (math "math") #\space
                         (emph
                          (#\e #\m #\p #\h #\space #\w #\i #\t #\h #\space
                           (math "math")))
                         #\space (ul (#\u #\l)) #\newline))
                   (nested ((par (#\n #\e #\s #\t #\e #\d #\newline))))
                   (code "code\n")))
                 (par (#\p #\a #\r #\a #\g #\r #\a #\p #\h #\space #\f #\o #\l #\l
                       #\o #\w #\space #\@ #\l #\r #\w #\0 #\4 #\_ #\space #\o #\n
                       #\space #\t #\w #\i #\t #\t #\e #\r #\, #\space #\@ #\l #\r
                       #\w #\0 #\4 #\space #\o #\n #\space #\g #\i #\t #\h #\u #\b
                       #\, #\space #\a #\n #\d #\space #\l #\r #\w #\0 #\4 #\space
                       #\o #\n #\space #\b #\i #\l #\i #\b #\i #\l #\i #\, #\space
                       #\t #\h #\a #\n #\k #\s #\space #\m #\e #\o #\w #\!
                       #\newline))
                 (code "code text\n")))
              (read-wetd (open-string-input-port
                          "!(title)Title 1
::(container)
text @(math)``math`` @(emph)*emph with @(math)`math`* @(ul)*ul*
::::(nested)
nested
::::
``(code)
code
``
::
paragraph follow @@lrw04_ on twitter, @@lrw04 on github, and lrw04 on bilibili, thanks meow!

``(code)
code text
``"))))

(define html->string
  (lambda (tree)
    (let-values (((port extractor) (open-string-output-port)))
      (html>> tree port)
      (close-output-port port)
      (extractor))))

(test (equal? (html->string '(img ((src . "a.png")))) "<img src=\"a.png\">"))
(test (equal? (html->string '(p () ("paragraph content" (em () ("emphasized"))))) "<p>paragraph content<em>emphasized</em></p>"))
(test (equal? (html->string '(script () ((raw "hljs.highlightAll()&&1;")))) "<script>hljs.highlightAll()&&1;</script>"))

(display "all tests passed.\n")
