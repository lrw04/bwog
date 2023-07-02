(import (util) (wetd) (xml) (rnrs))

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
(test (equal? (count-from-beginning "## title" #\#) 2))
(test (equal? (count-from-beginning "nothing to see here" #\$) 0))
(test (equal? (join-lines (list "a b" "c d")) "a b\nc d\n"))
(test (equal? (cfb-port (open-string-input-port "$$ display math $$") #\$) 2))
(test (equal? (read-until-k (open-string-input-port "some content $$$$") #\$ 2) "some content "))
(test (equal? (read-until-k (open-string-input-port "some content $$$$") #\$ 5) "some content $$$$"))
(test (equal? (read-until-k (open-string-input-port "math$") #\$ 1) "math"))

(test (equal? '(div "document"
                    ((title 1 ("title 1"))
                     (title 2 ("title 2"))
                     (codeblock "code" "some code\n")
                     (par ((code "inline code") " " (math inline "math") " " (math inline "display math")
                           " " (emph ("emph " (math inline "math"))) " "
                           (link "url" ("text " (math inline "math"))) " follow " "@"
                           "lrw04_ on twitter meow\n"))
                     (div "div" ((par ("content\n")))))) (read-wetd (open-string-input-port
                     "# title 1

## title 2

```code
some code
```

``inline code`` $math$ $display math$ *emph $math$* @(url)(text $math$) follow \\@lrw04_ on twitter meow

:div
content
:end"))))

(display "all tests passed.\n")
