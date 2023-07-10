(import (rnrs) (generator) (html) (util) (wetd))

(let ((path (if (> (length (command-line)) 1)
                 (cadr (command-line))
                 ".")))
   (generate-repo (read-repo path) path))
