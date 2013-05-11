#!/usr/bin/env sbcl --script

(with-open-file (file "symbols.lst" :direction :output :if-exists :supersede)
  (let ((symbols nil))
    (do-symbols (x "COMMON-LISP")
      (push x symbols))
    (mapc (lambda (x) (format file "~s~%" x)) (butlast symbols))))
    
