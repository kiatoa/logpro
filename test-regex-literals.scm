(require-library chicken-syntax)
(use regex regex-literals)

(define x "abcde")

(eval '(require-extension regex))
(eval '(require-extension regex-literals))
(load "file-with-regex-literals.scm")


