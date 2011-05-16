(define (parser fname)
  (let ((inp        (open-input-file fname))
	(thestack   (make-stack)))
    (stack-push! thestack 'start)
    (lexer-init 'port inp)
    (let loop ((token   (lexer)))
      (let ((token-type (car token))
            (token-val  (cadr token))
            (state      (stack-peek thestack)))
        (case token-type
          ('end-of-input       (print "Done")(close-input-port inp))
          ('whitespace         (loop (lexer)))  ;; skip whitespace
          ('comment-begin
           (stack-push! thestack 'comment)
           (loop (lexer)))
	  ('newline