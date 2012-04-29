;; (C) 2006,2007,2008,2009 Matthew Welland matt@kiatoa.com
;;  
;;   License GPL.

(use test srfi-69 regex-literals)

(include "logprocessor.scm")

(define oup (open-output-file "test-output.log"))
(current-error-port oup)

(define regexs (list (regexp "^hello$")(regexp "\\s+goodbye\\s+")))

(test "misc:line-match-regexs" '("hello") (misc:line-match-regexs "hello" regexs))

(test "expires, expired error"      #f (expect:process-expires "2/2/2002"))
(test "expires, not expired error"  #t (expect:process-expires "2/2/2020"))
(test "expires, not specified"      #f (expect:process-expires #f))

(define *sections* '())

;; Have the builtin trigger for Log start to account for.
(test "add trigger"      2 (begin (trigger "Trigger1" (regexp "Test patt1"))
				  (length *triggers*)))

(test "register a section" 1 (begin (section "Nada" "Start" "End")
				    (length *sections*)))

(test "add an expect" 1 (with-output-to-port oup
			  (lambda ()
			    (expect in "Nada" < 1 "Error 1" (regexp "foo"))
			    (length (hash-table-keys *expects*)))))

(define logpro-version -1) ;; need to fake it out
(test "load a command file" #t (with-output-to-port oup
                                 (lambda ()
				   (load "example.logpro") #t)))

;; (use trace)
;; (trace analyze-logfile)
;; (trace adj-active-sections)
;; (trace filter)
;; (trace sort)

(test "analyze loaded logfile" #t (with-input-from-file "example.log"
				    (lambda ()
				      (analyze-logfile oup) #t)))
				   
(close-output-port oup)
(test-exit)
 
