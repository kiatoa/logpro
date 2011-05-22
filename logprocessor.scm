;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use format srfi-69 srfi-1 posix) ;; sqlite3)
(use regex regex-literals)
(define getenv get-environment-variable)

(let ((args (argv)))
  (if (< (length args) 2)
      (begin
	(print "Usage: logpro cmdfile [htmlfile] > annotated.log < inputfile.log")
	(print "  Version " logpro-version)
	(print "  License GPL, more info about logpro at http://www.kiatoa.com/fossils/logpro")
	(exit 1))))

;; NOTES: 
;;   - need ability to apply a regex and pull out a value
;;     e.g. Number of matching nets found == 120

;;======================================================================
;; Misc
;;======================================================================
(define (misc:line-match-regexs line regexs)
  (if (null? regexs)
      #f
      (let loop ((hed (car regexs))
		 (tal (cdr regexs)))
	(let ((match (string-search hed line)))
	  (if match
	      (car match)
	      (if (null? tal)
		  #f
		  (loop (car tal)(cdr tal))))))))

;;======================================================================
;; Triggers
;;======================================================================

(define *triggers* '())

(define-inline (trigger:get-name vec)(vector-ref vec 0))
(define-inline (trigger:get-patts vec)(vector-ref vec 1))
(define-inline (trigger:get-remaining-hits vec)(vector-ref vec 2))
(define-inline (trigger:set-remaining-hits! vec val)(vector-set! vec 2 val))
(define-inline (trigger:inc-total-hits vec)(vector-set! vec 3 (+ (vector-ref vec 3) 1)))
(define-inline (trigger:get-total-hits vec)(vector-ref vec 3))

;; Triggers default to one hit 
(define (trigger name . patts)
  (set! *triggers* (cons (vector name patts 1 0) *triggers*)))

;; Do we want lifetime control? 0 forever, or N for number of times it may be invoked
;; or a list ( skipnum numtriggers)
(define (trigger-with-limit name numhits . patts)
  (set!  *triggers* (cons (vector name patts numhits 0) *triggers*)))

;;======================================================================
;; Sections
;;======================================================================

;; (list (1 "Header" (patt1 patt2 patt3 ...)) (2 ...))
(define *sections*  '()) ;; (make-hash-table))

(define-inline (section:get-name vec)(vector-ref vec 0))
(define-inline (section:get-start-trigger vec)(vector-ref vec 1))
(define-inline (section:get-end-trigger vec)(vector-ref vec 2))

(define (section name start-trigger end-trigger)
  (set! *sections* (cons (vector name start-trigger end-trigger) *sections*)))

;;======================================================================
;; Expects
;;======================================================================
(define *expects*   (make-hash-table))
(define *curr-expect-num* 0)

(define in     'in)
(define not-in 'not-in)
(define before 'before)
(define after  'after)

(define-inline (expects:get-where      vec)(vector-ref vec 0))
(define-inline (expects:get-section    vec)(vector-ref vec 1))
(define-inline (expects:get-comparison vec)(vector-ref vec 2))
(define-inline (expects:get-comparison-as-text vec)
  (let ((comp (expects:get-comparison vec)))
    (cond
     ((eq? comp =) "=")
     ((eq? comp >) ">")
     ((eq? comp <) "<")
     (else "unk"))))
(define-inline (expects:get-value      vec)(vector-ref vec 3))
(define-inline (expects:get-name       vec)(vector-ref vec 4))
(define-inline (expects:get-count      vec)(vector-ref vec 5))
(define-inline (expects:inc-count      vec)(vector-set! vec 5 (+ (expects:get-count vec) 1)))
(define-inline (expects:get-compiled-patts vec)(vector-ref vec 6))
(define-inline (expects:get-num        vec)(vector-ref vec 7))
(define-inline (expects:get-expires    vec)(vector-ref vec 8))
(define-inline (expects:get-type       vec)(vector-ref vec 9)) ;; 'expect 'ignore

;; where is 'in, 'before or 'after but only 'in is supported now.
;; (expect in "Header" > 0 "Copywrite" #/Copywrite/)
;; (expect not-in '("Header" "Footer") = 0 "ERROR" #/error/i)
;; NOTE: patts and section can be lists
;; (list rexp1 rexp2 ...)
;; '("section1" "section2" ....)
(define (expect where section comparison value name patts #!key (expires #f)(type 'expect))
  ;; note: (hier-hash-set! value key1 key2 key3 ...)
  (if (not (symbol? where))        (print "ERROR: where must be a symbol"))
  (if (not (string? section))      (print "ERROR: section must be a string"))
  (if (not (procedure? comparison))(print "ERROR: comparison must be one of > < or ="))
  (if (not (number? value))        (print "ERROR: value must be a number"))
  (if (not (string? name))         (print "ERROR: name must be a string"))
  (if (and expires (not (string? expires)))
      (print "ERROR: expires must be a date string MM/DD/YY, got " expires)
      (set! expires #f))
  (if (not (list? patts))
      (set! patts (list patts)))
  (for-each (lambda (rx)
	      (if (not (regexp? rx))
		  (print "ERROR: your regex is not valid: " rx)))
	    patts)
  (if expires
      (if (string-match #/^\d+\/\d+\/\d+$/ expires)
	  (let ((secs (local-time->seconds (string->time expires "%D"))))
	    (set! expires secs))
	  (begin
	    (print "WARNING: Couldn't parse date: " expires ", date should be MM/DD/YY")
	    (set! expires (- (current-seconds) 1000000)))))
  (if (or (not expires)
	  (not (and expires (> expires (current-seconds)))))
      (for-each
       (lambda (sect)
	 (hash-table-set! *expects*
			  sect ;;         0     1       2       3     4  5   6         7               8      9
			  (cons (vector where sect comparison value name 0 patts *curr-expect-num* expires type)
				(hash-table-ref/default *expects* section '()))))
       (if (list? section) section (list section))))
  (set! *curr-expect-num* (+ *curr-expect-num* 1)))

(define (expect:warning where section comparison value name patts #!key (expires #f)(type 'warning))
  (expect where section comparison value name patts expires: expires type: type))

(define (expect:ignore where section comparison value name patts #!key (expires #f)(type 'ignore))
  (expect where section comparison value name patts expires: expires type: type))

(define (expect:error where section comparison value name patts #!key (expires #f)(type 'error))
  (expect where section comparison value name patts expires: expires type: type))

(define (expect:required where section comparison value name patts #!key (expires #f)(type 'required))
  (expect where section comparison value name patts expires: expires type: type))

(define (expect:get-type-info expect)
  (case (expects:get-type expect)
    ((expect)   (vector "Expect"   "red"))
    ((ignore)   (vector "Ignore"   "green"))
    ((error)    (vector "Error"    "red"))
    ((warning)  (vector "Warning"  "yellow"))
    ((required) (vector "Required" "purple"))
    (else       (vector "Error"    "red"))))

(define-inline (expect:expect-type-get-type  vec)(vector-ref vec 0))
(define-inline (expect:expect-type-get-color vec)(vector-ref vec 1))

;;======================================================================
;; Cmdfile parser *sigh*, I really didn't want to have to write this
;;======================================================================

;; (define (parse-logpro fname)
;;   (let ((inp (open-input-file fname))
;; 	(blank-rx   #/^\s*$/)
;; 	(command-rx #/^\s*(trigger|section|expect)\s+(.*)$/)
;; 	(comment-rx #/^\s*#/)
;; 	(string-rx  #/^\s*\"([^\"]*)\"\s*/)
;; 	(param-rx   #/^\s*([^:\s]+):\s*/)
;; 	(integer-rx #/^\s*([0-9]+)\s*/)
;; 	(compare-rx #/^\s*([><=][><=]{0,1})\s*/))
;;     (let loop ((inl     (read-line inp)) ;; keep it simple, line oriented. One line per command
;; 	       (lnum    0))
;;       (if (eof-object? inl)
;; 	  (run-cmd cmdlst)
;; 	  (let* ((match (string-search command-rx inl))
;; 		 (cmd   (if match (string->symbol (cadr match)) #f))
;; 		 (reml  (if match (caddr match) #f)))
;; 	    (if (not cmd)
;; 		(begin
;; 		  (if (not (or (string-match comment-rx inl)
;; 			       (string-match blank-rx   inl)))
;; 		      (print "ERROR: Couldn't parse line " lnum "\n => \"" inl "\""))
;; 		  (loop (read-line)(+ lnum 1))))
;; 	    (loop (read-line)
;; 		  cmdlst)
;; 	    ;; now the loop to extract the tokens for this line
;; 	    (let cmdloop ((rem     reml)
;; 			  (params '()))
;; 	      (case cmd
;; 		((trigger)
;; 		
;; 	      
	      

;;======================================================================
;; Main
;;======================================================================

(define *htmlport* #f)

(define (process-log-file cmdfname . htmlfile)
  (cond 
   ((not (file-exists? cmdfname))
    (print "ERROR: command file " cmdfname " not found")
    (exit 2))
   (else
    (let* ((html-file (if (not (null? htmlfile))
			  (car htmlfile)))
	   (html-port (if html-file (open-output-file html-file) #f)))
      (set! *htmlport* html-port) ;; sigh, do me right some day...
      (eval '(require-extension regex-literals))
      (eval '(require-extension regex))
      (handle-exceptions
       exn
       (begin
	 (print "\nERROR: Syntax error in your command file!\n")
	 (print " =>  " ((condition-property-accessor 'exn 'message) exn))
         (html-print "\nERROR: Syntax error in your command file!\n")
         (html-print " =>  " ((condition-property-accessor 'exn 'message) exn))
	 (close-output-port *htmlport*)
	 (exit 1))
       (load cmdfname))
      (analyze-logfile)
      (print-results)
     ))))

(define (adj-active-sections trigger active-sections)
  (for-each 
   (lambda (section)
     (let ((section-name  (section:get-name section))
	   (start-trigger (section:get-start-trigger section))
	   (end-trigger   (section:get-end-trigger section)))
       (cond
	((string=? start-trigger (trigger:get-name trigger))
	 (hash-table-set! active-sections section-name section))
	((string=? end-trigger (trigger:get-name trigger))
	 (hash-table-delete! active-sections section-name)))))
   *sections*))
	
(define (html-print . stuff)
  (if *htmlport*
      (with-output-to-port
	  *htmlport*
	  (lambda ()
	    (apply print stuff)))))

(define (analyze-logfile)
  (let ((active-sections  (make-hash-table))
	(found-expects    '())
	(html-mode        'pre))
    ;; (curr-seconds     (current-seconds)))
    (html-print "<html><header>LOGPRO RESULTS</header><body><pre>")
    (let loop ((line (read-line))
	       (line-num  0))
      (if (not (eof-object? line))
	  (begin
	    ;; first find if any triggers are hit
	    ;; (print "find any trigger hits")
	    (for-each 
	     (lambda (trigger)
	       (let ((patts (trigger:get-patts trigger))
		     (remhits (trigger:get-remaining-hits trigger)))
		 (if (and (> remhits 0)
			  (misc:line-match-regexs line patts))
		     (begin
		       (trigger:set-remaining-hits! trigger (- remhits 1))
		       (if (eq? html-mode 'pre)
			   (begin
			     (html-print "</pre>")
			     (set! html-mode 'non-pre))
			   (html-print "<br>"))
		       (html-print "<font color=\"blue\">")
		       (print      "LOGPRO: hit trigger " (trigger:get-name trigger) " on line " line-num)
		       (html-print "LOGPRO: hit trigger " (trigger:get-name trigger) " on line " line-num)
		       (html-print "</b></font>")
		       ;; add another flag to triggers and change this ....
		       (trigger:inc-total-hits trigger)
		       (adj-active-sections trigger active-sections)))))
	     *triggers*)

	    ;; now look for any expect "in" fails
	    ;; (print "looking for \"in\" fails")
	    (for-each 
	     (lambda (section)
	       (let ((expects (filter (lambda (x)(eq? 'in (expects:get-where x)))
				      (hash-table-ref/default *expects* section '()))))
		 (if expects
		     (for-each 
		      (lambda (expect)
			(let ((patts (expects:get-compiled-patts expect)))
			  (if (misc:line-match-regexs line patts)
			      (set! found-expects (cons (list expect section) found-expects)))))
		      expects))))
	     (hash-table-keys active-sections))

	    ;; now look for any expect "not-in" fails
	    ;; (print "looking for \"not-in\" fails")
	    (for-each 
	     (lambda (section)
	       (let ((expects (filter (lambda (x)(eq? 'not-in (expects:get-where x)))
				      (hash-table-ref/default *expects* section '()))))
		 (if expects
		     (for-each 
		      (lambda (expect)
			(let ((patts (expects:get-compiled-patts expect)))
			  (if (misc:line-match-regexs line patts)
			      (set! found-expects (cons (list expect section) found-expects)))))
		      expects))))
	     (filter (lambda (x)(not (member x (hash-table-keys active-sections))))
		     (map section:get-name *sections*)))

	    ;; from the expect hits choose the firstist one
	    ;; (print "choose the first matching expect")
	    (if (not (null? found-expects))
		(begin
		  ;; (print "found-expects: \n" (intersperse found-expects "\n"))
		  (let* ((dat     (car (sort found-expects (lambda (a b)
							     (let ((vala (expects:get-num (car a)))
								   (valb (expects:get-num (car b))))
							       (if (and (number? vala)(number? valb))
								   (< vala valb);; (print "car a: " (car a) " car b: " (car b))
								   (begin
								     (print "WARNING: You have triggered a bug, please report it.\n  vala: " vala " valb: " valb)
								     #f)))))))
			 (expect    (car dat))
			 (section   (cadr dat))
			 (type-info (expect:get-type-info expect)))
		    (if (eq? html-mode 'pre)
			(begin
			  (html-print"</pre>")
			  (set! html-mode 'non-pre))
			(html-print "<br>"))
		    (html-print (conc "<font color=\"" (expect:expect-type-get-color type-info) "\">"))
			;; (html-print "<font color=\"gold\">"))
		    (let ((msg (list
				"LOGPRO "  (expect:expect-type-get-type type-info) ": " 
				(expects:get-name expect) " "
				(expects:get-comparison-as-text expect) " " 
				(expects:get-value expect)
				" in section " section " on line " line-num)))
		      (apply print msg)
		      (apply html-print msg))
		    (html-print "</font>")
		    (expects:inc-count expect)
		    (set! found-expects '()))))
	    (print line)
	    (if (not (eq? html-mode 'pre))
		(begin
		  (html-print "<br><pre>")
		  (set! html-mode 'pre)))
	    (html-print line)
	    (loop (read-line)(+ line-num 1)))))))

(define (print-results)
  (let ((status #t)
        ;;        type where section OK/FAIL compsym value name count
	(fmt     "Expect:  ~8a ~2@a ~12a ~4@a, expected ~a ~a of ~a, got ~a")
	(fmt-trg "Trigger: ~13a ~15@a, count=~a"))
    ;; first print any triggers that didn't get triggered - these are automatic failures
    (print      "==========================LOGPRO SUMMARY==========================")
    (html-print "==========================LOGPRO SUMMARY==========================")
    (for-each
     (lambda (trigger)
       (let ((count (trigger:get-total-hits trigger)))
	 (if (< count 1)(set! status #f))
	 (let ((lineout (format #f fmt-trg (trigger:get-name trigger) (if (> count 0) "OK" "FAIL") count)))
	   (html-print lineout)
	   (print lineout))))
     *triggers*)
    ;; now print the expects
    (for-each 
     (lambda (section)
       (for-each 
	(lambda (expect)
	  (let ((where   (expects:get-where expect)) ;; not used yet, "in" is only option
		;; (section (expects:get-section expect))
		(comp     (expects:get-comparison expect))
		(value    (expects:get-value expect))
		(count    (expects:get-count expect))
		(name     (expects:get-name expect))
		(typeinfo (expect:get-type-info expect))
		(xstatus #t)
		(compsym "=")
		(lineout ""))
	    (cond
	     ((eq? comp =)
	      (set! xstatus (eq? count value))
	      (set! compsym "="))
	     ((eq? comp >)
	      (set! xstatus (> count value))
	      (set! compsym ">"))
	     ((eq? comp <)
	      (set! xstatus (< count value))
	      (set! compsym "<"))
	     ((eq? comp >=)
	      (set! xstatus (>= count value)))
	     ((eq? comp <=)
	      (set! xstatus (<= count value))))
	    (set! lineout (format #f fmt (vector-ref typeinfo 0) where section (if xstatus "OK" "FAIL") compsym value name count))
	    (html-print lineout)
	    (print lineout)
	    (if (not xstatus)
		(set! status #f))))
	(hash-table-ref *expects* section)))
     (hash-table-keys *expects*))
    (html-print "</pre></body></html>")
    (if status
	(exit 0)
	(exit 1))))

(define (setup-logpro)
  (use regex)
  (use regex-literals))
