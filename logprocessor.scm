;; Copyright 2006-2016, Matthew Welland.
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

;; NOTES: 

;;======================================================================
;; Globals
;;======================================================================

(define *htmlport* #f)
(define *summport* #f)
(define *curr-expect-num* 0)

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
	      match ;; (car match)
	      (if (null? tal)
		  #f
		  (loop (car tal)(cdr tal))))))))

;; convert some procs to nice symbols
;; merge this with comp->string
(define (misc:op->symbol op)
  (cond
   ((or (string? op)
	(symbol? op)
	(number? op)) op)
   ((eq? op =)  '=)
   ((eq? op >)  '>)
   ((eq? op <)  '<)
   ((eq? op >=) '>=)
   ((eq? op <=) '<=)
   (else 'unk)))

;;======================================================================
;; Settings
;;======================================================================
   
(define *logpro:settings* (make-hash-table))

(define (logpro:set! var val)
  (hash-table-set! *logpro:settings* var val))

(define (logpro:get var)
  (hash-table-ref/default *logpro:settings* var #f))

(define (logpro:unset! var)
  (if (hash-table-exists? *logpro:settings* var)
      (hash-table-delete! *logpro:settings* var)))

;; some default settings
(for-each
 (lambda (datpair)
   (logpro:set! (car datpair)(cdr datpair)))
 '(("summdat" . #t))) ;; <fname>.dat is on by default

;;======================================================================
;; Hooks
;;======================================================================

(define *logpro:hooks* (make-hash-table))

;; if command is a string it is executed by system after substituting matches
;;    m1, m2, m3, m4 in #{m5} type string targets.
;;
;; if command is a proc it is called with the list of match results
;;
(define (hook:add name command #!key (one-time #f))
  (hash-table-set! *logpro:hooks* name (vector command one-time)))

;; escape single quotes and surround with single quotes
(define (hook:command-param-escape val)
  (conc "'"
	(string-substitute (regexp "(\'{1})") "\\\'" 
			   val)
	"'"))
;; (string-substitute (regexp "(\"{1})") "\\\"" val #t) #t))

(define (hook:subst-var hookstr var val)
  (string-substitute 
   (regexp (conc "#\\{escaped " var "\\}")) 
   (hook:command-param-escape val)
   (string-substitute (regexp (conc "#\\{" var "\\}")) (conc val)
		      hookstr #t) #t))

;; Variables to be substituted:
;;
;; line: the entire line matched
;; msg:  the expect message (usually the 5th parameter)
;; m1, m2 .... mN the regex matches
;;
(define (hook:process-line hookstr msg matches)
  ;; (print "HOOK:PROCESS-LINE hookstr: " hookstr "\nmsg: " msg "\nmatches: " matches)
  (let ((line  (car matches))
	(subm  (cdr matches))
	(res   ""))
    (set! res (hook:subst-var hookstr "msg" msg))
    (set! res (hook:subst-var res "line" line))
    (if (null? subm) res 
	(let loop ((hed (car subm))
		   (tal (cdr subm))
		   (cur 1))
	  (set! res (hook:subst-var res (conc "m" cur) hed))
	  (if (null? tal) res
	      (loop (car tal)(cdr tal)(+ cur 1)))))))

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
(define-inline (trigger:get-required-flag vec)(vector-ref vec 4))

;; Triggers default to one hit 
(define (trigger name . patts)
  (set! *triggers* (cons (vector name patts 1 0 #t) *triggers*)))

;; Do we want lifetime control? 0 forever, or N for number of times it may be invoked
;; or a list ( skipnum numtriggers)
(define (trigger-with-limit name numhits . patts)
  (set!  *triggers* (cons (vector name patts numhits 0 #t) *triggers*)))

(define (trigger:non-required name . patts)
   (set! *triggers* (cons (vector name patts 1 0 #f) *triggers*)))
 

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

;; Add the default section "LogFileBody"
(trigger "LogFileBodyStart" #/.*/)
(section "LogFileBody" "LogFileBodyStart" "LogFileBodyEnd")

;; For those of us who wish to save a little typing (is this mildly dangerous?)
(define logfile "LogFileBody")

;;======================================================================
;; Expects
;;======================================================================
(define *expects*   (make-hash-table))
;; expect links lookup table, each key_<n> entry tracks the last error pointed
;; to. Example: <a name="#key0_1"></a> for expect #0, second occurance
(define *expect-link-nums* (make-hash-table))

(define in     'in)
(define not-in 'not-in)
(define before 'before)
(define after  'after)

(define-inline (comp->text comp)
  (case comp
   ((=)    "=")
   ((>)    ">")
   ((<)    "<")
   ((>=)  ">=")
   ((<=)  "<=")
   (else "unk")))

;; for the given ops spit out the right html else return the instring
(define (text->html indat)
  (let ((instr (if (string? indat)
		   indat
		   (conc indat))))
    (string-substitute 
     ">" "&gt;"
     (string-substitute
      "<" "&lt;"
      instr))))

;;     (cond
;;      ((eq? comp =)    "=")
;;      ((eq? comp >)    ">")
;;      ((eq? comp <)    "<")
;;      ((eq? comp >=)  ">=")
;;      ((eq? comp <=)  "<=")
;;      ((string? comp) comp)
;;      (else "unk"))))

(define-inline (expects:get-where      vec)(vector-ref vec 0))
(define-inline (expects:get-section    vec)(vector-ref vec 1))
(define-inline (expects:get-comparison vec)(vector-ref vec 2))
(define-inline (expects:get-comparison-as-text vec)
  (let ((comp (expects:get-comparison vec)))
    (if (string? comp) comp (comp->text comp))))
(define-inline (expects:get-value      vec)(vector-ref vec 3))
(define-inline (expects:get-name       vec)(vector-ref vec 4))
(define-inline (expects:get-count      vec)(vector-ref vec 5))
(define-inline (expects:inc-count      vec)(vector-set! vec 5 (+ (expects:get-count vec) 1)))
(define-inline (expects:get-compiled-patts vec)(vector-ref vec 6))
(define-inline (expects:get-num        vec)(vector-ref vec 7))
(define-inline (expects:get-expires    vec)(vector-ref vec 8))
(define-inline (expects:get-type       vec)(vector-ref vec 9)) ;; 'expect 'ignore
(define-inline (expects:get-keyname    vec)(vector-ref vec 10))
(define-inline (expects:get-tol        vec)(vector-ref vec 11))
(define-inline (expects:get-measured   vec)(vector-ref vec 12))
(define-inline (expects:get-val-pass-count vec)
  (let ((pfv (vector-ref vec 13)))
    (vector-ref pfv 0)))
(define-inline (expects:get-val-fail-count vec)
  (let ((pfv (vector-ref vec 13)))
    (vector-ref pfv 1)))
(define-inline (expects:inc-val-pass-count vec)
  (let ((pfv (vector-ref vec 13)))
    (vector-set! pfv 0 (+ 1 (vector-ref pfv 0)))))
(define-inline (expects:inc-val-fail-count vec)
  (let ((pfv (vector-ref vec 13)))
    (vector-set! pfv 1 (+ 1 (vector-ref pfv 1)))))

(define-inline (expects:set-measured   vec val)(vector-set! vec 12 (cons val (expects:get-measured vec))))
(define-inline (expects:set-val-pass/fail vec val)(vector-set! vec 13 val))

(define-inline (expects:get-hook-ptr   vec)(vector-ref vec 14))
(define-inline (expects:get-hook vec)
  (vector-ref 
     (hash-table-ref/default *logpro:hooks* (expects:get-hook-ptr vec)(vector #f #f))
     0))
;; returns #t if it is a one-time hook
(define-inline (expects:get-hook-type vec)
  (vector-ref 
     (hash-table-ref/default *logpro:hooks* (expects:get-hook-ptr vec)(vector #f #f))
     1))
(define-inline (expects:delete-if-one-time vec)
  (if (expects:get-hook-type vec)
      (hash-table-delete *logpro:hooks* (expects:get-hook-ptr vec))))
(define-inline (expects:get-matchnum vec)(vector-ref vec 15))
(define-inline (expects:get-rulenum  vec)(vector-ref vec 16))

;; where is 'in, 'before or 'after but only 'in is supported now.
;; (expect in "Header" > 0 "Copywrite" #/Copywrite/)
;; (expect not-in '("Header" "Footer") = 0 "ERROR" #/error/i)
;; NOTE: patts and section can be lists
;; (list rexp1 rexp2 ...)
;; '("section1" "section2" ....)

(define *got-an-error* #f)

(define (print:error msg . remmesg)
  (set! *got-an-error* #t)
  (apply print msg remmesg))

;; if expires is a date convert it to seconds until or since expired
;; #t => rule is expired, no longer apply it
;; #f => rule is not expired, it still applies
(define (expect:process-expires expires)
  (let ((ex-val (if expires
		    (if (string-match #/^\d+\/\d+\/\d+$/ expires)
			(local-time->seconds (string->time expires "%m/%d/%Y"))
			(begin
			  (print "WARNING: Couldn't parse date: " expires ", date should be MM/DD/YY")
			  #f))
		    #f)))
    ;; now have #f: no expire spec'd, -ve num: expired, +ve num: not expired
    ;; (print "expires: " expires " type: " type " ex-val: " ex-val)
    (if ex-val
	(<= ex-val (current-seconds))  ;; expire specified
	#f)))

(define (expect where section comparison value name patts #!key (expires #f)(type 'error)(hook #f)(color #f))
  ;; note: (hier-hash-set! value key1 key2 key3 ...)
  (if (not (symbol? where))        (print:error "ERROR: where must be a symbol"))
  (if (not (or (string? section)
	       (list? section)))   (print:error "ERROR: section must be a string or a list of strings"))
  (if (not (procedure? comparison))(print:error "ERROR: comparison must be one of > < >= <= or ="))
  (if (not (number? value))        (print:error "ERROR: value must be a number"))
  (if (not (string? name))         (print:error "ERROR: name must be a string"))
  (if (and expires (not (string? expires)))
      (print:error "ERROR: expires must be a date string MM/DD/YY, got " expires))
  (if (not (list? patts))
      (set! patts (list patts)))
  (for-each (lambda (rx)
	      (if (not (regexp? rx))
		  (print:error "ERROR: your regex is not valid: " rx)))
	    patts)

  ;; #f => rule is not expired, go ahead and apply it
  ;; #t => rule is expired, do NOT apply it
  (if (not (expect:process-expires expires))
      (begin
	;; (print "expect:" type " " section " " (comp->text comparison) " " value " " patts " expires=" expires " hook=" hook)
	(for-each
	 (lambda (sect)
	   (hash-table-set! *expects*;;                                                                                                          11  12  13           14   15 16          
			    sect ;;         0     1       2       3     4  5   6         7               8      9 10                            tol  measured value=pass/fail  *curr-expect-num*
			    (cons (vector where sect comparison value name 0 patts *curr-expect-num* expires type (conc "key_" *curr-expect-num*) #f '() (vector 0 0) hook #f *curr-expect-num* color)
				  (hash-table-ref/default *expects* section '()))))
	 (if (list? section) section (list section))))
      (print "expect:" type " " section " " (comp->text comparison) " " value " " patts " expires=" expires " hook=" hook))

  (set! *curr-expect-num* (+ *curr-expect-num* 1)))

(define (expect:warning where section comparison value name patts #!key (expires #f)(type 'warning)(hook #f))
  (expect where section comparison value name patts expires: expires type: type hook: hook))

(define (expect:ignore where section comparison value name patts #!key (expires #f)(type 'ignore)(hook #f))
  (expect where section comparison value name patts expires: expires type: type hook: hook))

(define (expect:waive where section comparison value name patts #!key (expires #f)(type 'waive)(hook #f))
  (expect where section comparison value name patts expires: expires type: type hook: hook))

(define (expect:error where section comparison value name patts #!key (expires #f)(type 'error)(hook #f))
  (expect where section comparison value name patts expires: expires type: type hook: hook))

(define (expect:required where section comparison value name patts #!key (expires #f)(type 'required)(hook #f))
  (expect where section comparison value name patts expires: expires type: type hook: hook))

(define (expect:check where section comparison value name patts #!key (expires #f)(type 'check)(hook #f))
  (expect where section comparison value name patts expires: expires type: type hook: hook))

(define (expect:abort where section comparison value name patts #!key (expires #f)(type 'abort)(hook #f))
  (expect where section comparison value name patts expires: expires type: type hook: hook))

(define (expect:skip where section comparison value name patts #!key (expires #f)(type 'skip)(hook #f))
  (expect where section comparison value name patts expires: expires type: type hook: hook))


;;======================================================================
;; TODO: Compress this in with the expect routine above
;;======================================================================
(define (expect:value where section value tol name patt #!key (expires #f)(type 'value)(matchnum 1)(hook #f)(color #f))
  ;; note: (hier-hash-set! value key1 key2 key3 ...)
  (if (not (symbol? where))        (print:error "ERROR: where must be a symbol"))
  (if (not (or (string? section)
	       (list? section)))   (print:error "ERROR: section must be a string or list of strings"))
  (if (not (number? value))        (print:error "ERROR: value must be a number"))
  (if (not (or (number? tol)
	       (member tol (list < > <= >= =))))
      ((print:error "ERROR: tolerance must be a number or one of < > <= >= =")))
  (if (not (string? name))         (print:error "ERROR: name must be a string"))
  (if (and expires (not (string? expires)))
      (print:error "ERROR: expires must be a date string MM/DD/YY, got " expires)
      (set! expires #f))
  (if (not (regexp? patt))
      (print:error "ERROR: your regex is not valid: " rx))

  ;; Change methodology here. Expires becomes a flag with the following meaning
  ;;    #f             : no expires specified
  ;;   negative number : seconds since this rule expired
  ;;   postive number  : seconds until this rule expires
  (if (not (expect:process-expires expires)) ;; #f means yes, apply the rule, #t means no, do not apply the rule, i.e. if expired do not apply the rule
      (for-each
       (lambda (sect)
	 (hash-table-set! *expects* ;; comparison is not used                 matchnum used to pick the match from the regex
			  sect ;;         0     1       2       3  4   5       6                   7               8      9   10                               11 12  value=pass/fail
			  (cons (vector where sect    "<=>" value name 0 (list patt) *curr-expect-num* expires type (conc "key_" *curr-expect-num*) tol '() (vector 0 0) hook matchnum *curr-expect-num* color)
				(hash-table-ref/default *expects* section '()))))
       (if (list? section) section (list section))))
  (set! *curr-expect-num* (+ *curr-expect-num* 1)))

;; extract out the value if possible.
(define (expect:value-compare expect match)
  ;; (print "expect:value-compare :\n   " expect "\n   " match)
  (let* ((which-match (if (expects:get-matchnum expect)
			  (expects:get-matchnum expect);; expects:get-compiled-patts returns a list (patt matchnum)
			  1)) ;; input is (patt) or (patt n) where n is the patt number to take as the value
	 (match-str (if (> (length match) which-match)(list-ref match which-match) #f))
	 (match-num (if (string? match-str)(string->number match-str) #f))
	 (value     (expects:get-value expect))
	 (tol       (expects:get-tol   expect)))
    (if match-num
	(let ((result (if (number? tol)
			  (and (<= match-num (+ value tol))
			       (>= match-num (- value tol)))
			  (tol match-num value))))
	  (list result match-num "ok"))
	(if match-str
	    (list #f match-str "match is not a number")
	    (list #f match     "regex matched but no captured value, use parens; (...)")))))
;; 
(define (expect:get-type-info expect)
  (case (expects:get-type expect)
    ((expect)   (vector "Expect"   "red"))
    ((ignore)   (vector "Ignore"   "green"))
    ((waive)    (vector "Waive"    "brown"))
    ((error)    (vector "Error"    "red"))
    ((warning)  (vector "Warning"  "orange"))
    ((required) (vector "Required" "purple"))
    ((check)    (vector "Check"    "pink"))
    ((abort)    (vector "Abort"    "crimson"))
    ((skip)     (vector "Skip"     "#d1db64"))
    ((value)    (vector "Value"    "blue"))
    (else       (vector "Error"    "red"))))

(define-inline (expect:expect-type-get-type  vec)(vector-ref vec 0))
(define-inline (expect:expect-type-get-color vec)(vector-ref vec 1))

;;======================================================================
;; Main
;;======================================================================

(define (process-log-file cmdfname html-file waiver-file cssfile)
  (cond 
   ((not (file-exists? cmdfname))
    (print:error "ERROR: command file " cmdfname " not found")
    (exit 1))
   (else
    (let* ((html-port (if html-file (open-output-file html-file) #f))
	   (summ-port (if (logpro:get "summdat")
			  (let ((fname (conc (pathname-strip-extension (if html-file
									   html-file
									   "summary"))
					     ".dat")))
			    (open-output-file fname))
			  #f)))
      (set! *htmlport* html-port) ;; sigh, do me right some day...
      (set! *summport* summ-port)
      (eval '(require-extension regex-literals))
      (eval '(require-extension regex))
      (handle-exceptions
       exn
       (begin
	 (print "\nERROR: Syntax error in your command file!\n")
	 (print " =>  " ((condition-property-accessor 'exn 'message) exn))
         (print-call-chain)
         (html-print "\nERROR: Syntax error in your command file!\n")
         (html-print " =>  " ((condition-property-accessor 'exn 'message) exn))
         (print-call-chain *htmlport*)
	 (close-output-port *htmlport*)
	 (exit 1))
       ;; load the waiver file first if specified
       (if (and waiver-file
		(file-exists? waiver-file)
		(file-read-access? waiver-file))
	   (load waiver-file))
       ;; Filter applied rules - defer this feature
       ;;   (let ((allowed-rules (logpro:get "allowed-rules")))
       ;;     (logpro:set "allowed-rules" '(waive)) ;; doesn't do anything yet - need to filter rules in waiver file with this.
       ;;     (load waiver-file)
       ;;     (if allowed-rules  ;; have prior settings? preserve them
       ;;         (logpro:set "allowed-rules" allowed-rules)
       ;;         (logpro:unset! "allowed-rules"))))
       ;; load the command file
       (load cmdfname))
      ;; if we got this far we can symlink in the css file
      (if (and cssfile (file-exists? cssfile)(not (file-exists? "logpro_style.css")))
	  (create-symbolic-link cssfile "logpro_style.css"))
      (analyze-logfile (current-output-port) cssfile) ;; cssfile is used as a flag
      (let ((exit-code (print-results cssfile)))
	(if *htmlport* (close-output-port *htmlport*))
	(if *summport* (close-output-port *summport*))	
	(exit exit-code))))))

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

(define (analyze-logfile oup cssfile)
  (let ((active-sections  (make-hash-table))
	(found-expects    '())
	(html-mode        'pre)
	(html-hightlight-flag #f))
    ;; (curr-seconds     (current-seconds)))
    (html-print "<html>")
    (if (file-exists? "logpro_style.css")
	(html-print "<link rel=\"stylesheet\" type=\"text/css\" href=\"logpro_style.css\">"))
    (html-print "<header>LOGPRO RESULTS</header><body>")
    (html-print "Summary is <a href=\"#summary\">here</a>")
    (html-print "<br>(processed by logpro version " logpro-version ", tool details at: <a href=\"http://www.kiatoa.com/fossils/logpro\">logpro</a>)")
    (html-print "<hr><pre>")
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
		       (set! html-highlight-flag (vector "blue" #f #f (trigger:get-name trigger) #f 'trigger))
		       (with-output-to-port oup
			 (lambda ()
			   (print      "LOGPRO: hit trigger " (trigger:get-name trigger) " on line " line-num)))
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
			(let* ((patts   (expects:get-compiled-patts expect))
			       (matches (misc:line-match-regexs line patts)))
			  (if matches
			      (set! found-expects (cons (list expect section matches) found-expects)))))
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
			(let* ((patts   (expects:get-compiled-patts expect))
			       (matches (misc:line-match-regexs line patts)))
			  (if matches
			      (set! found-expects (cons (list expect section match) found-expects)))))
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
								   (with-output-to-port oup
								     (lambda ()
								       (print "WARNING: You have triggered a bug, please report it.\n  vala: " vala " valb: " valb)
								       #f))))))))
			 (expect    (car dat)) ;;  BUGGG!!!! RENAME ME!!!!!!
			 (section   (cadr dat))
			 (match     (if (> (length dat) 2)(caddr dat) #f))
			 (type-info (expect:get-type-info expect))
			 (keyname   (expects:get-keyname   expect))
			 (errnum    (+ (hash-table-ref/default *expect-link-nums* keyname 0) 1))
			 (expect-type (expects:get-type expect))
			 (is-value  (eq? expect-type 'value))
			 (pass-fail (if is-value
					(expect:value-compare expect match)
					#f))
			 (color     (if is-value
					(if (car pass-fail) "green" "red")
					(expect:expect-type-get-color type-info)))
			 (expires   (expects:get-expires expect)))
		    (hash-table-set! *expect-link-nums* keyname errnum)
		    (if is-value
			(let ((extracted-value (cadr pass-fail)))
			  (expects:set-measured expect extracted-value)
			  (if (car pass-fail)
			      (expects:inc-val-pass-count expect)
			      (expects:inc-val-fail-count expect))))
		    (set! html-highlight-flag (vector color 
						      (conc keyname "_" errnum)
						      (conc "#" keyname "_" (+ 1 errnum))
						      #f
						      errnum
						      expect-type))
		    (let ((msg (list
				(expect:expect-type-get-type type-info) ": " 
				(expects:get-name expect) " "
				(if is-value
				    (let ((tol (expects:get-tol expect))
					  (val (expects:get-value expect)))
				      (conc (if (number? tol) val "") " "
					    (if (number? tol) "+/-" (conc (misc:op->symbol tol) " " val))
					    " got " (cadr pass-fail)
					    " which is " (if (car pass-fail) "PASS" "FAIL")))
				    (expects:get-comparison-as-text expect))
				" " 
				(expects:get-value expect)
				" in section " section " on line " line-num)))
		      (with-output-to-port oup
			(lambda ()
			  (apply print (cons "LOGPRO " msg))))
		      (if (and (not pass-fail)
			       (eq? expect-type 'error))
			  ;; failed error case
			  (let ((cmd    (expects:get-hook expect)))
			    (if cmd
				(let ((errhook (hook:process-line cmd line match)))
				  (with-output-to-port oup
				    (lambda ()
				      (print "ERRMSG HOOK CALLED: " errhook)))
				  (system errhook)
				  (expects:delete-if-one-time expect))))
			  (let ((cmd    (expects:get-hook expect)))
			    (if cmd
				(let ((hookcmd (hook:process-line cmd line match)))
				  (with-output-to-port oup
				    (lambda ()
				      (print "NONERR HOOK CALLED: " hookcmd)))
				  (system hookcmd)
				  (expects:delete-if-one-time expect))))))
		    (expects:inc-count expect)
		    (set! found-expects '()))))
	    (with-output-to-port oup
	      (lambda ()
		(print line)))
	    (if html-highlight-flag
		(let ((color (vector-ref html-highlight-flag 0))
		      (label (vector-ref html-highlight-flag 1))
		      (link  (vector-ref html-highlight-flag 2))
		      (mesg  (vector-ref html-highlight-flag 3))
		      (etype (vector-ref html-highlight-flag 5))) ;; the expect
		  (begin
		    ;(if (eq? html-mode 'pre)
		    ;    (html-print "</pre>")
		    ;    (html-print "<br>"))
		    (html-print "<a name=\"" label "\"></a>"
				"<a href=\"" link "\" " (if cssfile 
							    (conc "class=\"" etype "\">")
							    (conc "style=\"background-color: white; color: " color ";\">"))
				line
				"</a>")
		    (set! html-mode 'html)))
		(begin
		  (if (not (eq? html-mode 'pre))
		      (begin
			(html-print "") ; <pre>")
			(set! html-mode 'pre)))
		  (html-print line)))
	    (if html-highlight-flag (set! html-highlight-flag #f))
	    (loop (read-line)(+ line-num 1)))))))

(define (print-results cssfile) ;; cssfile is used as a flag
  (let ((status       #t)
	(toterrcount   0)
	(totwarncount  0)
	(totcheckcount 0)
	(totwaivecount 0)
	(totabortcount 0)
	(totskipcount  0)
	(tblfmt      (conc "<tr>"
			   (string-intersperse (map (lambda (x) "<td>~a</td>") '(1 2 3 4 5 6 7 8 9 10)) "")
			   "</tr>"))
	;;           type where section OK/FAIL compsym value name count
	(valfmt      " ~6a ~8a ~2@a ~12a ~4@a, expected ~a ~a ~a got ~a, ~a pass, ~a fail")
        ;;            type where section OK/FAIL compsym value name count
	(fmt         " ~6a ~8a ~2@a ~12a ~4@a, expected ~a ~a of ~a, got ~a")
	(fmt-trg     "Trigger: ~13a ~15@a, count=~a"))
    ;; first print any triggers that didn't get triggered - these are automatic failures
    (print      "==========================LOGPRO SUMMARY==========================")
    (html-print "<a name=\"summary\"></a>")
    (html-print "==========================LOGPRO SUMMARY==========================")
    (for-each
     (lambda (trigger)
       (let ((count (trigger:get-total-hits trigger)))
	 ;; Triggers are forcibly required unless you use the "trigger:not-required"
	 (if (and (< count 1)
		  (trigger:get-required-flag trigger))
	     (set! status #f))
	 (let* ((trigger-status (if (> count 0)
				    "OK"
				    (if (trigger:get-required-flag trigger)
					"FAIL"
					"OPTIONAL")))
		(lineout (format #f fmt-trg (trigger:get-name trigger) trigger-status count)))
	   (html-print lineout)
	   (print lineout))))
     *triggers*)
    ;; now print the expects
    (html-print "</pre><p><table>") ;;  style=\"width:100%\">") ;; border=\"1\" 
    (html-print "<tr><th>RuleNum</th><th>RuleType</th><th></th><th>Section</th><th>Status</th><th>Comp</th><th>Count/Val</th><th>Desc</th><th>Count</th></tr>")
    (for-each 
     (lambda (section)
       (print "\nExpects for " section " section: ")
       ;; (html-print "<tr><td colspan=\"11\">Expects for " section " section: </td></tr>")
       (for-each 
	(lambda (expect)
	  (let* ((where   (expects:get-where expect)) ;; not used yet, "in" is only option
		 ;; (section (expects:get-section expect))
		 (comp     (expects:get-comparison expect))
		 (value    (expects:get-value expect))
		 (count    (expects:get-count expect))
		 (name     (expects:get-name expect))
		 (typeinfo (expect:get-type-info expect))
		 (etype    (expects:get-type expect))
		 (keyname  (expects:get-keyname expect))
		 ;; xstatus is the expected vs. actual count of the item in question
		 (xstatus  #f) ;; Jul 08, 2011 - changed to #f - seems safer
		 (compsym  "=")
		 (lineout  "")
		 (is-value (eq? etype 'value))
		 (rulenum  (expects:get-rulenum expect)))
	    ;(print "is-value: " is-value)
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
	      (set! xstatus (<= count value)))
	     (is-value
	      (if (and (< (expects:get-val-fail-count expect) 1)
		       (> (expects:get-val-pass-count expect) 0))
		  (set! xstatus #t)
		  (set! xstatus #f))
	      ;(print "xstatus: " xstatus " fail-count: " (expects:get-val-fail-count expect) " pass-count: " (expects:get-val-pass-count expect))
	      ))
	    ;; the summary line for this rule
	    (if is-value
		;; If a value construct the output line using some kinda complicated logic ...
		(let* ((cmd       (expects:get-hook expect))
		       (tolerance (expects:get-tol expect))
		       (measured  (if (null? (expects:get-measured expect)) "-" (car (expects:get-measured expect))))
		       (outvals   #f))
		  (set! outvals (list  
				 (conc "rule-" rulenum)
				 (expect:expect-type-get-type typeinfo) 
				 where 
				 section 
				 (if xstatus "OK" "FAIL") 
				 (if (number? tolerance) value (misc:op->symbol tolerance))
				 (if (number? tolerance) "+/-" "")
				 (if (number? tolerance) (misc:op->symbol tolerance) value)
				 measured
				 (expects:get-val-pass-count expect) 
				 (expects:get-val-fail-count expect)
				 ))
		  (set! lineout (apply format #f valfmt outvals));; valfmt
		  ;; have a hook to process for "value" items, do not call if nothing found
		  (if (and cmd (number? measured))
		      (let ((valuehook (hook:subst-var
					(hook:subst-var 
					 (hook:subst-var 
					  (hook:subst-var cmd "measured" (conc measured))
					  "message" name)
					 "expected" (conc value))
					"tolerance" (conc (misc:op->symbol tolerance)))))
			(print "VALUE HOOK CALLED: " valuehook)
			(system valuehook)))
		  (if *summport*
		      (with-output-to-port *summport*
			(if is-value
			    (lambda ()
			      (print "[" (conc "rule-" rulenum) "]")
			      (print "operator " where )
			      (print "section " section )
			      (print "desc " name)
			      (print "status " (if xstatus "OK" "FAIL"))
			      (print "expected "  value)
			      (print "measured "  measured)
			      (if (number? tolerance)
				  (begin
				    (print "type +/-")
				    (print "tolerance " tolerance))
				  (begin
				    (print "type " (misc:op->symbol tolerance))))
			      (print "pass " (expects:get-val-pass-count expect))
			      (print "fail " (expects:get-val-fail-count expect))
			      (print))
			    (lambda ()
			      (print "[" (conc "rule-" rulenum) "]")
			      (print "type "(expect:expect-type-get-type typeinfo))
			      (print "operator " where)
			      (print "section " section)
			      (print "status "  (if xstatus "OK" "FAIL"))
			      (print "compsym " compsym)
			      (print "value " value)
			      (print "desc " name)
			      (print "count " count)
			      (print))))))
		;; If not a value create the output line using the format "fmt"
		(begin
		  (set! outvals (list
				 (conc "rule-" rulenum)
				 (expect:expect-type-get-type typeinfo)
				 where
				 section
				 (if xstatus "OK" "FAIL")
				 compsym 
				 value
				 name
				 count
				 ""
				 ))
		  (set! lineout (apply format #f fmt outvals));;  fmt 
		  ))
	    ;; now send lineout to the html file
	    (let ((color (if (> count 0)
			     (if is-value
				 (if xstatus "green" "red")
				 (expect:expect-type-get-color typeinfo))
			     (if (eq? etype 'required)
				 (if xstatus (expect:expect-type-get-color typeinfo) "red")
				 "white"))))
	      ;; (html-print (conc "<font color=\"" 
	      ;;   		color
	      ;;   		"\"><a name=\"" keyname "_" (+ 1 (hash-table-ref/default *expect-link-nums* keyname 0)) "\"></a>"
	      ;;   		(if (> count 0) (conc "<a href=\"#" keyname "_1\">Expect:</a>" ) "Expect:")
	      ;;   		lineout "</font>")))
	      (html-print "<tr><td "
			  (if cssfile
			      (conc "class=\"" etype "\"")
			      (conc "bgcolor=\"" color "\""))
			  "><a name=\"" keyname "_" (+ 1 (hash-table-ref/default *expect-link-nums* keyname 0)) "\"></a><a href=\"#" keyname "_1\">"
			  (text->html (car outvals)) "</a></td>"
			  "<td " (if cssfile 
				     (conc "class=\"" etype "\"")
				     (conc "bgcolor=\"" color "\""))
			  ">"
			  (text->html (cadr outvals)) "</td><td>" ;; (caddr outvals) "</td>"
			  (string-intersperse
			   (map text->html (cddr outvals))
			   (conc "</td><td>"))) ;; <a href=\"#" keyname "_1\">")))
	      (html-print "</td></tr>"))
	    ;; (html-print "</table>")
	    (if (> (string-length lineout) 0)(print "Expect:" lineout))
	    (if (not xstatus) ;; 
		(begin
		  (set! status #f)
		  (cond
		   ((eq? etype 'skip)
		    (set! totskipcount  (+ totskipcount  1)))
		   ((or (member etype '(error required value))) ;; (eq? etype 'error)(eq? etype 'required)(eq? etype 'value)(eq? etype 'waive))
		    (set! toterrcount   (+ toterrcount   1)))
		   ((eq? etype 'warning)
		    (set! totwarncount  (+ totwarncount  1)))
		   ((eq? etype 'abort)
		    (set! totabortcount (+ totabortcount 1)))
		   ((eq? etype 'check)
		    (set! totcheckcount (+ totcheckcount 1)))
		   ((eq? etype 'waive)
		    (set! totwaivecount (+ totwaivecount 1)))
		   )))))
		;; (begin
		;;   (cond
		;;    )))))
	(hash-table-ref *expects* section)))
     (hash-table-keys *expects*))
    (html-print "</table>")
    ;; (print "Total errors: " toterrcount)
    ;; (print "Total warnings: " totwarncount)
    ;; (print "status: " status)
    ;; (if (and (not *got-an-error*) status)
    ;;     (exit 0)
    (let ((res (cond ;; ordering here is critical as it sets the precedence of which status "wins"
		((> totskipcount  0) 6)
		((> totabortcount 0) 5)
		((> totcheckcount 0) 3)
		((> toterrcount   0) 1)
		((> totwarncount  0) 2)
		((> totwaivecount 0) 4)
		(*got-an-error*      (begin
				       (print "ERROR: Logpro error, probably in your command file. Look carefully at prior messages to help root cause.")
				       1))
		(status             0)
		(else               0))))
      (html-print "<h1 class=\"exitcode\">EXIT CODE: " res " ("
		  (case res
		    ((1) "FAIL")
		    ((2) "WARN")
		    ((3) "CHECK")
		    ((4) "WAIVE")
		    ((5) "ABORT")
		    ((6) "SKIP")
		    ((0) "PASS")))
      (html-print ")</h1></body></html>")
      res)))

(define (setup-logpro)
  (use regex)
  (use regex-literals))
