;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(define logpro-version 1.18)

(require-library chicken-syntax)

(include "logpro_style.css.scm")
(include "logprocessor.scm")

;; (use trace)
;; (trace pathname-directory)

;; (process-log-file (cadr (argv)))

(define logpro-usage	    
  (conc 
"Usage: logpro cmdfile [htmlfile] [waiverfile] > annotated.log < inputfile.log
  Exits with 
    error code = 0 on success,
    error code = 1 errors found,
    error code = 2 warnings found,
    error code = 3 check condition found,
    error code = 4 waivers found.
    error code = 5 abort signature found.
    error code = 6 skip signature found.
 Version " logpro-version "
  License GPL, more info about logpro at http://www.kiatoa.com/fossils/logpro"))

(let* ((args         (argv)))
  (if (< (length args) 2)
      (begin
	(print logpro-usage)
	(exit 1)))
  (let* ((control-file (cadr args))
	 (html-file    (if (> (length args) 2)
			   (caddr args)
			   #f))
	 (waiver-file  (if (> (length args) 3)
			   (list-ref args 3)
			   #f))
	 (bin-file     (car args))
	 (bin-home     (if bin-file (pathname-directory (readlink-f bin-file)) #f) )
	 (tool-home    (if bin-home (pathname-directory bin-home) #f))
	 (css-dir      (if tool-home (conc tool-home "/share/css") #f))
	 (full-css-file (conc (or (if html-file 
				      (pathname-directory html-file)
				      #f)
				  (current-directory))
			      "/logpro_style.css"))
	 (cssfile      (or (getenv "LOGPRO_CSS")
			   (if (file-exists? full-css-file) full-css-file #f)
			   (let ((cfile (conc css-dir "/logpro_style.css")))
			     (if (file-exists? cfile)
				 cfile
				 #f))
			   (if bin-home 
			       (let ((altfile (conc bin-home "/logpro_style.css")))
				 (if (file-exists? altfile)
				     altfile
				     #f))))))
    (print "\nbin-file: " bin-file "\nbin-home: " bin-home "\ncss-dir:  " css-dir "\ncssfile: " cssfile "\ntool-home: " tool-home)
    ;; else proceed to process the input files
    (process-log-file control-file html-file waiver-file cssfile)))

