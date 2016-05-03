;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(define logpro-version 1.14)

(require-library chicken-syntax)

(include "logprocessor.scm")

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
	 (bin-home     (pathname-directory (pathname-directory (car args))))
	 (css-dir      (if bin-home (conc bin-home "/share/css") #f))
	 (cssfile      (or (getenv "LOGPRO_CSS")
			   (if (file-exists? "logpro_style.css") "logpro_style.css" #f)
			   (if css-dir  (conc css-dir "/logpro_style.css") #f))))
    ;; else proceed to process the input files
    (process-log-file control-file html-file waiver-file cssfile)))
