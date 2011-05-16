;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(define logpro-version 0.99)

(require-library chicken-syntax)

(include "logprocessor.scm")

;; (process-log-file (cadr (argv)))
	    
(let* ((args         (argv))
       (control-file (cadr args))
       (html-file    (if (> (length args) 2)
			 (caddr args)
			 #f)))
  (process-log-file control-file html-file))
