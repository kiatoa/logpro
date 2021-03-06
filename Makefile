PREFIX ?= /usr/local
CHICKEN = $(shell which csi)
CHICKEN_DIR = $(shell dirname $(CHICKEN))

logpro : logprocessor.scm logpro.scm logpro_style.css.scm
	csc -X regex -X logprocessor.scm logpro.scm -o logpro

logpro/logpro : logprocessor.scm logpro.scm logpro_style.css.scm
	chicken-install -p logpro -deploy format srfi-69 srfi-1 posix regex regex-literals typed-records
	csc -X regex -X logprocessor.scm logpro.scm -deploy
	echo $(CHICKEN)
	echo $(CHICKEN_DIR)	
	rsync -av $(CHICKEN_DIR)/../lib/chicken/7/chicken.import.so logpro/
	rsync -av $(CHICKEN_DIR)/../lib/chicken/7/irregex.import.so logpro/

logpro_style.css.scm : logpro_style.css
	echo "(define *logpro_style.css* #<<EOF" > logpro_style.css.scm
	cat logpro_style.css >> logpro_style.css.scm
	echo "EOF" >> logpro_style.css.scm
	echo ")" >> logpro_style.css.scm

logpro.profiled : logprocessor.scm logpro.scm
	csc -profile -X regex -X regex-literals logpro.scm -o logpro.profiled

test : logpro
	cd tests;csi -q -b -n run.scm 
	! ./logpro example.logpro example.html waivers.logpro < example.log > example.out
	firefox --new-tab example.html

examples :
	(./logpro example.logpro example.html < example.log > /dev/null; echo "expect error code = 1, got $$?")
	(./logpro example.logpro example-warn.html < example-warn.log > /dev/null; echo "expect warning code = 2, got $$?")

install : logpro
	install logpro $(PREFIX)/bin
	mkdir -p $(PREFIX)/share/css
	install logpro_style.css $(PREFIX)/share/css
