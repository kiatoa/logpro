PREFIX ?= /usr/local

logpro : logprocessor.scm logpro.scm logpro_style.css.scm
	csc -X regex -X regex-literals logpro.scm -o logpro

logpro/logpro : logprocessor.scm logpro.scm
	chicken-install -p logpro -deploy format srfi-69 srfi-1 posix regex regex-literals
	csc -X regex -X regex-literals logpro.scm -deploy

logpro_style.css.scm : logpro_style.css
	echo "(define *logpro_style.css* #<<EOF" > logpro_style.css.scm
	cat logpro_style.css >> logpro_style.css.scm
	echo "EOF" >> logpro_style.css.scm
	echo ")" >> logpro_style.css.scm

logpro.profiled : logprocessor.scm logpro.scm
	csc -profile -X regex -X regex-literals logpro.scm -o logpro.profiled

test : logpro
	csi -q -b -n tests/run.scm 
	! ./logpro example.logpro example.html waivers.logpro < example.log > example.out
	firefox --new-tab example.html

examples :
	(./logpro example.logpro example.html < example.log > /dev/null; echo "expect error code = 1, got $$?")
	(./logpro example.logpro example-warn.html < example-warn.log > /dev/null; echo "expect warning code = 2, got $$?")

install : logpro
	install logpro $(PREFIX)/bin
	mkdir -p $(PREFIX)/share/css
	install logpro_style.css $(PREFIX)/share/css
