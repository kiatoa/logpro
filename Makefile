logpro : logprocessor.scm logpro.scm
	csc -X regex -X regex-literals logpro.scm -o logpro

$(DEPLOYTARG)/logpro : logprocessor.scm logpro.scm
	csc -X regex -X regex-literals logpro.scm -deployed -o $(DEPLOYTARG)/logpro

logpro.profiled : logprocessor.scm logpro.scm
	csc -profile -X regex -X regex-literals logpro.scm -o logpro.profiled

test : logpro
	csi -q -b -n tests/run.scm 
	! ./logpro example.logpro example.html < example.log > example.out
	firefox example.html

examples :
	(./logpro example.logpro example.html < example.log > /dev/null; echo "expect error code = 1, got $$?")
	(./logpro example.logpro example-warn.html < example-warn.log > /dev/null; echo "expect warning code = 2, got $$?")

install : logpro
	cp logpro /usr/local/bin 
