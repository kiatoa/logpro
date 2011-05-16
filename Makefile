logpro : logprocessor.scm logpro.scm
	csc -X regex -X regex-literals logpro.scm -o logpro

test :
	csi -n -b test.scm 

