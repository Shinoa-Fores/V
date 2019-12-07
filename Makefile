LISP=sbcl

all: ${LISP}

sbcl:
	cd src ;\
	sbcl --disable-debugger \
		--load v.asd \
		--eval "(asdf:load-system 'v)" \
		--eval "(sb-ext:save-lisp-and-die #p\"v\" :toplevel #'v::main :executable t)" ;\
	mv v ..

#credit: trinque
ccl:
	cd src ;\
	ccl --no-init \
		--load v.asd \
		--eval "(asdf:load-system 'v)" \
		--eval "(ccl:save-application #P\"v\" :toplevel-function #'v::main :prepend-kernel t)" ;\
	mv v ..
