LISP=sbcl

all: ${LISP}

sbcl:
	sbcl --no-sysinit --no-userinit --disable-debugger \
		--load src/package.lisp \
		--load src/knobs.lisp \
		--load src/bits.lisp \
		--load src/cl-keccak.lisp \
		--eval "(sb-ext:save-lisp-and-die #p\"cl-keccak\" :toplevel #'cl-keccak::main :executable t)"

ccl:
	ccl --no-init \
		--load src/package.lisp \
		--load src/knobs.lisp \
		--load src/bits.lisp \
		--load src/cl-keccak.lisp
		--eval "(ccl:save-application #P\"cl-keccak\" :toplevel-function #'cl-keccak::main :prepend-kernel t)"
