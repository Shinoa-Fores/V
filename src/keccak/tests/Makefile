LISP=sbcl

all: ${LISP}

sbcl:
	sbcl --no-sysinit --no-userinit --disable-debugger \
		--load ../src/package.lisp \
		--load ../src/knobs.lisp \
		--load ../src/bits.lisp \
		--load ../src/cl-keccak.lisp \
        --load cl-keccak-tests.lisp \
		--eval "(sb-ext:save-lisp-and-die #p\"cl-keccak-tests\" :toplevel #'cl-keccak::run-tests :executable t)"

ccl:
	ccl --no-init \
		--load ../src/package.lisp \
		--load ../src/knobs.lisp \
		--load ../src/bits.lisp \
		--load ../src/cl-keccak.lisp \
        --load cl-keccak-tests.lisp \
		--eval "(ccl:save-application #P\"cl-keccak-tests\" :toplevel-function #'cl-keccak::run-tests :prepend-kernel t)"
