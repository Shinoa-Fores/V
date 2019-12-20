SHELL := bash
LISP := sbcl
INSTDIR=/usr/bin
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

all: ${LISP}

sbcl:
	@echo
	cd src ;\
	sbcl --disable-debugger \
		--load v.asd \
		--eval "(asdf:load-system 'v)" \
		--eval "(sb-ext:save-lisp-and-die #p\"v\" :toplevel #'v::main :executable t)" ;\
	mv v ../bin ;\
	cd keccak ;\
	sbcl --no-sysinit --no-userinit --disable-debugger \
		--load src/package.lisp \
		--load src/knobs.lisp \
		--load src/bits.lisp \
		--load src/cl-keccak.lisp \
		--eval "(sb-ext:save-lisp-and-die #p\"cl-keccak\" :toplevel #'cl-keccak::main :executable t)" ;\
	mv cl-keccak ../../bin/ksum ;\
	echo -e "\033[32mOK\033[0m " ;\
	echo "Build complete! Binaries are located in bin/ - Run \`make install\` to install them."

#credit: trinque
ccl:
	@echo
	cd src ;\
	ccl --no-init \
		--load v.asd \
		--eval "(asdf:load-system 'v)" \
		--eval "(ccl:save-application #P\"v\" :toplevel-function #'v::main :prepend-kernel t)" ;\
	mv v ../bin ;\
	cd keccak ;\
	ccl --no-init \
		--load src/package.lisp \
		--load src/knobs.lisp \
		--load src/bits.lisp \
		--load src/cl-keccak.lisp
		--eval "(ccl:save-application #P\"cl-keccak\" :toplevel-function #'cl-keccak::main :prepend-kernel t)" ;\
	mv cl-keccak ../../bin/ksum ;\
	echo -e "\033[32mOK\033[0m " ;\
	echo "Build complete! Binaries are located in bin/ - Run \`make install\` to install them."

install:
	@echo 
	echo "Installing ..." ;\
	install -m 755 bin/v bin/ksum bin/vdiff ${INSTDIR}/ ;\
	echo -e "\033[32mOK\033[0m "

clean:
	@echo
	rm bin/v bin/ksum ;\
	echo -e "\033[32mOK\033[0m "
