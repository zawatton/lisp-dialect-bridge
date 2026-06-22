EMACS ?= emacs
SRC = ldb-ir.el ldb-scheme.el ldb-emit-elisp.el ldb-guix-importer.el ldb-cl.el lisp-dialect-bridge.el
TEST_SRC = test/ldb-guix-importer-test.el test/ldb-cl-test.el

EMACS_BATCH = $(EMACS) -Q --batch -L . -L test

.PHONY: all test compile clean help

all: test

help:
	@echo "make test     — run ERT suite"
	@echo "make compile  — byte-compile all .el files, warnings-as-errors"
	@echo "make clean    — remove .elc files"

test:
	$(EMACS_BATCH) -l ert \
	  $(foreach f,$(TEST_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS_BATCH) --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(SRC)

clean:
	rm -f *.elc test/*.elc
