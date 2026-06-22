EMACS ?= emacs
SRC = ldb-ir.el ldb-scheme.el ldb-emit-elisp.el ldb-guix-importer.el ldb-cl.el ldb-cl-macro.el lisp-dialect-bridge.el
TEST_SRC = test/ldb-guix-importer-test.el test/ldb-cl-test.el
# Differential + macro suites: need an external SBCL on PATH (skip cleanly without).
DIFF_SRC = test/ldb-cl-sbcl-diff.el test/ldb-cl-macro-test.el test/ldb-cl-alexandria-test.el

EMACS_BATCH = $(EMACS) -Q --batch -L . -L test

.PHONY: all test difftest compile clean help

all: test

help:
	@echo "make test     — run ERT suite (hermetic, no external deps)"
	@echo "make difftest — run SBCL-oracle differential suite (needs sbcl)"
	@echo "make compile  — byte-compile all .el files, warnings-as-errors"
	@echo "make clean    — remove .elc files"

test:
	$(EMACS_BATCH) -l ert \
	  $(foreach f,$(TEST_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

difftest:
	$(EMACS_BATCH) -l ert \
	  $(foreach f,$(DIFF_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS_BATCH) --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(SRC)

clean:
	rm -f *.elc test/*.elc
