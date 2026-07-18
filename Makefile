EMACS ?= emacs

.PHONY: check test smoke byte-compile static shell

check: smoke static shell test byte-compile

test:
	$(EMACS) --batch -Q -l test/run-tests.el

smoke:
	$(EMACS) --batch -Q -l test/startup-smoke.el

byte-compile:
	$(EMACS) --batch -Q -l test/byte-compile.el

static:
	$(EMACS) --batch -Q -l test/static-checks.el

shell:
	bash -n bin/rich2org.sh test/rich2org-test.sh
	bash test/rich2org-test.sh
	@if command -v shellcheck >/dev/null 2>&1; then \
		shellcheck bin/rich2org.sh test/rich2org-test.sh; \
	else \
		printf '%s\n' 'shellcheck not installed; syntax and behavior checks passed'; \
	fi
