SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

TEST-FILES := $(shell ls test/helm-ag-*.el)

.PHONY: clean checkdoc lint install compile unix-test

ci: clean install compile

install:
	@echo "Installing..."
	$(EASK) package
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

unix-test:
	@echo "Testing..."
	$(EASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

clean:
	$(EASK) clean-all
