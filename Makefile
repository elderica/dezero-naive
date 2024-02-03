.DEFAULT_GOAL := test

test:
	sbcl --noinform --load "${HOME}/quicklisp/setup.lisp" --script scripts/run-tests.lisp

sb-cover:
	sbcl --noinform --load "${HOME}/quicklisp/setup.lisp" --script scripts/sb-cover.lisp

.PHONY: test sb-cover
