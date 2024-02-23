.DEFAULT_GOAL := test

test:
	sbcl --noinform --non-interactive \
		--load scripts/load-asd.lisp \
		--load scripts/run-tests.lisp

sb-cover:
	sbcl --noinform --non-interactive \
		--load scripts/load-asd.lisp \
		--load scripts/sb-cover.lisp

quickload:
	sbcl --noinform --non-interactive \
		--load scripts/load-asd.lisp \
		--eval "(ql:quickload :dezero-naive.test)"

build-image:
	docker buildx build --tag elderica/dezero-naive:test .

test-image:
	docker run -it --rm \
		--mount type=bind,source=$(shell pwd),target=/root/common-lisp/dezero-naive \
		elderica/dezero-naive:test \
		test

.PHONY: test sb-cover quickload build-image test-image
