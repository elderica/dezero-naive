.PHONY: test sb-cover quickload build-image test-image help
.DEFAULT_GOAL := help

test: ## runs test suites
	sbcl --noinform --non-interactive \
		--load scripts/load-asd.lisp \
		--load scripts/run-tests.lisp

sb-cover: ## collects code coverage using `:sb-cover`
	sbcl --noinform --non-interactive \
		--load scripts/load-asd.lisp \
		--load scripts/sb-cover.lisp

quickload: ## quickloads the system in advance
	sbcl --noinform --non-interactive \
		--load scripts/load-asd.lisp \
		--eval "(ql:quickload :dezero-naive.test)"

build-image: ## builds docker image
	docker buildx build --tag elderica/dezero-naive:test .

test-image: ## runs test suites in docker container with bind mount
	docker run -it --rm \
		--mount type=bind,source=$(shell pwd),target=/root/common-lisp/dezero-naive \
		elderica/dezero-naive:test \
		test
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
