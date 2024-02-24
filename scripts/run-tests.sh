#!/bin/sh

LISP=${LISP:-sbcl}

_no_debug_flag=""
case "${LISP}" in
    sbcl)
        _no_debug_flag="--non-interactive"
        ;;
    ecl)
        _no_debug_flag="--nodebug"
        ;;
esac

${LISP} "${_no_debug_flag}"  \
        --load scripts/load-asd.lisp \
        --load scripts/run-tests.lisp
