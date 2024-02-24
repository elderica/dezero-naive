#!/bin/sh

set -u -e

if  type wget >/dev/null 2>&1
then
    wget -q -O quicklisp.lisp "https://beta.quicklisp.org/quicklisp.lisp"
elif type curl >/dev/null 2>&1
then
    curl -sS -o quicklisp.lisp "https://beta.quicklisp.org/quicklisp.lisp"
else
    echo 'install wget or curl'
    exit 1
fi

sha256sum -c - <<EOF
4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17  quicklisp.lisp
EOF

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
        --load quicklisp.lisp \
        --eval '(quicklisp-quickstart:install)' \
        --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
        --eval '(uiop:quit)'

rm quicklisp.lisp
