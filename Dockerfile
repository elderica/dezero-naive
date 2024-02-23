FROM alpine:latest

ENV QUICKLISP_SHA256SUM=4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17

RUN apk update \
 && apk add --no-cache sbcl make

RUN wget -q -O quicklisp.lisp "https://beta.quicklisp.org/quicklisp.lisp" \
 && (sha256sum quicklisp.lisp | grep ${QUICKLISP_SHA256SUM}) \
 && sbcl --non-interactive \
    --load quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
 && rm quicklisp.lisp

WORKDIR /root/common-lisp/dezero-naive

COPY . .

RUN make quickload

ENTRYPOINT ["make"]
