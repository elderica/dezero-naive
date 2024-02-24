FROM alpine:latest

RUN apk update \
 && apk add --no-cache sbcl make

WORKDIR /root/common-lisp/dezero-naive

COPY . .

RUN scripts/install-quicklisp.sh

RUN make quickload

ENTRYPOINT ["make"]
