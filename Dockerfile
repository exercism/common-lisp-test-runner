FROM daewok/sbcl:alpine
RUN apk update && apk upgrade
ADD https://beta.quicklisp.org/quicklisp.lisp /
RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
RUN echo '(load "/root/quicklisp/setup.lisp")' > /root/.sbclrc
RUN sbcl --eval "(mapcar #'ql:quickload '(:alexandria :serapeum :bordeaux-threads :cl-ppcre :iterate :local-time :lparallel :named-readtables :series :cl-json :fiveam :trivial-benchmark :closer-mop :trivia))" --quit
