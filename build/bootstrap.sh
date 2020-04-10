#/bin/sh -e

image=$1

sbcl --load ./quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval "(ql:quickload '(:fiveam :dissect :st-json))" \
     --eval "(save-lisp-and-die \"$image\" :executable t)" \
     --quit
