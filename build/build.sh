#/bin/sh -e

builder=$1
runner=$2

${builder} --load "./src/run.lisp" \
           --eval "(save-lisp-and-die \"${runner}\" :executable t :toplevel #'test-runner)" \
           --quit
