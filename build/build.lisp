(load "quicklisp/setup")
(ql:quickload "test-runner")

(sb-ext:save-lisp-and-die "test-runner"
                          :toplevel #'test-runner:test-runner
                          :executable t)
