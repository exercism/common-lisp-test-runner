(load "quicklisp/setup.lisp")
(ql:quickload "test-runner")

(let ((bin-dir (make-pathname :directory '(:relative "bin"))))
  (ensure-directories-exist bin-dir)
  (sb-ext:save-lisp-and-die (merge-pathnames "test-runner" bin-dir)
                          :toplevel #'test-runner:test-runner
                          :executable t))
