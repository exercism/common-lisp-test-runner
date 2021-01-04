(defsystem "test-runner"
  :name "test-runner"
  :version "0.0.0"
  :description "Exercism Common Lisp Test-Runner"

  :depends-on ("uiop" "fiveam" "st-json" "alexandria")

  :pathname "test-runner"
  :serial t
  :components ((:file "astonish")
               (:file "test-runner" :depends-on ("astonish"))))
