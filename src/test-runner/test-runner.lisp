;;;; Exercism Common Lisp Test Runner

(defpackage test-runner
  (:use :cl)
  (:export :test-runner))

(in-package :test-runner)

;;; Set some parameters
(defvar *max-output-chars* 500)
(defvar *results-file* "results.json")

;;; Load and run tests
(defun get-test-results (slug src-path out-path)
  (let* ((*default-pathname-defaults* (truename src-path))
         (test-slug (format nil "~A-test" slug))
         (*package* (progn (load test-slug) (find-package (string-upcase test-slug))))
         (test-forms (astonish:load-forms-from-file test-slug))
         (test-macros (mapcar #'cadr (astonish:select-conses '(defmacro) test-forms)))
         (tests (mapcar (lambda (x) (astonish:macroexpand-select test-macros x))
                        (astonish:select-conses '(test) test-forms))))
    (mapcan #'process-test tests)))

(defun process-test (test)
  (let* ((*test-code* (with-output-to-string (*standard-output*)
                        (loop :for sexpr :in test :unless (atom sexpr)
                              :do (fresh-line)
                                  (write sexpr :pretty t :case :downcase))))
         (5am:*test-dribble* nil)
         (*standard-output* (make-string-output-stream))
         (results (fiveam:run (eval test)))
         (*test-output* (truncate-output (get-output-stream-string *standard-output*)))
         (*test-progress* (cons 0 (length results))))
    (declare (special *test-code* *test-output* *test-progress*))
    (loop :for result :in results
          :do (incf (car *test-progress*))
          :collect (process-test-result result))))

;;; Methods for processing different test results
(defmethod process-test-result ((result 5am::test-passed))
  (st-json:jso "name" (get-test-description result)
               "status" "pass"
               "message" :null
               "output" *test-output*
               "test_code" *test-code*))

(defmethod process-test-result ((result 5am::unexpected-test-failure))
  (st-json:jso "name" (get-test-description result)
               "status" "error"
               "message" (princ-to-string (5am::actual-condition result))
               "output" :null
               "test_code" *test-code*))

(defmethod process-test-result ((result 5am::test-failure))
  (st-json:jso "name" (get-test-description result)
               "status" "fail"
               "message" (5am::reason result)
               "output" *test-output*
               "test_code" *test-code*))

(defmethod process-test-result ((result 5am::test-skipped)))

;;; Get information from test-results
(defun get-test-description (test-result)
  (let* ((test-case   (5am::test-case test-result))
         (description (5am::description test-case))
         (name        (string (5am::name test-case))))
    (format nil "~A (~A/~A)"
            (if (string= description "") name description)
            (car *test-progress*)
            (cdr *test-progress*))))

(defun truncate-output (output)
  (if (> (length output) *max-output-chars*)
      (format nil "~A...~%Output was truncated. Please limit to ~A characters."
              (subseq output 0 *max-output-chars*) *max-output-chars*)
      output))

;;; Generate final report
(defun results-status (results)
  (every (lambda (res) (string= "pass" (st-json:getjso "status" res)))
         results))

(defmethod generate-report ((err error))
  (st-json:jso "version" 2
               "status" "error"
               "message" (princ-to-string err)))

(defmethod generate-report ((results list))
  (st-json:jso "version" 2
               "status" (if (results-status results) "pass" "fail")
               "tests" results))

;;; Invoke the test-runner
(defun test-runner ()
  (destructuring-bind (slug src-path out-path) (uiop:command-line-arguments)
    (let ((results-file (merge-pathnames (truename out-path) *results-file*)))
      (with-open-file (fs results-file :direction :output :if-exists :supersede)
        (st-json:write-json
         (generate-report
          (handler-case (get-test-results slug src-path out-path)
            (error (c) c)))
         fs)))))
