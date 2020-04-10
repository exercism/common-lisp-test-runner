;;;; Exercism Common Lisp Test Runner

;; ;;; Setup Quicklisp & FiveAM
;; (load "/root/quicklisp/setup.lisp")
;; (ql:quickload '(:fiveam :dissect :st-json))

;;; Set some parameters
(defvar *max-output-chars* 500)
(defvar *results-file* "results.json")

;;; Load and run tests
(defun get-test-results (slug src-path out-path)
  (let ((test-slug (format nil "~A-test" slug))
        (*default-pathname-defaults* (truename src-path)))
    (load test-slug)
    (uiop:symbol-call (string-upcase test-slug) :run-tests nil)))

;;; Methods for processing different test results
(defmethod process-test-result ((result 5am::test-passed))
  (st-json:jso "name" (get-test-description result)
               "status" "pass"
               "message" :null
               "output" (get-test-output result)))

(defmethod process-test-result ((result 5am::unexpected-test-failure))
  (st-json:jso "name" (get-test-description result)
               "status" "error"
               "message" (dissect:present (5am::actual-condition result) nil)
               "output" :null))

(defmethod process-test-result ((result 5am::test-failure))
  (st-json:jso "name" (get-test-description result)
               "status" "fail"
               "message" (5am::reason result)
               "output" (get-test-output result)))

(defmethod process-test-result ((result 5am::test-skipped)))

;;; Get information from test-results
(defun get-test-description (test-result)
  (let* ((test-case   (5am::test-case test-result))
         (description (5am::description test-case))
         (name        (string (5am::name test-case))))
    (if (= (length description) 0) name description)))

(defun get-test-output (test-result)
  (truncate-output
    (with-output-to-string (*standard-output*)
      (eval (5am::test-expr test-result)))))

(defun truncate-output (output)
  (if (> (length output) *max-output-chars*)
      (format nil "~A...~%Output was truncated. Please limit to ~A characters."
              (subseq output 0 *max-output-chars*) *max-output-chars*)
      output))

;;; Generate final report
(defun results-status (results)
  (cond ((null results) "error")
        ((5am:results-status results) "pass")
        (t "fail")))

(defun generate-report (results &optional error)
  (st-json:jso "status" (results-status results)
               "message" (unless results (dissect:present error nil))
               "tests" (remove nil (mapcar #'process-test-result results))))

;;; Invoke the test-runner
(defun test-runner ()
  (destructuring-bind (slug src-path out-path) (uiop:command-line-arguments)
    (let ((results-file (merge-pathnames (truename out-path) *results-file*)))
      (with-open-file (fs results-file :direction :output :if-exists :supersede)
        (st-json:write-json
         (handler-case (generate-report (get-test-results slug src-path out-path))
           (error (c) (generate-report nil c)))
         fs))))
  (format t "~%")
  (values))

;; (test-runner)
 
 
