#!/usr/local/bin/sbcl --script
;;;; Common Lisp Exercism Test Runner

;; Setup Quicklisp & FiveAM
(load "/root/quicklisp/setup.lisp")
(ql:quickload '(:fiveam :dissect :st-json))

;; Set some parameters
(defvar *max-output-chars* 500)

;; These functions desperately need better names
(defun get-test-results (slug src-path out-path)
  (let ((test-slug (format nil "~A-test" slug))
        (*default-pathname-defaults* (truename src-path)))
    (load test-slug)
    (uiop:symbol-call (string-upcase test-slug) :run-tests nil)))

;; (defun process-test-case (test-case)
;;   (etypecase test-case
;;     (5am::test-passed "Gucci")
;;     (5am::unexpected-test-failure "Shit")
;;     (5am::test-failure "Stinky")
;;     (5am::test-skipped "Yeet")))

;; Not a fan of this just yet
(defun get-test-description (test-result)
  (let* ((test-case   (5am::test-case test-result))
         (description (5am::description test-case))
         (name        (string (5am::name test-case))))
    (if (= (length description) 0) name description)))

(defmethod process-test-result ((result 5am::test-passed))
  (st-json:jso "name" (get-test-description result)
               "status" "pass"
               "message" nil
               "output" (get-output result)))

(defmethod process-test-result ((result 5am::unexpected-test-failure))
  (st-json:jso "name" (get-test-description result)
               "status" "error"
               "message" (dissect:present (5am::actual-condition result) nil)
               "output" nil))

(defmethod process-test-result ((result 5am::test-failure))
  (st-json:jso "name" (get-test-description result)
               "status" "fail"
               "message" (5am::reason result)
               "output" (get-output result)))

(defmethod process-test-result ((result 5am::test-skipped)))

(defun get-output (test-result)
  (truncate-output
    (with-output-to-string (*standard-output*)
      (eval (5am::test-expr test-result)))))

(defun truncate-output (output)
  (if (> (length output) *max-output-chars*)
      (format nil "~A...~%Output was truncated. Please limit to ~A characters."
              (subseq output 0 *max-output-chars*) *max-output-chars*)
      output))

(defun results-status (results)
  (cond ((5am:results-status results) "pass")
        ((null results) "error")
        (t "fail")))

(defun generate-report (results &optional error)
  (st-json:jso "status" (results-status results)
               "message" (unless results (dissect:present error))
               "tests" (mapcar #'process-test-result results)))

(defun test-runner ()
  (destructuring-bind (slug src-path out-path) (uiop:command-line-arguments)
    (st-json:write-json-to-string
      (handler-case (generate-report (get-test-results slug src-path out-path))
        (error (c) (generate-report nil c))))))

;; (with-output-to-string (str)
;;   (format str "~A" (fiveam::name (fiveam::test-case (car (test-runner)))))

(princ (test-runner))
  
;(defvar *lst* (test-runner))
;(defvar *fst* (find-if #'fiveam::test-failure-p *lst*))

;(format t "~%~A: ~A~%" (fiveam::name (fiveam::test-case *fst*)) (fiveam::description (fiveam::test-case *fst*)))
;(format t "~A~%" (fiveam::reason *fst*))
;(5am:explain! (list *fst*))
;(princ (get-output *fst*))
;; (princ (mapcar #'st-json:write-json-to-string (mapcar #'process-test-result *lst*)))
;(print (dissect:present (5am::actual-condition *fst*) nil))

; ADD DESCRIPTIONS

; function to process one test-case at a time

; test-case class!

;(dissect:present (fiveam::actual-condition (car (test-runner))))

; process all tests, using explain, returning the jso

;; (defvar *results* (test-runner))

;; (format t "Results:~%~A~%~%~A" (mapcar #'fiveam::test-passed-p *results*) (fiveam:results-status *results*))
