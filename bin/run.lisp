;;;; Exercism Common Lisp Test Runner

;;; Setup Quicklisp & FiveAM
(load "/root/quicklisp/setup.lisp")
(ql:quickload '(:fiveam :st-json))

;;; Set some parameters
(defvar *max-output-chars* 500)
(defvar *results-file* "results.json")

;;; Automated message & expected value generation

;; I should break this up some
(defun get-test-sexprs (slug)
  (let ((src-file (format nil "~A-test.lisp" slug))
        (test-list '()))
    (with-open-file (fs src-file :direction :input)
      (handler-case (loop (push (read fs) test-list))
        (end-of-file ()
          (apply #'concatenate 'list
                 (mapcar (lambda (test-expr) (reverse (remove-if-not #'listp test-expr)))
                         (remove-if-not (lambda (expr) (eq 'test (car expr)))
                                        (reverse test-list)))))))))

(defun get-test-metadata (test-src)
  (let ((test-type (car test-src))
        (test-body (cdr test-src)))
    (test-components test-type test-body)))

(defmethod test-components ((type (eql 'is)) body)
  (destructuring-bind (expected expr) (cdar body)
    (st-json:jso "cmd" (print-expression expr)
                 "expected" expected)))

(defmethod test-components ((type (eql 'is-true)) body)
  (st-json:jso "cmd" (print-expression (car body))
               "expected" "T"))

(defmethod test-components ((type (eql 'is-false)) body)
  (st-json:jso "cmd" (print-expression (car body))
               "expected" "NIL"))

(defmethod test-components ((type (eql 'signals)) body)
  (st-json:jso "cmd" (print-expression (cadr body))
               "expected" (format nil "Signals: ~A" (car body))))

(defmethod test-components ((type (eql 'finishes)) body)
  (st-json:jso "cmd" (print-expression (car body))
               "expected" "Completes Execution"))

(defmethod test-components (type body)
  (st-json:jso "cmd" (write-to-string (cons type body) :pretty t)
               "expected" :null))

(defun print-expression (expr)
  (write-to-string (expand-test-expression expr) :pretty t))

(defun expand-test-expression (expr)
  (if (listp expr)
      (macroexpand (mapcar #'expand-test-expression expr))
      expr))

(defun merge-jso (a b)
  (st-json:mapjso (lambda (k v) (setf (st-json:getjso k a) v)) b)
  a)

;;; Load and run tests
(defun get-test-results (slug)
  (let ((test-slug (format nil "~A-test" slug)))
    (load test-slug)
    (do-symbols (sym (find-package (string-upcase test-slug)))
      (unless (find-symbol (string sym)) (import sym)))
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
               "message" (princ-to-string (5am::actual-condition result))
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
      (let ((expr (5am::test-expr test-result)))
        (cond ((and (listp expr) (not (listp (car expr)))) (eval expr))
              ((listp expr) (eval (car expr)))))))) ; signals and finishes don't behave here

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

(defun generate-report (results sexprs &optional error)
  (st-json:jso "status" (results-status results)
               "message" (unless results (princ-to-string error))
               "tests" (remove nil (mapcar #'merge-jso
                                           (mapcar #'process-test-result results)
                                           (mapcar #'get-test-metadata sexprs)))))

;;; Invoke the test-runner
(defun test-runner ()
  (destructuring-bind (slug src-path out-path) (uiop:command-line-arguments)
    (let ((results-file (merge-pathnames (truename out-path) *results-file*)))
      (with-open-file (fs results-file :direction :output :if-exists :supersede)
        (let ((*default-pathname-defaults* (truename src-path)))
          (st-json:write-json
           (handler-case (generate-report (get-test-results slug) (get-test-sexprs slug))
             (error (c) (generate-report nil nil c)))
           fs))))))

(test-runner)
