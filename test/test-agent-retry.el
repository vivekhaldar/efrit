;;; test-agent-retry.el --- Tests for efrit-agent HTTP retry/backoff -*- lexical-binding: t; -*-

;; Author: Efrit Tests

(require 'ert)
(require 'cl-lib)

(load-file (expand-file-name "../lisp/efrit-agent.el" (file-name-directory (or load-file-name buffer-file-name))))

(defvar test-agent--calls 0)
(defvar test-agent--script nil)

(defun test-agent--make-response-buffer (status headers body)
  "Create a temp buffer simulating an HTTP response."
  (let ((buf (generate-new-buffer "*test-agent-http*")))
    (with-current-buffer buf
      (insert (format "HTTP/1.1 %d\r\n" status))
      (dolist (h headers)
        (insert (format "%s: %s\r\n" (car h) (cdr h))))
      (insert "\r\n")
      (insert body))
    buf))

(defun test-agent--mock-url-retrieve (_url callback &rest _args)
  "Mock url-retrieve to feed scripted responses and call CALLBACK."
  (setq test-agent--calls (1+ test-agent--calls))
  (let* ((step (pop test-agent--script))
         (status (plist-get step :status))
         (headers (plist-get step :headers))
         (body (plist-get step :body))
         (buf (test-agent--make-response-buffer status headers body)))
    (with-current-buffer buf
      (funcall callback nil))
    buf))

(defun test-agent--immediate-timer (_secs _repeat fn &rest args)
  "Mock run-at-time to call FN immediately."
  (apply fn args)
  ;; Return a fake timer object
  (list :timer 'immediate))

(ert-deftest efrit-agent-retry-529-then-success ()
  "Agent should retry on 529 with x-should-retry and then succeed."
  (let* ((test-agent--calls 0)
         (test-agent--script (list
                              ;; First response: 529 with retry header
                              (list :status 529
                                    :headers '(("Content-Type" . "application/json")
                                               ("x-should-retry" . "true")
                                               ("request-id" . "req_test_1"))
                                    :body "{\"error\":{\"type\":\"overloaded_error\",\"message\":\"please retry\"}}")
                              ;; Second response: 200 with content signaling completion
                              (list :status 200
                                    :headers '(("Content-Type" . "application/json")
                                               ("request-id" . "req_test_2"))
                                    :body "{\"content\":[{\"type\":\"text\",\"text\":\"{\\\"status\\\":\\\"complete\\\",\\\"rationale\\\":\\\"ok\\\"}\"}]}")))
         ;; Monkey patches
         (orig-url-retrieve (symbol-function 'url-retrieve))
         (orig-run-at-time (symbol-function 'run-at-time))
         (orig-get-key (symbol-function 'efrit-agent--get-api-key))
         (orig-message (symbol-function 'message))
         (messages '())
         (session nil))
    (unwind-protect
        (progn
          (fset 'url-retrieve #'test-agent--mock-url-retrieve)
          (fset 'run-at-time #'test-agent--immediate-timer)
          (fset 'efrit-agent--get-api-key (lambda () "test-key"))
          ;; capture message output to keep tests quiet
          (fset 'message (lambda (fmt &rest args)
                           (push (apply 'format fmt args) messages)))
          (setq efrit-agent-max-retries 3)
          (setq efrit-agent-retry-initial-delay 0.01)
          (setq session (efrit-agent-solve "test-goal"))
          ;; After the scripted two responses, we expect exactly 2 calls
          (should (= test-agent--calls 2)))
      (fset 'url-retrieve orig-url-retrieve)
      (fset 'run-at-time orig-run-at-time)
      (fset 'efrit-agent--get-api-key orig-get-key)
      (fset 'message orig-message))))

(provide 'test-agent-retry)

