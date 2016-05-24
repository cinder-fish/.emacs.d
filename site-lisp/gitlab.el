;;; -*- lexical-binding: t -*-

;;; gitlab.el --- Some utility gitlab requests

(require 'request)
(require 'dash)
(require 'underlay)
(require 's)

(defvar gitlab-private-token "_WTx2PihCLgrRrkPouay"
  "The Gitlab user's private token to use to access the REST API")

(defun gitlab--make-request (route additional-keys method success-handler)
  (let ((api-call-url (s-concat "http://10.26.70.21/api/v3" route))
        (error-handler (function*
                        (lambda (&key error-thrown &allow-other-keys&rest _)
                          (message "Got error calling gitlab: %S" error-thrown)))))
    (apply 'request api-call-url
           :type method
           :headers `(("PRIVATE-TOKEN" . ,gitlab-private-token))
           :sync t
           :parser 'json-read
           :success success-handler
           :error error-handler
           additional-keys)))

(defun gitlab--make-GET-request (route params success-handler)
  (gitlab--make-request route `(:params ,params) "GET" success-handler))

(defun gitlab--make-POST-request (route params success-handler)
  (gitlab--make-request route `(:data ,params) "POST" success-handler))

(defun gitlab--add-user-to-buffer (user buffer)
  (let ((display-str (format "%S, %S\n"
                             (cdr (assq 'username user))
                             (cdr (assq 'id user))))
        (details-str (pp-to-string (reverse user)))
        details-overlay
        beg)
    (with-current-buffer buffer
      (insert display-str)
      (underlay-write-overlay details-str '((invisible . t)
                                           (background-color . "black"))))))


;; Users

(defun get-gitlab-response-buffer ()
  (let ((buffer (get-buffer-create "*Gitlab Response*")))
    (with-current-buffer buffer
      (underlay-view-mode))
    buffer))

(defun gitlab-list-all-users ()
  (interactive)
  (let* ((output-buffer (get-gitlab-response-buffer))
         (success-handler (function*
                           (lambda (&key data &allow-other-keys)
                             (when data
                               (with-current-buffer output-buffer
                                 (erase-buffer)
                                 (cl-loop for user across data
                                          do (gitlab--add-user-to-buffer user output-buffer))))))))
    (gitlab--make-GET-request "/users" '(("per_page" . "1000")) success-handler)
    (switch-to-buffer-other-window output-buffer)))
