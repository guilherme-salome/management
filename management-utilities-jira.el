;;; management-utilities-jira.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Guilherme Salome
;;
;; Author: Guilherme Salome <guilhermesalome@gmail.com>
;; Maintainer: Guilherme Salome <guilhermesalome@gmail.com>
;; Created: April 07, 2025
;; Modified: April 07, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/C304095/management-utilities-jira
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org)
(require 'json)
(require 'url)
(require 'management-utilities)

(defun management-jira-get (endpoint)
  "GET request to JIRA's API with ENDPOINT, returning parsed JSON response."
  (let* ((project-properties (org-entry-properties))
         (url (cdr (assoc "JIRA_URL" project-properties)))
         (domain (replace-regexp-in-string "^https?://" "" url))
         (email (cdr (assoc "JIRA_EMAIL" project-properties)))
         (token (auth-source-pick-first-password :host domain :user email :source "~/.authinfo"))
         (credentials (format "%s:%s" email token))
         (base64 (concat "Basic " (base64-encode-string credentials t)))
         (url-request-method "GET")
         (url-request-extra-headers
          (list (cons "Content-Type" "application/json")
                (cons "Accept" "application/json")
                (cons "Authorization" base64))))
    (with-current-buffer (url-retrieve-synchronously (concat url endpoint) t)
      (json-read))))

(defun management-jira-build-endpoint (epic-key &optional start-at max-results)
  "Construct a Jira API endpoint for searching issues with expanded fields.
EPIC-KEY is the Epic's key.
START-AT is the index to start retrieving results from (for pagination).
MAX-RESULTS is the number of results to retrieve per request."
  (let ((base-endpoint "/rest/api/2/search")
        (jql (format "\"Epic Link\"=%s" epic-key))
        (start (or start-at 0))
        (max (or max-results 50)))
    (format "%s?jql=%s&startAt=%d&maxResults=%d&expand=changelog&fields=summary,description,status,assignee,duedate,customfield_10121,customfield_10124"
            base-endpoint
            (url-encode-url jql)
            start
            max)))

(defun management-jira-get-tasks (epic-key)
  "Retrieve ALL tasks for a specific Epic, handling pagination.
EPIC-KEY is the key of the Epic to retrieve tasks for."
  (let ((all-issues '())
        (start-at 0)
        (max-results 50)
        (total-issues nil))
    (while (or (null total-issues) (< (length all-issues) total-issues))
      (let* ((endpoint (management-jira-build-endpoint epic-key start-at max-results))
             (response (management-jira-get endpoint))
             (total (alist-get 'total response))
             (issues (append (alist-get 'issues response) nil)))
        (setq total-issues total
              all-issues (append all-issues issues)
              start-at (+ start-at max-results))))
    all-issues))

(defun management-jira-parse-assignee (assignee)
  "Parse assignee information from ASSIGNEE alist.
Returns a plist with assignee details or nil if no assignee."
  (when assignee
    (list
     :name (cdr (assoc 'displayName assignee))
     :email (cdr (assoc 'emailAddress assignee))
     :account-id (cdr (assoc 'accountId assignee)))))

(defun management-jira-find-done-date (changelog)
  "Find the date when the issue was last set to 'Done' status.
CHANGELOG is the list of status changes from the Jira issue.
Returns the date of the first 'Done' status in YYYY-MM-DD format or nil."
  (catch 'done-date
    (dolist (item (append (alist-get 'histories changelog) nil))
      (dolist (item-history (append (alist-get 'items item) nil))
        (when (and (equal (alist-get 'field item-history) "status")
                   (equal (alist-get 'toString item-history) "Done"))
          (let* ((full-date (alist-get 'created item))
                 (date-match (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" full-date)))
            (when date-match
              (throw 'done-date (match-string 1 full-date)))))))))

(defun management-jira-extract-custom-field (fields field-id)
  "Extract a custom field value from FIELDS.
FIELD-ID is the Jira custom field identifier.
Returns the field value or nil if not found."
  (let ((custom-field (assoc (intern field-id) fields)))
    (when custom-field (cdr custom-field))))

(defun management-jira-parse-task (task)
  "Parse an Epic's task from the Jira API response with detailed fields.
TASKS is the task alist to parse."
  (let* ((fields (alist-get 'fields task)))
    (list
     :key (alist-get 'key task)
     :summary (alist-get 'summary fields)
     :status (alist-get 'name (alist-get 'status fields))
     :assignee (management-jira-parse-assignee (alist-get 'assignee fields))
     :due-date (alist-get 'duedate fields)
     :done-date (management-jira-find-done-date (alist-get 'changelog task))
     :description (alist-get 'description fields)
     :acceptance-criteria (management-jira-extract-custom-field fields "customfield_10121")
     :detailed-status-update (management-jira-extract-custom-field fields "customfield_1024"))))

(defun management-jira-escape-org-text (text)
  "Escape Org-specific characters in TEXT."
  (replace-regexp-in-string "^\\*+" "-" text))

(defun management-jira-create-tasks-outline (tasks epic-name)
  (dolist (task tasks)
    (let* ((status (plist-get task :status))
           (task-status (cond
                         ((string-equal status "Done") "DONE")
                         ((string-equal status "In Progress") "DOING")
                         (t "TODO")))
           (task-heading (format "%s [[https://your-jira-instance.com/browse/%s][%s]]"
                                 task-status
                                 (plist-get task :key)
                                 (plist-get task :summary)))
           (assignee (plist-get task :assignee))
           (due-date (plist-get task :due-date))
           (done-date (plist-get task :done-date))
           (description (plist-get task :description))
           (acceptance-criteria (plist-get task :acceptance-criteria))
           (detailed-status-update (plist-get task :detailed-status-update)))

      ;; Create outline-path
      (management-goto-or-create-org-outline-path (list "Planning" epic-name task-heading))

      ;; Add metadata first: assignee and deadline
      (when assignee
        (org-set-property "ASSIGNED_TO" (plist-get assignee :name))) ;; TODO Insert the link to the org-roam node if possible
      (when due-date
        (org-deadline nil due-date))
      (setq point-before-meta-data (point))
      (org-end-of-meta-data t) ;; force metadata placement and move cursor after it

      ;; Insert descriptive content below the metadata
      (when description
        (insert (format "Description: %s\n" (management-jira-escape-org-text description))))
      (when acceptance-criteria
        (insert (format "Acceptance Criteria:\n%s\n" (management-jira-escape-org-text acceptance-criteria))))
      (when detailed-status-update
        (insert (format "Detailed Status Update:\n%s\n" (management-jira-escape-org-text detailed-status-update))))

      ;; If the task is Done, go back and insert CLOSED timestamp right after the heading
      (when (and done-date (string-equal status "Done"))
        (goto-char point-before-meta-data)      ; Return to the heading
        (insert (format "\nCLOSED: %s" (format-time-string "<%Y-%m-%d %a>" (date-to-time done-date))))))))

(defun management-jira-update-planning ()
  "Gets all Epics and Tasks for a project, updating and replacing 'Planning'."
  (interactive)
  (let ((epics (management-jira-get-epics)))
    (dolist (epic epics)
        (management-jira-create-tasks-outline (plist-get epic :key) (plist-get epic :summary)))))

(defun management-jira-print-epic-tasks (epic-key)
  "Retrieve and print all tasks for an Epic in a detailed, readable format.
EPIC-KEY is the Jira Epic key."
  (interactive "sEnter Epic Key: ")
  (let ((tasks (management-jira-get-epic-tasks epic-key)))
    (with-current-buffer (get-buffer-create "*Jira Epic Tasks*")
      (erase-buffer)
      (insert (format "Tasks for Epic %s (Total: %d)\n\n" epic-key (length tasks)))
      (dolist (task tasks)
        (insert (format "Key: %s\n" (plist-get task :key)))
        (insert (format "Summary: %s\n" (plist-get task :summary)))
        (insert (format "Status: %s\n" (plist-get task :status)))

        (let ((assignee (plist-get task :assignee)))
          (when assignee
            (insert (format "Assignee: %s (Email: %s)\n"
                            (plist-get assignee :name)
                            (or (plist-get assignee :email) "N/A")))))

        (when (plist-get task :due-date)
          (insert (format "Due Date: %s\n" (plist-get task :due-date))))

        (when (plist-get task :done-date)
          (insert (format "Done Date: %s\n" (plist-get task :done-date))))

        (when (plist-get task :description)
          (insert (format "Description:\n%s\n" (plist-get task :description))))

        (when (plist-get task :acceptance-criteria)
          (insert (format "Acceptance Criteria:\n%s\n" (plist-get task :acceptance-criteria))))

        (when (plist-get task :detailed-status-update)
          (insert (format "Detailed Status Update:\n%s\n" (plist-get task :detailed-status-update))))

        (insert "\n---\n\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))




(provide 'management-utilities-jira)
;;; management-utilities-jira.el ends here
