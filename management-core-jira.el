;;; management-core-jira.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Guilherme Salome
;;
;; Author: Guilherme Salome <guilhermesalome@gmail.com>
;; Maintainer: Guilherme Salome <guilhermesalome@gmail.com>
;; Created: April 07, 2025
;; Modified: April 07, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/C304095/management-core-jira
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
(require 'management-utilities-jira)

(defun management-jira-set-properties-in-project ()
  "Interactively set JIRA properties in current Org-roam node."
  (interactive)
  (if (member "person" (org-property-values "ROAM_TAGS"))
      (let* ((url (read-string "Enter JIRA URL (e.g., https://lilly-jira.atlassian.net): "))
             (domain (replace-regexp-in-string "^https?://" "" url))
             (email (read-string "Enter JIRA Email: "))
             (board-id (read-number "Enter JIRA Board ID: ")))
        ;; Insert properties in the current Org-roam node
        (org-with-point-at (point-min)
          (org-set-property "JIRA_URL" url)
          (org-set-property "JIRA_EMAIL" email)
          (org-set-property "JIRA_BOARD_ID" (number-to-string board-id)))
        (message "To set the API token, add it to ~/.authinfo following this format:\n\
machine %s login %s password your_api_token" domain email))
    (error "Not in a project. Please ensure the property ROAM_TAGS is set to 'project'")))

(defun management-jira-get-epics (&optional board-id)
  "Fetch all epics from a JIRA board.

If BOARD-ID is provided, it is used to fetch the epics. Otherwise,
the function attempts to obtain the board ID from the current
Org entry's properties by looking for the \"JIRA_BOARD_ID\" property.

Arguments:
  BOARD-ID: An optional board identifier for the JIRA board."
  (interactive)
  (let* ((board-id (or board-id (cdr (assoc "JIRA_BOARD_ID" (org-entry-properties)))))
         (endpoint (format "/rest/agile/1.0/board/%s/issue?jql=issuetype=Epic" board-id))
         (response (management-jira-get endpoint)))
    (message "Obtained Epics for board %s: %s" board-id response)
    (mapcar (lambda (epic)
              (let ((key (alist-get 'key epic))
                    (summary (alist-get 'summary (alist-get 'fields epic))))
                (cons key summary)))
            (alist-get 'issues response))))

(defun management-jira-get-epic-tasks (epic-id)
  (interactive)
  (let ((tasks (management-jira-get-tasks epic-id)))
    (mapcar #'management-jira-parse-task tasks)))

(defun management-jira-get-epics-tasks (&optional board-id)
  "Fetch all epics and their tasks from a JIRA board."
  (interactive)
  (let* ((board-id (or board-id (cdr (assoc "JIRA_BOARD_ID" (org-entry-properties)))))
         (epics (management-jira-get-epics board-id))
         (epics-tasks '()))
    (dolist (epic epics)
      (let* ((tasks (management-jira-get-epic-tasks (car epic)))
             (epic-tasks (cons (cdr epic) tasks)))
        (push epic-tasks epics-tasks)))
    epics-tasks))

(defun management-jira-create-outline (epics-tasks &optional buffer jira-url)
  (interactive)
  (let* ((buffer-name (or buffer (current-buffer)))
         (buffer-results (get-buffer-create buffer-name)))
    (with-current-buffer buffer-results
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (goto-char (point-min))
      (let* ((project-properties (org-entry-properties))
             (url (cdr (assoc "JIRA_URL" project-properties)))
             (url (or jira-url (or url "https://your-jira-instance.com"))))
        (dolist (epic-tasks epics-tasks)
          (let* ((epic-name (car epic-tasks))
                 (tasks (cdr epic-tasks)))
            (dolist (task tasks)
              (let* ((status (plist-get task :status))
                     (task-status (cond
                                   ((string-equal status "Done") "DONE")
                                   ((string-equal status "In Progress") "DOING")
                                   (t "TODO")))
                     (task-heading (format "%s [[%s/browse/%s][%s]]"
                                           task-status
                                           url
                                           (plist-get task :key)
                                           (plist-get task :summary)))
                     (assignee (plist-get task :assignee))
                     (due-date (plist-get task :due-date))
                     (done-date (plist-get task :done-date))
                     (description (plist-get task :description))
                     (acceptance-criteria (plist-get task :acceptance-criteria))
                     (detailed-status-update (plist-get task :detailed-status-update)))
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
                  (insert (format "\nCLOSED: %s" (format-time-string "<%Y-%m-%d %a>" (date-to-time done-date)))))))))
        (display-buffer (current-buffer))))))

(provide 'management-core-jira)
;;; management-core-jira.el ends here
