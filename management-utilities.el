;;; management-utilities.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Guilherme Salome
;;
;; Author: Guilherme Salome <guilhermesalome@gmail.com>
;; Maintainer: Guilherme Salome <guilhermesalome@gmail.com>
;; Created: April 02, 2025
;; Modified: April 02, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/C304095/management-utilities
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
(require 'org-roam)
(require 'org-ql)

(defun management-skip-to-first-org-headline ()
  "Move point to the first Org headline, skipping property drawers."
  (goto-char (point-min))
  (while (and (not (looking-at "^\\*")) (not (eobp)))
    (forward-line 1)))

(defun management-goto-or-create-org-outline-path (outline-path)
  "Navigate to or create a nested Org outline based on outline-path, ensuring each level exists."
  (management-skip-to-first-org-headline)
  (let ((level 1))
    (dolist (x outline-path)
      (let* ((heading (concat (make-string level ?*) " " x))
             (matchon (format "^%s$" (regexp-quote heading)))
             (matched (re-search-forward matchon nil t)))
        (if matched nil (insert (format "\n%s" heading)))
        (org-narrow-to-subtree))
        (setq level (1+ level))))
    (widen))

(defun management-get-project-planning-tasks-linked-to-node-id (node-id)
  "Retrieve all tasks from projects under 'Planning' tree linked to the given Org-roam node ID."
  (org-ql-query
    :select '(list
              :todo (org-get-todo-state)
              :title (org-get-heading t t t t)
              :deadline (org-entry-get nil "DEADLINE" t)
              :outline (org-get-outline-path)
              :filepath (buffer-file-name)
              :project (org-roam-node-title (org-roam-node-at-point)))
    :from (directory-files-recursively (concat org-roam-directory "projects/") "\\.org$")
    :where `(and (or (todo) (done))
                 (ancestors "Planning")
                 (link ,(concat "id:" node-id)))))

(defun management-update-or-create-working-on-tree-with-project-tasks (tasks)
  "Update the 'Working On' section in Org-roam with tasks from multiple projects, organizing them under their respective project headings."
  ;; Create and clear the "Working On" tree
  (management-goto-or-create-org-outline-path (list "Working On"))
  (org-narrow-to-subtree)
  (forward-line 1)
  (delete-region (point) (point-max))
  ;; Insert each task from multiple projects linked to the person
  (dolist (task tasks)
    (let* ((heading (format "%s [[file:%s::%s][%s]]"
                           (or (plist-get task :todo) "TODO")  ;; Insert TODO state
                           (plist-get task :filepath)          ;; File path of project
                           (plist-get task :title)             ;; Exact heading as anchor
                           (plist-get task :title)))
           (outline-path (append (list "Working On" (plist-get task :project)) (cdr (plist-get task :outline)) (list heading))))
      (management-goto-or-create-org-outline-path outline-path)))
  (widen))

(defun management-select-task-from-projects-and-create-link ()
  "Select a TODO task from Org-roam project files, filtering tasks under 'Planning'."
  (interactive)
  (let* ((tasks (org-ql-query
                  :select '(list
                            :title (org-get-heading t t t t)
                            :outline (org-get-outline-path)
                            :filepath (buffer-file-name)
                            :project (org-roam-node-title (org-roam-node-at-point)))
                  :from (directory-files-recursively (concat org-roam-directory "projects/") "\\.org$")
                  :where `(and (todo) (ancestors "Planning"))))
         (task-sort (lambda (task) (concat
                                    (or (plist-get task :project) "")
                                    (mapconcat #'identity (or (plist-get task :outline) '()) " ")
                       (or (plist-get task :title) ""))))
         (sorted-tasks (sort (copy-sequence tasks) (lambda (a b) (string< (funcall task-sort a) (funcall task-sort b)))))
         (choices (mapcar
                   (lambda (task)
                     (let* ((filepath (plist-get task :filepath))
                            (project (plist-get task :project))
                            (outline (mapconcat #'identity (plist-get task :outline) " → "))
                            (title (plist-get task :title)))
                       (cons (format "%s - (%s) %s" project outline title)
                             (format "[[file:%s::%s][(%s) %s]]" filepath title project title))))
                   sorted-tasks))
         (selection (completing-read "Select a TODO: " choices nil t)))
    (cdr (assoc selection choices))))

(defun management-get-discussions (org-file)
  "Extract first-level subtrees under the 'Discussions' heading from ORG-FILE.
Returns an alist where each entry is (SUBTREE-TITLE . SUBTREE-CONTENTS). Filters
subtrees based on date in the headline, only including those from the last week."
  (with-temp-buffer
    (insert-file-contents org-file)
    (org-mode)
    (goto-char (point-min))
    (let ((discussions nil))
      (while (re-search-forward "^\\* \\(.*\\) \\(\\[\\([0-9]+-[0-9]+-[0-9]+\\)\\]\\)" nil t)
        (let* ((date-string (match-string 3))
               (date (date-to-time date-string))
               (current-time (current-time)))
          ;; Check if the date is within the last week
          (when (time-less-p date (time-subtract current-time (* 7 24 60 60)))
            ;; Extract the subtree title and contents
            (let ((title (match-string 1))
                  (subtree-start (match-beginning 0)))
              ;; Move to the end of the subtree
              (org-narrow-to-subtree)
              ;; Store the title and contents in the alist
              (push (cons title (buffer-substring-no-properties subtree-start (point))) discussions)
              ;; Widen to get back to the full buffer
              (widen)))))
      discussions)))

(provide 'management-utilities)
;;; management-utilities.el ends here
