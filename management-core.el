;;; management-core.el --- Core functions for management -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Guilherme Salome
;;
;; Author: Guilherme Salome <guilhermesalome@gmail.com>
;; Maintainer: Guilherme Salome <guilhermesalome@gmail.com>
;; Created: April 01, 2025
;; Modified: April 01, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/C304095/management-core
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'management-custom)
(require 'org-roam nil t)  ;; Load org-roam if available, no error if not

(defun management-setup-defaults ()
  "Set up default configurations for management."
  (interactive)
  (unless (featurep 'org-roam)
    (error "management requires org-roam, which is not loaded. Please install and load it."))
  (when management-org-roam-directory
    (setq org-roam-directory management-org-roam-directory))
  (setq org-roam-capture-templates management-org-roam-templates
        org-capture-templates management-org-capture-templates))

(defun management-people-update-working-on ()
  "Update or create the 'Working On' tree for a person's Org-roam node based on tasks assigned to them across any projects."
  (interactive)
  (when (member "person" (org-property-values "ROAM_TAGS"))
    (let* ((node (org-roam-node-at-point))
           (node-id (org-roam-node-id node))
           (person-name (org-roam-node-title node)))
      (management-update-or-create-working-on-tree-with-project-tasks (management-get-project-planning-tasks-linked-to-node-id node-id))
      (message (concat "Updated tasks for: " person-name)))))

(provide 'management-core)
;;; management-core.el ends here
