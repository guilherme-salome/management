;;; management-templates.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Guilherme Salome
;;
;; Author: Guilherme Salome <guilhermesalome@gmail.com>
;; Maintainer: Guilherme Salome <guilhermesalome@gmail.com>
;; Created: April 01, 2025
;; Modified: April 01, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/C304095/management-templates
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defgroup management nil
  "Customization group for management."
  :group 'org)


(defcustom management-org-roam-templates
  '(("d" "default" plain "%?"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n"))
    ("p" "person" plain "%?"
     :if-new (file+head "people/${slug}.org"
                        ":PROPERTIES:\n:ROAM_TAGS: person\n:ALIAS: ${slug}\n:END:\n#+title: ${title}\n* Personal\n* Performance\n** Career Goals\n** %(format-time-string \"%Y\")\n*** Goals\n*** Reviews\n* Working On\n* Discussions\n** %(org-insert-time-stamp (current-time))\n* Progress")
     :unnarrowed t)
    ("j" "project" plain "%?"
     :if-new (file+head "projects/${slug}.org"
                        ":PROPERTIES:\n:ROAM_TAGS: project\n:CATEGORY: ${slug}\n:END:\n#+title: ${title}\n* Planning\n** Deliveries\n** Enhancements\n** Requests\n** Backlog\n* Discussions\n** %(org-insert-time-stamp (current-time))\n* Progress")
     :unnarrowed t))
  "Default org-roam capture templates provided by management."
  :type '(repeat (list string string function plist))
  :group 'management)

(defcustom management-org-capture-templates
  '(("t" "Templates for Tasks"
     ("td" "Task - Project Delivery" entry (file+headline buffer-file-name "Deliveries")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:ASSIGNED_TO: %^{Assignee|Unassigned}\n:DEADLINE: %^t\n:END:\nContext:\n\nAcceptance Criteria:")
     ("te" "Task - Project Enhancement" entry (file+headline buffer-file-name "Enhancements")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:ASSIGNED_TO: %^{Assignee|Unassigned}\n:END:\nContext:\n\nAcceptance Criteria:")
     ("tr" "Task - Project Request" entry (file+headline buffer-file-name "Requests")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:ASSIGNED_TO: %^{Assignee|Unassigned}\n:REQUESTED_BY: %^{Requested by}\n:END:\nQuestion:\n")
     ("tb" "Task - Project Backlog" entry (file+headline buffer-file-name "Backlog")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\nContext:\n")
     ("th" "Task - Here" entry (here) "* TODO %?"))
    ("d" "Discussion" entry (file+headline buffer-file-name "Discussions")
     "* <%(format-time-string \"%Y-%m-%d %<%a>\")>\n%?")
    ("p" "Templates for Progress Updates"
     ("pp" "Project Progress" entry (file+headline buffer-file-name "Progress")
      "* <%(format-time-string \"%Y-%m-%d %<%a>\")> %(management-select-task-from-projects-and-create-link)")
     ("pg" "General Progress" entry (file+headline buffer-file-name "Progress")
      "* <%(format-time-string \"%Y-%m-%d %<%a>\")> %?")))
  "Default org capture templates provided by management."
  :type '(repeat (choice (list string string (function :tag "Type") (sexp :tag "Target") string)
                         (list string string
                               (repeat (list string string (function :tag "Type") (sexp :tag "Target") string)))))
  :group 'management)

(defun management-setup-templates ()
  "Set up default template configurations for management."
  (interactive)
  (when (featurep 'org-roam)
    (setq org-roam-capture-templates management-package-org-roam-templates))
  (when (featurep 'org)
    (setq org-capture-templates management-package-org-capture-templates)))


(provide 'management-templates)
;;; management-templates.el ends here
