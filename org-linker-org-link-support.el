;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Support org-linker to org-mode links.

(require 'org-linker)

(org-add-link-type "linker" 'org-linker/open)

(defun org-linker/open (path)
  "Open the file specified by PATH using org-linker."
  (let ((full-path (concat org-linker/root-directory path)))
    (find-file full-path)))

(defun org-linker/trash-folder-at-point ()
  "Trash the folder associated with the org-linker link at point.

This interactive function checks if the cursor is positioned at
an org-linker link. If not, it displays an error message using
`org-linker/error`. If the cursor is at an org-linker link, it
extracts the UUID from the path of the link and then calls
`org-linker/trash-folder-uuid` to move the folder associated with
that UUID to the trash.

This function is intended to be used interactively within an Org
Mode buffer to facilitate trashing folders linked via org-linker."
  (interactive)
  (unless (string= "linker" (org-element-property :type (org-element-context)))
    (org-linker/error "Cursor not at an org-linker link."))
  (let ((uuid (car (split-string (org-element-property :path (org-element-context)) "/"))))
    (org-linker/trash-folder-uuid uuid)))

(provide 'org-linker-org-link-support)
