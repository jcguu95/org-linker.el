;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Support org-linker to org-mode links.

(require 'org-linker)

(org-add-link-type "linker" #'org-linker/open)

(defun org-linker/ensure-uuid (uuid)
  "Check whether the UUID folder exists."
  (message uuid)
  (unless (file-directory-p (file-name-concat org-linker/root-directory uuid))
    (org-linker/error "Folder UUID:%s does not exist." uuid)))

(defun org-linker/expand (org-link-path)
  "Expand the org-mode link path to a file path."
  (file-name-concat org-linker/root-directory org-link-path))

(defun org-linker/open (org-link-path)
  "Open the file specified by PATH using org-linker."
  (let ((uuid (car (split-string org-link-path "/")))
        (path (org-linker/expand org-link-path)))
    (org-linker/ensure-uuid uuid)
    (find-file path)))

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
    (org-linker/ensure-uuid uuid)
    (org-linker/trash-folder-uuid uuid)))

(defun org-linker/support-org-inline-image ()
  ;; TODO Save the old version for users to switch back just in case.
  "Support org-mode inline images toggling by redefining
1. #'org-display-inline-images
2. #'+org/dwim-at-point
"
  (message "org-linker: warning: Redefining #'org-display-inline-images.")
  (defun org-display-inline-images (&optional include-linked refresh beg end)
    "
---------------------------------------------------------------
|    WARN: This function is redefined by `org-linker.el`.     |
|   The following is the content of the original docstring.   |
---------------------------------------------------------------

Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
    (interactive "P")
    ;; WARN org-linker redefinitions here.
    (message "Warning: #'org-display-inline-images has been redefined by org-linker.el.")
    (when (display-graphic-p)
      (when refresh
        (org-remove-inline-images beg end)
        (when (fboundp 'clear-image-cache) (clear-image-cache)))
      (let ((end (or end (point-max))))
        (org-with-point-at (or beg (point-min))
          (let* ((case-fold-search t)
                 (file-extension-re (image-file-name-regexp))
                 (link-abbrevs (mapcar #'car
                                       (append org-link-abbrev-alist-local
                                               org-link-abbrev-alist)))
                 ;; Check absolute, relative file names and explicit
                 ;; "file:" links.  Also check link abbreviations since
                 ;; some might expand to "file" links.
                 (file-types-re
                  (format "\\[\\[\\(?:file%s:\\|attachment:\\|linker:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
                          (if (not link-abbrevs) ""
                            (concat "\\|" (regexp-opt link-abbrevs))))))
            (while (re-search-forward file-types-re end t)
              (let* ((link (org-element-lineage
                            (save-match-data (org-element-context))
                            '(link) t))
                     (linktype (org-element-property :type link))
                     (inner-start (match-beginning 1))
                     (path
                      (cond
                       ;; No link at point; no inline image.
                       ((not link) nil)
                       ;; File link without a description.  Also handle
                       ;; INCLUDE-LINKED here since it should have
                       ;; precedence over the next case.  I.e., if link
                       ;; contains filenames in both the path and the
                       ;; description, prioritize the path only when
                       ;; INCLUDE-LINKED is non-nil.
                       ((or (not (org-element-property :contents-begin link))
                            include-linked)
                        (and (or (equal "file" linktype)
                                 ;; WARN org-linker redefinitions here.
                                 (equal "linker" linktype)
                                 (equal "attachment" linktype))
                             (org-element-property :path link)))
                       ;; Link with a description.  Check if description
                       ;; is a filename.  Even if Org doesn't have syntax
                       ;; for those -- clickable image -- constructs, fake
                       ;; them, as in `org-export-insert-image-links'.
                       ((not inner-start) nil)
                       (t
                        (org-with-point-at inner-start
                          (and (looking-at
                                (if (char-equal ?< (char-after inner-start))
                                    org-link-angle-re
                                  org-link-plain-re))
                               ;; File name must fill the whole
                               ;; description.
                               (= (org-element-property :contents-end link)
                                  (match-end 0))
                               (match-string 2)))))))
                (when (and path (string-match-p file-extension-re path))
                  (let ((file
                         (cond ((equal "attachment" linktype)
                                (progn (require 'org-attach)
                                       (ignore-errors (org-attach-expand path))))
                               ;; WARN org-linker redefinitions here.
                               ((equal "linker" linktype)
                                (progn (require 'org-linker)
                                       (ignore-errors (org-linker/expand path))))
                               ((equal "file" linktype)
                                (expand-file-name path)))))
                    (message file)
                    (when (and file (file-exists-p file))
                      (let ((width (org-display-inline-image--width link))
                            (old (get-char-property-and-overlay
                                  (org-element-property :begin link)
                                  'org-image-overlay)))
                        (if (and (car-safe old) refresh)
                            (image-flush (overlay-get (cdr old) 'display))
                          (let ((image (org--create-inline-image file width)))
                            (when image
                              (let ((ov (make-overlay
                                         (org-element-property :begin link)
                                         (progn
                                           (goto-char
                                            (org-element-property :end link))
                                           (skip-chars-backward " \t")
                                           (point)))))
                                ;; FIXME: See bug#59902.  We cannot rely
                                ;; on Emacs to update image if the file
                                ;; has changed.
                                (image-flush image)
                                (overlay-put ov 'display image)
                                (overlay-put ov 'face 'default)
                                (overlay-put ov 'org-image-overlay t)
                                (overlay-put
                                 ov 'modification-hooks
                                 (list 'org-display-inline-remove-overlay))
                                (when (boundp 'image-map)
                                  (overlay-put ov 'keymap image-map))
                                (push ov org-inline-image-overlays))))))))))))))))
  (message "org-linker: warning: Redefining #'+org/dwim-at-point.")
  (defun +org/dwim-at-point (&optional arg)
    "
---------------------------------------------------------------
|    WARN: This function is redefined by `org-linker.el`.     |
|   The following is the content of the original docstring.   |
---------------------------------------------------------------

Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
    (interactive "P")
    ;; WARN org-linker redefinitions here.
    (message "Warning: #'+org/dwim-at-point has been redefined by org-linker.el.")
    (if (button-at (point))
        (call-interactively #'push-button)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        ;; skip over unimportant contexts
        (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
          (setq context (org-element-property :parent context)
                type (org-element-type context)))
        (pcase type
          ((or `citation `citation-reference)
           (org-cite-follow context arg))

          (`headline
           (cond ((memq (bound-and-true-p org-goto-map)
                        (current-active-maps))
                  (org-goto-ret))
                 ((and (fboundp 'toc-org-insert-toc)
                       (member "TOC" (org-get-tags)))
                  (toc-org-insert-toc)
                  (message "Updating table of contents"))
                 ((string= "ARCHIVE" (car-safe (org-get-tags)))
                  (org-force-cycle-archived))
                 ((or (org-element-property :todo-type context)
                      (org-element-property :scheduled context))
                  (org-todo
                   (if (eq (org-element-property :todo-type context) 'done)
                       (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                           'todo)
                     'done))))
           ;; Update any metadata or inline previews in this subtree
           (org-update-checkbox-count)
           (org-update-parent-todo-statistics)
           (when (and (fboundp 'toc-org-insert-toc)
                      (member "TOC" (org-get-tags)))
             (toc-org-insert-toc)
             (message "Updating table of contents"))
           (let* ((beg (if (org-before-first-heading-p)
                           (line-beginning-position)
                         (save-excursion (org-back-to-heading) (point))))
                  (end (if (org-before-first-heading-p)
                           (line-end-position)
                         (save-excursion (org-end-of-subtree) (point))))
                  (overlays (ignore-errors (overlays-in beg end)))
                  (latex-overlays
                   (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                               overlays))
                  (image-overlays
                   (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                               overlays)))
             (+org--toggle-inline-images-in-subtree beg end)
             (if (or image-overlays latex-overlays)
                 (org-clear-latex-preview beg end)
               (org--latex-preview-region beg end))))

          (`clock (org-clock-update-time-maybe))

          (`footnote-reference
           (org-footnote-goto-definition (org-element-property :label context)))

          (`footnote-definition
           (org-footnote-goto-previous-reference (org-element-property :label context)))

          ((or `planning `timestamp)
           (org-follow-timestamp-link))

          ((or `table `table-row)
           (if (org-at-TBLFM-p)
               (org-table-calc-current-TBLFM)
             (ignore-errors
               (save-excursion
                 (goto-char (org-element-property :contents-begin context))
                 (org-call-with-arg 'org-table-recalculate (or arg t))))))

          (`table-cell
           (org-table-blank-field)
           (org-table-recalculate arg)
           (when (and (string-empty-p (string-trim (org-table-get-field)))
                      (bound-and-true-p evil-local-mode))
             (evil-change-state 'insert)))

          (`babel-call
           (org-babel-lob-execute-maybe))

          (`statistics-cookie
           (save-excursion (org-update-statistics-cookies arg)))

          ((or `src-block `inline-src-block)
           (org-babel-execute-src-block arg))

          ((or `latex-fragment `latex-environment)
           (org-latex-preview arg))

          (`link
           (let* ((lineage (org-element-lineage context '(link) t))
                  (path (org-element-property :path lineage))
                  ;; WARN org-linker redefinitions here.
                  (link-type (org-element-property :type lineage)))
             (if (or (equal (org-element-property :type lineage) "img")
                     ;; WARN org-linker redefinitions here.
                     (and (not (string= "linker" link-type))
                          path (image-type-from-file-name path)))
                 (+org--toggle-inline-images-in-subtree
                  (org-element-property :begin lineage)
                  (org-element-property :end lineage))

               (org-open-at-point arg))))

          ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
           (org-toggle-checkbox))

          (`paragraph
           (+org--toggle-inline-images-in-subtree))

          (_
           (if (or (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil  t)
                   (org-in-regexp org-link-any-re nil t))
               (call-interactively #'org-open-at-point)
             (+org--toggle-inline-images-in-subtree
              (org-element-property :begin context)
              (org-element-property :end context)))))))))

;; Redefine #'org-display-inline-images to support inline images.
;; (org-linker/support-org-inline-image)

(provide 'org-linker-org-link-support)
