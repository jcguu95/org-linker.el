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
  "Support org-mode inline images toggling by redefining
#'org-display-inline-images."
  ;; TODO Save the old version for users to switch back just in case.
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
                                (push ov org-inline-image-overlays)))))))))))))))))

;; Redefine #'org-display-inline-images to support inline images.
;; (org-linker/support-org-inline-image)

(provide 'org-linker-org-link-support)
