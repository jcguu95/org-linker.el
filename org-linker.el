;;; -*- coding: utf-8; lexical-binding: t; -*-
;;;
;;;            --- org-linker.el ---
;;;   A tool designed to streamline the process of
;;;   attaching files to your org-mode documents.

;; Copyright © 2023-2024 Jin-Cheng Guu <jcguu95@gmail.com>

;; Author: Jin-Cheng Guu <jcguu95@gmail.com>
;; URL: https://github.com/jcguu95/org-linker
;; Keywords: org-mode, org-link, convenience
;; Version: 1.0.0
;; Package-Requires: ((org "9.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code
(require 'org)

(defgroup org-linker nil
  "Customization options for the org-linker package."
  :group 'org)

(defcustom org-linker/root-directory
  "/tmp/org-linker/"
  "The root directory where attachments are stored in org-linker."
  :type 'string
  :group 'org-linker)

(defcustom org-linker/transaction-file-name
  "db.tx"
  "The file name that holds the transaction history."
  :type 'string
  :group 'org-linker)

(defcustom org-linker/file-size-limit
  50000
  "The maximum file or directory size to be copied (in kilobytes)."
  :type 'string
  :group 'org-linker)

(defcustom org-linker/uuid-generator-function
  (lambda ()
    (shell-command-to-string "uuidgen | cut -d'-' -f1-2 | tr -d '\n'"))
  "The function responsible for generating a UUID. Users are
 encouraged to customize this function according to their needs.
 In practice, this function is invoked by
 #'org-linker/generate-unique-uuid to ensure the generation of
 unique UUIDs without conflicts."
  :type 'function
  :group 'org-linker)

(defcustom org-linker/trashing-function
  (lambda (file)
    "Move FILE to the system trash using the `trash-put` command line app."
    (call-process "trash-put" nil 0 nil file))
  "The function responsible for trashing a path. It is expected to
take one parameter."
  :type 'function
  :group 'org-linker)

(defun org-linker/write-data-to-file (file-path data)
  "Write Elisp-readable data to a file."
  (with-temp-buffer
    (goto-char (point-max))
    (insert "\n")
    (prin1 data (current-buffer))
    (write-region (point-min) (point-max) file-path t 'silent)))

(defun org-linker/write-transaction (uuid act file file-size)
  "Write a transaction record to the transaction file. This function
generates a timestamp, constructs a data structure with the
provided UUID, action, file path, and file size, and writes this
data to the transaction file using
`org-linker/write-data-to-file`."
  (let* ((time (format-time-string "%Y-%m-%d %H:%M:%S"))
         (data `(:time ,time :uuid ,uuid :act ,act :file ,file :file-size ,file-size)))
    (org-linker/write-data-to-file (org-linker/transaction-file) data)))

(defun org-linker/make-root-directory ()
  (message "Make directory: %s" org-linker/root-directory)
  (make-directory org-linker/root-directory t))

(defun org-linker/ensure-root-directory ()
  "Ensure that the root directory for org-linker exists; prompt to
create if not. If the root directory specified in
`org-linker/root-directory`does not exist, this function prompts
the user to create it by displaying a confirmation message and
handling user input accordingly."
  (unless (file-directory-p org-linker/root-directory)
    (let ((response
           (read-minibuffer
            (format "org-linker: root directory does not exist at %s; create (yes/no)? "
                    org-linker/root-directory))))
      (if (string= response "yes")
          (org-linker/make-root-directory)
        (org-linker/error "No root directory. Aborting..")))))

(defun org-linker/uuid-conflict? (uuid)
  "Check if a UUID conflicts with an existing directory under the
root directory. This function determines if a directory with the
provided UUID already exists under the root directory specified
in `org-linker/root-directory`."
  (file-directory-p (file-name-concat org-linker/root-directory uuid)))

(defun org-linker/transaction-file ()
  "Get the path to the transaction file. This function constructs
and returns the full path to the transaction file based on
`org-linker/root-directory` and
`org-linker/transaction-file-name`."
  (file-name-concat org-linker/root-directory
                    org-linker/transaction-file-name))

(defun org-linker/error (&rest args)
  "Signal an error with an `org-linker` prefix. This function
formats an error message with an `org-linker` prefix and raises
an error using `error` with the formatted message."
  (setf (car args) (concat "org-linker/error: " (car args)))
  (apply #'error (car args) (cdr args)))

(cl-defun org-linker/generate-unique-uuid (&optional (n-times-left 10))
  "Generate UUID. Regenerate in case of conflict."
  (when (> 0 n-times-left)
    (org-linker/error "Fail to find a new uuid without conflict."))
  (let ((uuid (funcall org-linker/uuid-generator-function)))
    (message "org-linker: Trying UUID \"%s\"" uuid)
    (if (org-linker/uuid-conflict? uuid)
        (org-linker/generate-unique-uuid (1- n-times-left))
      uuid)))

(defun org-linker/get-file-size (file-path)
  "Get the total size of a file in kilobytes."
  (let* ((file-attr (file-attributes file-path))
         (file-size (nth 7 file-attr)))
    (if file-size
        (read (format "%.2f" (/ file-size 1024.0)))
      (org-linker/error "File not found or inaccessible."))))

(defun org-linker/get-folder-size (directory)
  "Get the total size of a directory and its contents recursively in
kilobytes. If the probe times out, return a ridiculously large number."
  (let ((output (shell-command-to-string (format "timeout 1s du -sk %s | cut -f1" directory))))
    (if (string= output "")
        (progn
          (warn "Very likely that size probing of %s timed out as it is too large.")
          (warn "Automatically assign a very large number.")
          (* 1000000 org-linker/file-size-limit))
        (string-to-number (replace-regexp-in-string "\n" "" output)))))

(defun org-linker/get-path-size (path)
  "Get the total size of the file or the directory at path in kilobytes."
  (if (file-directory-p path)
      (org-linker/get-folder-size path)
    (org-linker/get-file-size path)))

(defun org-linker/get-folder-name (path)
  "Get the name of the folder from a file path."
  (file-name-nondirectory (directory-file-name path)))

(provide 'org-linker)

(cl-eval-when (load eval)
  (require 'org-linker-commands)
  (require 'org-linker-org-link-support))
