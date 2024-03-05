;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'org-linker)

;;;###autoload
(defun org-linker/attach-file-with-uuid ()
  "Attach a file or directory with a unique UUID to the org-linker
root directory. This function prompts the user to select a file
or directory, generates a unique UUID,copies the selected item to
a new directory under the root directory, and inserts an org-mode
link. If the file size exceeds the limit set in
`org-linker/file-size-limit`, an error is raised."
  (interactive)
  (org-linker/ensure-root-directory)
  (let* ((uuid (org-linker/generate-unique-uuid))
         (new-dir (concat org-linker/root-directory uuid "/"))
         (file (read-file-name "Attach file or directory: "))
         (file-size (org-linker/get-path-size file))
         (new-path (concat new-dir (file-name-nondirectory file)))
         (link))
    (when (> file-size org-linker/file-size-limit)
      (org-linker/error "File size (%s kilobytes) is larger than the limit (%s kilobytes).
Customize org-linker/file-size before proceeding if necessary."
                        file-size org-linker/file-size-limit))
    (make-directory new-dir t)
    (if (file-directory-p file)
        (progn
          (copy-directory file new-path 'recursive)
          (setq new-path (concat new-path "/" (org-linker/get-folder-name file))))
      (copy-file file new-path))
    ;; Write transaction
    (org-linker/write-transaction uuid 'copy file file-size)
    ;; Handle org-link
    (setq link (concat "linker:" uuid "/" (file-name-nondirectory new-path)))
    (kill-new link)
    (org-insert-link nil link nil)))

;;;###autoload
(defun org-linker/trash-folder-uuid (&optional uuid)
  "Move a folder identified by its UUID under the org-linker root
directory to the system trash. This function lists all
directories under the root directory, prompts the user to select a
UUID, and then moves the corresponding folder to the system trash
using `org-linker/trashing-function`."
  (interactive)
  (org-linker/ensure-root-directory)
  (cl-flet ((list-all-directories (directory)
              "List all directories under a certain directory."
              (mapcar (lambda (x) (file-name-nondirectory (directory-file-name x)))
                      (seq-filter #'file-directory-p
                                  (mapcar #'file-name-as-directory
                                          (directory-files directory t "^[^.].*" t))))))
    (let* ((uuids (list-all-directories org-linker/root-directory))
           (uuid (or uuid (completing-read "Select a string: " uuids nil t)))
           (file-to-trash (file-name-concat org-linker/root-directory uuid))
           (confirmation (string= "yes" (read-minibuffer
                                         (format "Really trashing org-linker:%s? (yes/no) > " uuid)))))
      (if confirmation
          (progn
            (message "Trashing %s" file-to-trash)
            ;; Write transaction
            (org-linker/write-transaction uuid 'trash file-to-trash '\?)
            ;; Trash it!
            (funcall org-linker/trashing-function file-to-trash))
        (org-linker/error "Aborting..")))))

(provide 'org-linker-commands)
