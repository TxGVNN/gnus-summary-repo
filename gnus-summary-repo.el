;;; gnus-summary-repo.el --- Import and export files between IMAP and local by using GNUS -*- lexical-binding: t -*-

;; Copyright (C) 2019 Giap Tran <txgvnn@gmail.com>

;; Author: Giap Tran <txgvnn@gmail.com>
;; URL: https://github.com/TxGVNN/gnus-sum-repo
;; Version: 0.1
;; Package-Requires: ((emacs "25"))

;;; Commentary:
;; You have to be at Summary mode (defined in ‘gnus-sum.el’) before
;; call the gnus-summary-repo-* function

;;; Code:

(require 'gnus-sum)

(declare-function gnus-summary-article-subject 'gnus-sum)

(defgroup gnus-summary-repo nil
  "GNUS repo configuration."
  :group 'gnus-summary)

(defcustom gnus-summary-repo-dir-local nil
  "Default local directory."
  :group 'gnus-summary-repo
  :type 'string)

(defcustom gnus-summary-repo-header-from nil
  "Default From header in mail."
  :group 'gnus-summary-repo
  :type 'string)

(defun gnus-summary-repo-import-directory (&optional directory)
  "Import files in a folder to Group Imap.
if DIRECTORY non-nil, export to DIRECTORY"
  (interactive)
  (if (not (equal major-mode 'gnus-summary-mode))
      (error "You have to go to Summary Gnus (Ex: INBOX on your mail))"))
  (if (not directory)
      (setq directory gnus-summary-repo-dir-local))
  (if (not directory)
      (setq directory (file-name-as-directory
                       (expand-file-name
                        (read-directory-name "Select a directory to import: ")))))
  (if (file-regular-p directory)
      (error "%s is not directory" directory))
  (dolist (file (directory-files-recursively directory ""))
    (unless (or (file-directory-p file) (string-match "^.*\/.git.*" file))
      (gnus-summary-repo-import-file file (concat (substring
                                                   (file-name-directory (expand-file-name file))
                                                   (length (file-name-as-directory (expand-file-name directory)))
                                                   nil)
                                                  (file-name-nondirectory file))))))

(defun gnus-summary-repo-export-directory (&optional directory)
  "Export files Group Imap to a folder.
if DIRECTORY non-nil, export to DIRECTORY"
  (interactive)
  (if (not (equal major-mode 'gnus-summary-mode))
      (error "You have to go to Summary Gnus (Ex: INBOX on your mail))"))
  (if (not directory)
      (setq directory gnus-summary-repo-dir-local))
  (if (not directory)
      (setq directory (file-name-as-directory
                       (expand-file-name
                        (read-directory-name "Select a directory to export: ")))))
  (if (file-regular-p directory)
      (error "%s is not directory" directory))
  (dolist (article gnus-newsgroup-limit)
    (gnus-summary-repo--export-file article directory)))

(defun gnus-summary-repo-export-file (n &optional directory)
  "Export the attachment in article to DIRECTORY.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil, export at."
  (interactive "p")
  (if (not (equal major-mode 'gnus-summary-mode))
      (error "You have to go to Summary Gnus (Ex: INBOX on your mail))"))
  (if (not directory)
      (setq directory gnus-summary-repo-dir-local))
  (if (not directory)
      (setq directory (file-name-as-directory
                       (expand-file-name
                        (read-directory-name "Select a directory to export: ")))))
  (if (file-regular-p directory)
      (error "%s is not directory" directory))
  (let* ((articles (gnus-summary-work-articles n)))
    (dolist (article articles)
      (gnus-summary-repo--export-file article directory))))

(defun gnus-summary-repo--export-file (article directory)
  "Export an attachment in an ARTICLE to a DIRECTORY."
  (if (not (equal major-mode 'gnus-summary-mode))
      (error "You have to go to Summary Gnus (Ex: INBOX on your mail))"))
  (let (dir-path fullpath subject)
    (setq subject (gnus-summary-article-subject article))
    (setq dir-path (file-name-directory (format "%s%s" (file-name-as-directory directory) subject )))
    (setq fullpath (format "%s%s" dir-path (file-name-nondirectory subject)))
    (mkdir dir-path t)
    (when (gnus-summary-repo--mail-newer-than-file article fullpath)
      (message "Export %s" subject)
      (save-excursion
        (gnus-summary-display-article article nil)
        (gnus-summary-select-article-buffer)
        (goto-char (point-min))
        (when (search-forward "\nattachment:" nil t)
          (widget-forward 1)
          (let ((data (get-text-property (point) 'gnus-data)))
            (when data
              (if (file-exists-p fullpath)
                  (delete-file fullpath))
              (mm-save-part-to-file data fullpath))))))))

(defun gnus-summary-repo-import-file (&optional file subject)
  "Import an arbitrary FILE with SUBJECT into a mail newsgroup."
  (interactive)
  (if (not (equal major-mode 'gnus-summary-mode))
      (error "You have to go to Summary Gnus (Ex: INBOX on your mail))"))
  (if (not file)
      (setq file (expand-file-name (read-file-name "Select a file to import: "))))
  (if (not subject)
      (setq subject (read-string "Select a file to import: " (file-name-nondirectory file))))
  (if (not (file-name-absolute-p file))
      (error "%s is not absolute path" file))
  (let ((group gnus-newsgroup-name)
        atts not-newer header-from)
    (setq not-newer t)
    (unless (gnus-check-backend-function 'request-accept-article group)
      (error "%s does not support article importing" group))
    (or (file-readable-p file)
        (not (file-regular-p file))
        (error "Can't read %s" file))

    ;; Delete the outdated files
    (let ((articles (gnus-summary-find-matching "subject" (format "^%s$" subject) 'all nil nil nil))
          (nnmail-expiry-target 'delete) not-deleted)
      (save-excursion
        (while articles
          (if (or (> (length articles) 1) (gnus-summary-repo--mail-newer-than-file (car articles) file t))
              (progn()
                    (setq not-deleted (gnus-request-expire-articles
                                       (make-list 1 (car articles)) gnus-newsgroup-name 'force))
                    (gnus-summary-remove-process-mark (car articles))
                    ;; The backend might not have been able to delete the article
                    ;; after all.
                    (unless (memq (car articles) not-deleted)
                      (gnus-summary-mark-article (car articles) gnus-canceled-mark)
                      (let* ((article (car articles))
                             (ghead (gnus-data-header
                                     (assoc article (gnus-data-list nil)))))
                        (run-hook-with-args 'gnus-summary-article-delete-hook
                                            'delete ghead gnus-newsgroup-name nil nil))))
            (setq not-newer nil))
          (setq articles (cdr articles)))))

    ;; Add new one
    (when not-newer
      (message "Import %s" file)
      (with-current-buffer (gnus-get-buffer-create " *import file*")
        (erase-buffer)
        (insert subject)
        (mml-insert-empty-tag 'part
                              'type "application/octet-stream"
                              'filename (substring-no-properties file)
                              'disposition "attachment"
                              'description subject)
        (goto-char (point-min))
        (if (nnheader-article-p)
            (save-restriction
              (goto-char (point-min))
              (search-forward "\n\n" nil t)
              (narrow-to-region (point-min) (1- (point)))
              (goto-char (point-min))
              (unless (re-search-forward "^date:" nil t)
                (goto-char (point-max))
                (setq atts (file-attributes file))
                (insert "Date: " (message-make-date (nth 5 atts)) "\n")))
          ;; This doesn't look like an article, so we fudge some headers.
          (setq atts (file-attributes file))
          (setq header-from gnus-summary-repo-header-from)
          (if (not header-from)
              (setq header-from (format-time-string "<%y-%m-%d@%H:%M>" (time-to-seconds))))
          (insert "From: " header-from "\n"
                  "Subject: " subject "\n"
                  "Date: " (message-make-date (nth 5 atts)) "\n\n"))
        (gnus-request-accept-article group nil t)
        (kill-buffer (current-buffer)))
      (setq gnus-newsgroup-active (gnus-activate-group group)))))

(defun gnus-summary-repo-sync-deleted-files-base-directory (&optional directory)
  "Delete the file on Group, when this file was deleted on local DIRECTORY."
  (interactive)
  (if (not (equal major-mode 'gnus-summary-mode))
      (error "You have to go to Summary Gnus (Ex: INBOX on your mail))"))
  (if (not directory)
      (setq directory gnus-summary-repo-dir-local))
  (if (not directory)
      (setq directory (file-name-as-directory
                       (expand-file-name
                        (read-directory-name "Select a directory to export: ")))))
  (if (file-regular-p directory)
      (error "%s is not directory" directory))
  (dolist (article gnus-newsgroup-limit)
    (message "Mark %s will be deleted" (gnus-summary-article-subject article))
    (unless (file-regular-p (expand-file-name (format "%s%s" (file-name-as-directory directory) (gnus-summary-article-subject article))))
      (gnus-summary-mark-article article gnus-canceled-mark))))


(defun gnus-summary-repo--mail-newer-than-file(article file &optional reverse)
  "Compare date of ARTICLE newer than FILE.
If REVERSE is non-nil, reverse the result."
  (if (file-exists-p file)
      (let (mail-modifation-time file-modifation-time)
        (save-excursion
          (gnus-summary-display-article article nil)
          (gnus-summary-select-article-buffer)
          (goto-char (point-min))
          (when (search-forward "\ndate:" nil t)
            (setq mail-modifation-time (truncate (time-to-seconds (mail-header-parse-date (nnheader-header-value)))))
            (setq file-modifation-time (truncate (time-to-seconds (file-attribute-modification-time (file-attributes file)))))
            (if reverse
                (< mail-modifation-time file-modifation-time)
              (> mail-modifation-time file-modifation-time)))))
    (if reverse nil t)))

(provide 'gnus-summary-repo)

;;; gnus-summary-repo.el ends here
