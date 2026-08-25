;;; elpaish-keyring.el --- GPG Keyring and trust anchors for ELPAish package archives -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maint, tools, package, security
;; URL: https://github.com/tychoish/elpaish

;;; Commentary:
;; Client-side helper module for establishing ELPAish archive trust anchors
;; and configuring package archives.

;;; Code:

(require 'package)
(require 'subr-x)

(defgroup elpaish-keyring nil
  "GPG Keyring and trust anchors for ELPAish package archives."
  :group 'package
  :prefix "elpaish-keyring-")

(defvaralias 'elpaish-base-url 'elpaish-keyring-base-url)

(defcustom elpaish-keyring-base-url "https://tychoish.github.io/elpaish"
  "Base URL for the ELPAish package repository site."
  :type 'string
  :group 'elpaish-keyring)

(defun elpaish-keyring--canonical-stream (stream)
  "Return canonical stream symbol for STREAM."
  (pcase stream
    ((or 'snapshot 'dev 'default 'tip 'main 'elpaish) 'snapshot)
    ((or 'stable 'release 'tag 'semver 'elpaish-stable) 'stable)
    ((or 'staging 'prerelease 'rc 'beta 'elpaish-staging) 'staging)
    (_ 'snapshot)))

;;;###autoload
(defun elpaish-keyring-setup (&optional stream base-url)
  "Configure `package-archives' to include ELPAish repository for STREAM.
STREAM defaults to \\='snapshot.  BASE-URL defaults to `elpaish-keyring-base-url'."
  (interactive)
  (unless (bound-and-true-p package-archive-contents)
    (package-initialize))
  (let* ((selected-stream (or stream 'snapshot))
         (canonical (elpaish-keyring--canonical-stream selected-stream))
         (stream-name (symbol-name canonical))
         (root-url (string-remove-suffix "/" (or base-url elpaish-keyring-base-url)))
         (archive-url (format "%s/%s/" root-url stream-name))
         (existing (assoc stream-name package-archives)))
    (if existing
        (setcdr existing archive-url)
      (add-to-list 'package-archives (cons stream-name archive-url) t))
    (message "Added %s (%s) to `package-archives'." stream-name archive-url)))

(provide 'elpaish-keyring)

;; Local Variables:
;; package-lint-main-file: "pkg/elpaish.el"
;; End:
;;; elpaish-keyring.el ends here
