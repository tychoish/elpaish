;;; bootstrap-elpaish.el --- Bootstrap package environment for ELPAish CI -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, ci

;;; Commentary:
;; Initializes package archives, package-user-dir, and core tooling dependencies
;; required for running ELPAish builds and preflight checks in CI environments.
;; Repository package dependencies are derived and installed implicitly during processing.

;;; Code:

(require 'package)

(setq package-user-dir (expand-file-name "elpa-ci/" default-directory))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (bound-and-true-p package-archive-contents)
  (package-refresh-contents))

(defconst elpaish-bootstrap-deps
  '(web-server htmlize annotated-completing-read transient package-lint)
  "Prerequisite packages needed for ELPAish itself to load and run preflight checks.")

(dolist (pkg elpaish-bootstrap-deps)
  (unless (package-installed-p pkg)
    (condition-case err
        (package-install pkg)
      (error (message "Warning: %s installation skipped or failed: %s" pkg (error-message-string err))))))

(provide 'bootstrap-elpaish)
;;; bootstrap-elpaish.el ends here
