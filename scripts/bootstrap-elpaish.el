;;; bootstrap-elpaish.el --- Bootstrap dependencies for ELPAish CI -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, ci

;;; Commentary:
;; Initializes package archives and installs prerequisite packages required for
;; running ELPAish builds and preflight checks in headless CI environments.

;;; Code:

(require 'package)

(setq package-user-dir (expand-file-name "elpa-ci/" default-directory))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(defconst elpaish-bootstrap-deps
  ;; f/s are runtime deps of registered packages (xtdlib) that ELPAish itself
  ;; doesn't use. Preflight byte-compilation loads each package's real
  ;; `require's, so those deps must be installed here too, or compilation
  ;; fails with "Cannot open load file" and the package gets quarantined.
  '(package-lint magit projectile transient modus-themes f s)
  "Prerequisite packages needed to build and validate the ecosystem in CI.")

(dolist (pkg elpaish-bootstrap-deps)
  (unless (package-installed-p pkg)
    (condition-case err
        (package-install pkg)
      (error (message "Warning: %s installation skipped or failed: %s" pkg (error-message-string err))))))

(provide 'bootstrap-elpaish)
;;; bootstrap-elpaish.el ends here
