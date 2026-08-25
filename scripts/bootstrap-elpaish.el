;;; bootstrap-elpaish.el --- Bootstrap package environment for ELPAish CI -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, ci

;;; Commentary:
;; Initializes package archives and package-user-dir required for
;; running ELPAish builds and preflight checks in CI environments.
;; Package dependencies are derived and installed implicitly during package processing.

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

(provide 'bootstrap-elpaish)
;;; bootstrap-elpaish.el ends here
