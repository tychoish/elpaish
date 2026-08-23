;;; build-elpaish.el --- Headless build runner for ELPAish archives -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, ci

;;; Commentary:
;; Headless entry point for building ELPAish package archives.
;; Invoked by GitHub Actions CI workflows or local batch runs.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
  (add-to-list 'load-path (expand-file-name "../pkg" dir))
  (add-to-list 'load-path (expand-file-name "../scripts" dir))
  (add-to-list 'load-path (expand-file-name "pkg" default-directory))
  (add-to-list 'load-path (expand-file-name "scripts" default-directory)))

;; Initialize package infrastructure so dependencies installed by bootstrap-elpaish.el
;; or present in elpa/ are activated.
(require 'package)
(let ((ci-elpa (expand-file-name "elpa-ci" default-directory))
      (local-elpa (expand-file-name "elpa" default-directory)))
  (cond
   ((file-directory-p ci-elpa)
    (setq package-user-dir ci-elpa)
    (package-initialize))
   ((file-directory-p local-elpa)
    (setq package-user-dir local-elpa)
    (package-initialize))
   (t
    (package-initialize))))

(require 'elpaish)
(require 'elpaish-recipes)
(require 'elpaish-website)
(require 'elpaish-check nil t)

;; Load external package definitions from top-level packages.el
(elpaish-load-packages)

;; Configure output directory from environment or default to public/
(setq elpaish-output-dir
      (or (getenv "ELPAISH_OUTPUT_DIR")
          (expand-file-name "public/" default-directory)))

;; Preflight quality gates can be toggled via ELPAISH_RUN_PREFLIGHT
(when (equal (getenv "ELPAISH_RUN_PREFLIGHT") "0")
  (setq elpaish-run-preflight nil))

;; Initialize GPG signing from environment variables (ELPAISH_SIGNING_KEY)
(elpaish-init-signing-from-env)

(message "[elpaish] Building ELPAish repository into %s..." elpaish-output-dir)
(elpaish-build-all 'all elpaish-output-dir)
(message "[elpaish] Repository build complete!")

(provide 'build-elpaish)
;;; build-elpaish.el ends here
