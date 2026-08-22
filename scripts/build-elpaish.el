;;; build-elpaish.el --- Headless build runner for ELPAish archives -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, ci

;;; Commentary:
;; Headless entry point for building ELPAish multi-track package archives.
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

;; If secret key is provided in environment, configure loopback pinentry and import key
(let ((key-armor (getenv "ELPAISH_SIGNING_KEY"))
      (passphrase (or (getenv "ELPAISH_GPG_PASSPHRASE") "")))
  (when (and key-armor (not (string-empty-p key-armor)) (executable-find "gpg"))
    ;; Ensure gpg-agent is configured for loopback pinentry in CI
    (let* ((gnupg-dir (expand-file-name ".gnupg" (or (getenv "GNUPGHOME") (getenv "HOME"))))
           (agent-conf (expand-file-name "gpg-agent.conf" gnupg-dir)))
      (make-directory gnupg-dir t)
      (set-file-modes gnupg-dir #o700)
      (with-temp-file agent-conf
        (insert "allow-loopback-pinentry\n"))
      (set-file-modes agent-conf #o600)
      (call-process "gpgconf" nil nil nil "--kill" "gpg-agent")
      (call-process "gpg-connect-agent" nil nil nil "reloadagent" "/bye"))
    (with-temp-buffer
      (insert key-armor)
      (call-process-region (point-min) (point-max) "gpg" nil nil nil "--batch" "--import"))
    (setq elpaish-sign-packages t)
    (setq elpaish-gpg-passphrase passphrase)
    (setq elpaish-gpg-key (elpaish--detect-secret-key-id))
    (message "[elpaish] GPG signing initialized for key %s" elpaish-gpg-key)))

(message "[elpaish] Building ELPAish multi-track repository into %s..." elpaish-output-dir)
(elpaish-build-all 'all elpaish-output-dir)
(message "[elpaish] Multi-track build complete!")

(provide 'build-elpaish)
;;; build-elpaish.el ends here
