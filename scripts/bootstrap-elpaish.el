;;; bootstrap-elpaish.el --- Bootstrap package environment for ELPAish CI -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, ci

;;; Commentary:
;; Initializes package archives, package-user-dir, and derives core dependencies
;; for the ELPAish package itself by processing pkg/elpaish.el.

;;; Code:

(require 'package)

(let ((package-user-dir (expand-file-name "elpa-ci/" default-directory))
      (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                          ("melpa" . "https://melpa.org/packages/"))))
  (package-initialize)
  (unless (bound-and-true-p package-archive-contents)
    (package-refresh-contents))

  ;; Load minimal installer (0 external dependencies) and derive/install elpaish's own dependencies
  (let* ((pkg-dir (expand-file-name "pkg" default-directory))
         (main-file (expand-file-name "elpaish.el" pkg-dir)))
    (add-to-list 'load-path pkg-dir)
    (require 'elpaish-install)
    (elpaish-install-ensure-package-dependencies main-file)))

(provide 'bootstrap-elpaish)
;;; bootstrap-elpaish.el ends here
