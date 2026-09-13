;;; bootstrap-elpaish.el --- Bootstrap package environment for ELPAish CI -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, ci

;;; Commentary:
;; Initializes package archives, package-user-dir, and derives core dependencies
;; for ELPAish and all registered packages in packages.el.

;;; Code:

(require 'cl-lib)
(unless (macrop 'incf)
  (defmacro incf (place &optional delta)
    "Increment PLACE by DELTA (default 1)."
    (declare (debug (gv-place &optional form)))
    (if (and (symbolp place) (null delta))
        (list 'setq place (list '1+ place))
      (list 'cl-incf place delta))))

(unless (macrop 'decf)
  (defmacro decf (place &optional delta)
    "Decrement PLACE by DELTA (default 1)."
    (declare (debug (gv-place &optional form)))
    (if (and (symbolp place) (null delta))
        (list 'setq place (list '1- place))
      (list 'cl-decf place delta))))

(unless (macrop 'static-when)
  (defmacro static-when (condition &rest body)
    "A conditional compilation macro."
    (declare (indent 1) (debug t))
    (when (eval condition lexical-binding)
      (cons 'progn body))))

(require 'package)

(let ((ci-dir (expand-file-name "elpa-ci" default-directory)))
  (setq package-user-dir ci-dir)
  (dolist (d (file-expand-wildcards (expand-file-name "*" ci-dir)))
    (when (file-directory-p d)
      (add-to-list 'load-path d))))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (bound-and-true-p package-archive-contents)
  (package-refresh-contents))

;; Ensure compat is installed early
(unless (package-installed-p 'compat)
  (condition-case nil
      (package-install 'compat)
    (error nil)))
(let ((ci-dir (expand-file-name "elpa-ci" default-directory)))
  (dolist (d (file-expand-wildcards (expand-file-name "*" ci-dir)))
    (when (file-directory-p d)
      (add-to-list 'load-path d))))
(require 'compat nil t)

;; Load minimal installer and install dependencies for elpaish and all registered packages
(let* ((pkg-dir (expand-file-name "pkg" default-directory))
       (main-file (expand-file-name "elpaish.el" pkg-dir)))
  (add-to-list 'load-path pkg-dir)
  (require 'elpaish-install)
  (elpaish-install-ensure-package-dependencies main-file)
  (package-initialize)
  (let ((ci-dir (expand-file-name "elpa-ci" default-directory)))
    (dolist (d (file-expand-wildcards (expand-file-name "*" ci-dir)))
      (when (file-directory-p d)
        (add-to-list 'load-path d))))
  (require 'compat nil t)
  (require 'elpaish)
  (require 'elpaish-recipes)
  (elpaish-load-packages)
  (dolist (recipe (hash-table-values elpaish-registry))
    (elpaish-install-ensure-package-dependencies recipe))
  (package-initialize))

(provide 'bootstrap-elpaish)
;;; bootstrap-elpaish.el ends here
