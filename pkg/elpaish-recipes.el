;;; elpaish-recipes.el --- Recipe discovery and configuration for ELPAish -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maintenance, tools, elpa, package

;;; Commentary:
;; Recipe path resolution, package loading, and monorepo discovery tooling for
;; the ELPAish package repository builder.  Decoupled from specific package
;; manifests — package definitions are loaded from a top-level `packages.el'
;; file or registered dynamically via `elpaish-register-package'.

;;; Code:

(require 'elpaish)

(defcustom elpaish-recipe-local-search-dirs '("~/src/")
  "Local checkout root directories searched by `elpaish-recipe-path'.
Each recipe's bare directory NAME is looked up under every root, in
order (after which \"external/NAME\" is tried under
`user-emacs-directory' and under `default-directory') — the first
match that exists on disk is used.  Falls back to the recipe's
remote URL if no local directory matches.

Customizing this lets a developer work against their personal
local checkouts without baking any assumptions about one
maintainer's personal directory layout into individual recipes."
  :type '(repeat directory)
  :group 'elpaish)

(defcustom elpaish-packages-file "packages.el"
  "Filename or path of the top-level package definitions file."
  :type 'string
  :group 'elpaish)

(defun elpaish-recipe-path (name remote-url)
  "Return the first existing local checkout of NAME, or REMOTE-URL.
NAME is a bare directory name (e.g. \"xtdlib\"), not a full path — this
searches `elpaish-recipe-local-search-dirs', then \"external/NAME\" under
`user-emacs-directory' and under `default-directory', so recipes never
hardcode where any particular maintainer's checkouts happen to live."
  (let ((roots (append elpaish-recipe-local-search-dirs
                       (list (expand-file-name "external/" user-emacs-directory)
                             (expand-file-name "external/" default-directory)))))
    (or (seq-some (lambda (root)
                    (let ((cand (expand-file-name name (expand-file-name root))))
                      (and (file-directory-p cand) cand)))
                  roots)
        remote-url)))

(defun elpaish-find-packages-file (&optional file)
  "Locate package definitions FILE across current directory hierarchy.
Defaults to `elpaish-packages-file'."
  (let* ((target (or file elpaish-packages-file))
         (candidates
          (delq nil
		;; TODO refactor this as a list without conitionals and then filter nils
                (list (and (file-name-absolute-p target) (file-exists-p target) target)
                      (let ((p (expand-file-name target default-directory)))
                        (and (file-exists-p p) p))
                      (let ((p (expand-file-name target (expand-file-name ".." default-directory))))
                        (and (file-exists-p p) p))
                      (when-let* ((lib (locate-library "elpaish")))
                        (let ((p (expand-file-name target (file-name-directory lib))))
                          (and (file-exists-p p) p)))
                      (when-let* ((lib (locate-library "elpaish")))
                        (let ((p (expand-file-name (concat "../" target) (file-name-directory lib))))
                          (and (file-exists-p p) p)))
                      (and (boundp 'user-emacs-directory)
                           (let ((p (expand-file-name target user-emacs-directory)))
                             (and (file-exists-p p) p)))))))
    (car candidates)))

;;;###autoload
(defun elpaish-load-packages (&optional file)
  "Load package recipe definitions from FILE (defaults to `elpaish-packages-file').
Returns the number of registered recipes."
  (interactive)
  (let ((resolved (elpaish-find-packages-file file)))
    (if (and resolved (file-exists-p resolved))
        (progn
          (load resolved nil t)
          (let ((count (hash-table-count elpaish-registry)))
            (message "Loaded %d ELPAish package definitions from %s" count resolved)
            count))
      (message "No package definitions file found matching %s" (or file elpaish-packages-file))
      (hash-table-count elpaish-registry))))

;;;###autoload
(defun elpaish-recipes-register-all ()
  "Load all package recipes from the active `packages.el' file."
  (interactive)
  ;; TODO this is redundant: remove one or make an alias
  (elpaish-load-packages))

;;; Monorepo Package Discovery Tooling

;;;###autoload
(defun elpaish-discover-recipes (root-dir &optional patterns)
  "Scan ROOT-DIR for subdirectories containing Emacs Lisp package headers.
Registers a recipe for each discovered package with appropriate `:source-dir'."
  (interactive "DDiscover packages in directory: ")
  (let ((root (expand-file-name root-dir))
        (count 0))
    (dolist (subdir (directory-files root t "\\`[^.]"))
      (when (file-directory-p subdir)
        (let* ((dir-name (file-name-nondirectory subdir))
               (main-el (expand-file-name (format "%s.el" dir-name) subdir)))
          (when (file-exists-p main-el)
            (let ((pkg-sym (intern dir-name)))
              (elpaish-register-package
               pkg-sym
               root
               :source-dir dir-name
               :files (or patterns '("*.el"))
               :summary (format "Package %s from %s" dir-name (file-name-nondirectory root)))
              (setq count (1+ count)))))))
    (message "Discovered and registered %d package recipes in %s" count root)
    count))

(provide 'elpaish-recipes)
;;; elpaish-recipes.el ends here
