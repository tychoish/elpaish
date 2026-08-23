;;; elpaish-recipes.el --- Recipe discovery and configuration for ELPAish -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maintenance, tools, elpa, package

;;; Commentary:
;; Recipe path resolution, package loading, and monorepo discovery tooling for
;; the ELPAish package repository builder.  Decoupled from specific package
;; manifests — package definitions are loaded from a top-level `packages.el'
;; file or registered dynamically via `elpaish-register-package'.

;;; Code:

(require 'cl-lib)
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
Defaults to `elpaish-packages-file'. An absolute FILE is used as-is (each
candidate root below is ignored by `expand-file-name' in that case)."
  (let* ((target (or file elpaish-packages-file))
         (lib-dir (when-let* ((lib (locate-library "elpaish")))
                    (file-name-directory lib)))
         (roots (delq nil
                      (list default-directory
                            (expand-file-name ".." default-directory)
                            lib-dir
                            (and lib-dir (expand-file-name ".." lib-dir))
                            (and (boundp 'user-emacs-directory) user-emacs-directory)))))
    (seq-some (lambda (root)
                (let ((p (expand-file-name target root)))
                  (and (file-exists-p p) p)))
              roots)))

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
(defalias 'elpaish-recipes-register-all 'elpaish-load-packages
  "Load all package recipes from the active `packages.el' file.")

;;; Monorepo Package Discovery Tooling

(defun elpaish--package-header-p (file)
  "Return non-nil if FILE looks like an Emacs Lisp package entry point."
  (and (file-regular-p file)
       (with-temp-buffer
         (insert-file-contents file nil 0 4096)
         (goto-char (point-min))
         (re-search-forward "^;;; [^ ]+\\.el --- .+-\\*-.*-\\*-" nil t))))

(defun elpaish--discover-candidates (root)
  "Return a list of (SOURCE-DIR-REL . MAIN-FILE) for packages nested under ROOT.
A subdirectory is a candidate when it contains a `<dir-name>.el' file with a
recognizable package header comment."
  (thread-last (directory-files root t "\\`[^.]")
    (seq-filter #'file-directory-p)
    (seq-map (lambda (dir)
               (let* ((dir-name (file-name-nondirectory (directory-file-name dir)))
                      (main-file (expand-file-name (format "%s.el" dir-name) dir)))
                 (when (elpaish--package-header-p main-file)
                   (cons (file-relative-name dir root) main-file)))))
    (delq nil)))

;;;###autoload
(cl-defun elpaish-discover-recipes (root &key branch files)
  "Scan ROOT for monorepo subpackages and register a recipe for each.
Looks for immediate subdirectories containing a `<dir-name>.el' file with a
package header, and registers each with the appropriate `:source-dir'.
BRANCH defaults to `elpaish-default-branch'. FILES overrides the default
single-file `(\"<name>.el\")' pattern used for every discovered package."
  (interactive "DMonorepo root directory: ")
  (let* ((expanded-root (expand-file-name root))
         (candidates (elpaish--discover-candidates expanded-root)))
    (seq-do
     (lambda (candidate)
       (let* ((source-dir-rel (string-remove-suffix "/" (car candidate)))
              (name (file-name-base (cdr candidate))))
         (elpaish-register-package
          (intern name)
          expanded-root
          :branch (or branch elpaish-default-branch)
          :source-dir source-dir-rel
          :files (or files (list (format "%s.el" name))))))
     candidates)
    (message "Discovered and registered %d monorepo package(s) under %s."
             (length candidates) expanded-root)
    candidates))

(provide 'elpaish-recipes)
;;; elpaish-recipes.el ends here
