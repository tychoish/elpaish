;;; elpaish-install.el --- Package bootstrapping and upgrade utilities -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maint, tools, package
;; URL: https://github.com/tychoish/elpaish

;;; Commentary:
;; Client-side helper module for installing groups of packages, bootstrapping
;; configurations, and upgrading installed packages with cooperative non-blocking
;; yielding and continue-on-error semantics.

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'gen nil t)

(declare-function elpaish-recipe-p "elpaish" (object))
(declare-function elpaish-recipe-name "elpaish" (recipe))
(declare-function elpaish-recipe-requires "elpaish" (recipe))
(declare-function copy-elpaish-recipe "elpaish" (recipe))
(declare-function elpaish-recipe-name-from-url "elpaish-recipes" (url))
(declare-function elpaish-register-package "elpaish" (name repository-path &rest options))
(declare-function elpaish-build-package "elpaish" (recipe &optional stream output-directory-path))
(declare-function elpaish--recipe-source-path "elpaish" (recipe))
(declare-function sprite-future-then "sprite" (future callback))
(declare-function gen-run-async "gen" (fn sequence &rest args))
(declare-function package--reload-previously-loaded "package" (pkg-desc &optional warn))
(declare-function package--description-file "package" (dir))
(declare-function package-quickstart-refresh "package" ())

(defvar elpaish-work-dir)
(defvar elpaish-sign-packages)
(defvar elpaish-registry)
(defvar elpaish--resolved-repo-path-cache)
(defvar package-quickstart-file)
(defvar package-user-dir)
(defvar warning-inhibit-types)
(defvar warning-suppress-types)
(defvar warning-suppress-log-types)

(defgroup elpaish-install nil
  "Package bootstrapping and upgrade utilities."
  :group 'package
  :prefix "elpaish-install-")

(define-obsolete-variable-alias
  'elpaish-install-bootstrap-packages-list
  'elpaish-install-packages-list
  "0.3.0")

(define-obsolete-variable-alias
  'elpaish-install-bootstrap-packages
  'elpaish-install-packages-list
  "0.2.0")

(defcustom elpaish-install-packages-list
  '(compat web-server htmlize annotated-completing-read transient package-lint)
  "List of package symbols to ensure are installed on the system.
Used as default target package list for `elpaish-install-packages'
when no explicit packages are specified."
  :type '(repeat symbol)
  :group 'elpaish-install)

(defcustom elpaish-install-upgrade-packages-list nil
  "List of package symbols to upgrade on demand.
Packages in this list must be a strict subset of
`elpaish-install-packages-list'.  Used as default target package list
for `elpaish-install-upgrade-packages'."
  :type '(repeat symbol)
  :group 'elpaish-install)

(defcustom elpaish-install-concurrency 1
  "Maximum concurrent package operations when running asynchronously.
Defaults to 1 because in-session package installation modifies global
`load-path' and `package-alist' state."
  :type 'integer
  :group 'elpaish-install)

(defcustom elpaish-install-yield-delay 0.005
  "Delay in seconds between package operations when yielding to the event loop.
Allows the Emacs event loop to remain responsive and process user input."
  :type 'number
  :group 'elpaish-install)

(defcustom elpaish-install-package-updated-functions nil
  "Hook run when a package is installed or upgraded by `elpaish-install'.
Each function is called with three arguments: PKG (symbol), STATUS
  (\\='installed or \\='upgraded), and RES (the result plist)."
  :type 'hook
  :group 'elpaish-install)

(defcustom elpaish-install-bootstrap-finished-functions nil
  "Hook run when package bootstrap or installation completes.
Each function is called with two arguments: RES-LIST (the list of package
result plists) and ACTION (symbol, \\='upgrade or \\='install)."
  :type 'hook
  :group 'elpaish-install)

(defvar elpaish-install--active-cancel-handle nil
  "Cancellation handle function for active async package installation.")

;;;###autoload
(defun elpaish-install-register-package (&rest pkgs)
  "Append unique package symbols from PKGS to `elpaish-install-packages-list'.
PKGS can be package symbols or lists of package symbols.  Updates
`elpaish-install-packages-list' and returns it."
  (let ((new-pkgs nil))
    (dolist (p (flatten-tree pkgs))
      (when (and (symbolp p)
                 p
                 (not (memq p elpaish-install-packages-list))
                 (not (memq p new-pkgs)))
        (push p new-pkgs)))
    (when new-pkgs
      (setq elpaish-install-packages-list
            (append elpaish-install-packages-list (nreverse new-pkgs)))))
  elpaish-install-packages-list)

(define-obsolete-function-alias
  'elpaish-install-register-bootstrap-package
  #'elpaish-install-register-package
  "0.3.0")

(define-obsolete-function-alias
  'elpaish-install-register-bootstrap-packages
  #'elpaish-install-register-package
  "0.2.0")

(define-obsolete-function-alias
  'elpaish-install-add-bootstrap-packages
  #'elpaish-install-register-package
  "0.2.0")

;;;###autoload
(defun elpaish-install-register-upgrade-package (&rest pkgs)
  "Append unique package symbols from PKGS to upgrade list.
PKGS can be package symbols or lists of package symbols.  Packages
are also ensured to be in `elpaish-install-packages-list' so that
the upgrade list is a strict subset of the installed packages list.
Returns `elpaish-install-upgrade-packages-list'."
  (let ((new-pkgs nil))
    (dolist (p (flatten-tree pkgs))
      (when (and (symbolp p) p)
        ;; Enforce strict subset: package must be in List 1
        (unless (memq p elpaish-install-packages-list)
          (elpaish-install-register-package p))
        (when (and (not (memq p elpaish-install-upgrade-packages-list))
                   (not (memq p new-pkgs)))
          (push p new-pkgs))))
    (when new-pkgs
      (setq elpaish-install-upgrade-packages-list
            (append elpaish-install-upgrade-packages-list (nreverse new-pkgs)))))
  elpaish-install-upgrade-packages-list)

(define-obsolete-function-alias
  'elpaish-install-register-upgrade-packages
  #'elpaish-install-register-upgrade-package
  "0.3.0")

(defun elpaish-install--get-upgrade-packages ()
  "Return packages from upgrade list that are in `elpaish-install-packages-list'."
  (seq-filter (lambda (p) (memq p elpaish-install-packages-list))
              elpaish-install-upgrade-packages-list))

(defun elpaish-install--extract-header-requires (file)
  "Extract package requirements list from FILE header using `package-buffer-info'."
  (require 'package)
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let* ((pkg-info (condition-case nil (package-buffer-info) (error nil))))
        (package-desc-reqs pkg-info)))))

;;;###autoload
(defun elpaish-install-ensure-package-dependencies (dir-or-recipe)
  "Ensure dependencies in Package-Requires of DIR-OR-RECIPE are installed.
DIR-OR-RECIPE can be an `elpaish-recipe' struct, a directory string, or a file
string containing package sources.  Extracts dependencies from package header
and invokes `package-install' for any uninstalled dependency."
  (unless package-archives
    (user-error "Cannot ensure package dependencies: `package-archives' is empty"))
  (unless (bound-and-true-p package-archive-contents)
    (package-initialize))
  (let* ((recipe (if (and (fboundp 'elpaish-recipe-p) (elpaish-recipe-p dir-or-recipe))
                     dir-or-recipe
                   nil))
         (reqs (cond
                ((and recipe (elpaish-recipe-requires recipe))
                 (elpaish-recipe-requires recipe))
                (t
                 (let* ((is-file (and (stringp dir-or-recipe) (file-regular-p dir-or-recipe)))
                        (source-dir (if recipe
                                        (elpaish--recipe-source-path recipe)
                                      (if is-file
                                          (file-name-directory (expand-file-name dir-or-recipe))
                                        (file-name-as-directory (expand-file-name dir-or-recipe)))))
                        (name (if recipe
                                  (elpaish-recipe-name recipe)
                                (if is-file
                                    (file-name-base dir-or-recipe)
                                  (file-name-nondirectory (directory-file-name source-dir)))))
                        (main-file (if is-file
                                       (expand-file-name dir-or-recipe)
                                     (expand-file-name (format "%s.el" name) source-dir))))
                   (elpaish-install--extract-header-requires main-file))))))
    (when reqs
      (let ((missing nil))
        (dolist (req reqs)
          (let ((dep-pkg (if (consp req) (car req) req)))
            (unless (or (eq dep-pkg 'emacs)
                        (package-installed-p dep-pkg)
                        ;; Skip packages that are part of the local ELPAish repository
                        (and (boundp 'elpaish-registry)
                             (hash-table-p elpaish-registry)
                             (gethash (symbol-name dep-pkg) elpaish-registry)))
              (push dep-pkg missing))))
        (when missing
          (message "Refreshing package archive contents to install missing dependencies: %S" missing)
          (package-refresh-contents)
          (dolist (dep-pkg missing)
            (message "Installing missing package dependency: %s" dep-pkg)
            (condition-case err
                (package-install dep-pkg)
              (error
               (message "Warning: Failed to install package dependency %s: %s"
                        dep-pkg (error-message-string err))))))))))

;;;###autoload
(defun elpaish-install-repository (repository &rest options)
  "Build REPOSITORY and install its package in the current Emacs session.
REPOSITORY is a local directory, Git URL, or `elpaish-recipe'.  For
strings, OPTIONS are keyword arguments accepted by
`elpaish-register-package'; :name overrides the name derived from the
repository path.  For example:

  (elpaish-install-repository \"https://example.org/foo.git\"
                              :name \"foo\" :branch \"main\")

A recipe object already contains its options and does not accept OPTIONS.
The build uses the snapshot stream and a temporary output directory.  Remote
repositories are cloned into a temporary checkout; local directories are used
as-is, so :branch applies only to remote repositories.  Artifacts are not
signed.  Dependencies are installed by package.el.
Return the installed `package-desc', or signal an error if the build fails."
  (interactive (list (read-string "Repository directory or Git URL: ")))
  (require 'elpaish-recipes)
  (when (and (elpaish-recipe-p repository) options)
    (error "Recipe objects cannot be combined with registration options"))
  (unless (or (elpaish-recipe-p repository) (stringp repository))
    (signal 'wrong-type-argument (list 'string-or-recipe-p repository)))
  (unless (and (zerop (% (length options) 2))
               (cl-loop for (key _value) on options by #'cddr always (keywordp key)))
    (error "Registration options must be keyword/value pairs"))
  (let* ((temp-dir (make-temp-file "elpaish-install-" t))
         (elpaish-work-dir (expand-file-name "repos" temp-dir))
         (elpaish-sign-packages nil)
         (elpaish-registry (make-hash-table :test 'equal))
         (elpaish--resolved-repo-path-cache (make-hash-table :test 'eq)))
    (unwind-protect
        (let* ((recipe
                (if (elpaish-recipe-p repository)
                    (copy-elpaish-recipe repository)
                  (let ((name (or (plist-get options :name)
                                  (elpaish-recipe-name-from-url
                                   (directory-file-name repository))))
                        (args nil))
                    (while options
                      (let ((key (pop options)) (value (pop options)))
                        (unless (eq key :name)
                          (setq args (append args (list key value))))))
                    (apply #'elpaish-register-package name repository args))))
               (artifact (elpaish-build-package recipe 'snapshot
                                                (expand-file-name "build" temp-dir))))
          (unless artifact
            (error "Could not build package %s" (elpaish-recipe-name recipe)))
          (let* ((built-desc (package-install-file artifact))
                 (desc (or (seq-find (lambda (candidate)
                                       (equal (package-desc-version candidate)
                                              (package-desc-version built-desc)))
                                     (cdr (assq (package-desc-name built-desc) package-alist)))
                           built-desc)))
            (when (and desc (fboundp 'package--reload-previously-loaded))
              (package--reload-previously-loaded desc))
            desc))
      (delete-directory temp-dir t))))

;;;; Core Single Package Operation

(defun elpaish-install--process-package (pkg &optional action refresh)
  "Perform single package PKG operation for ACTION (\\='upgrade or \\='install).
ACTION defaults to \\='upgrade, which installs PKG if missing or upgrades it
if an update is available.  If REFRESH is non-nil, refresh archive contents."
  (require 'package)
  (unless (bound-and-true-p package-archive-contents)
    (package-initialize))
  (when refresh
    (package-refresh-contents))
  (let ((act (or action 'upgrade)))
    (condition-case err
        (pcase act
          ('install
           (cond
            ((package-installed-p pkg)
             (list :status :already-installed :pkg pkg))
            (t
             (package-install pkg)
             (when (fboundp 'package--reload-previously-loaded)
               (when-let* ((desc (cadr (assq pkg package-alist))))
                 (condition-case nil
                     (package--reload-previously-loaded desc)
                   (error nil))))
             (let ((res (list :status :installed :pkg pkg)))
               (run-hook-with-args 'elpaish-install-package-updated-functions
                                   pkg 'installed res)
               res))))
          (_
           (cond
            ((not (package-installed-p pkg))
             (package-install pkg)
             (when (fboundp 'package--reload-previously-loaded)
               (when-let* ((desc (cadr (assq pkg package-alist))))
                 (condition-case nil
                     (package--reload-previously-loaded desc)
                   (error nil))))
             (let ((res (list :status :installed :pkg pkg)))
               (run-hook-with-args 'elpaish-install-package-updated-functions
                                   pkg 'installed res)
               res))
            ((fboundp 'package-upgrade)
             (let* ((installed-desc (cadr (assq pkg package-alist)))
                    (inst-ver (and installed-desc (package-desc-version installed-desc))))
               (condition-case sub-err
                   (progn
                     (package-upgrade pkg)
                     (let* ((new-desc (cadr (assq pkg package-alist)))
                            (new-ver (and new-desc (package-desc-version new-desc)))
                            (from-str (and inst-ver (package-version-join inst-ver)))
                            (to-str (and new-ver (package-version-join new-ver)))
                            (res (list :status :upgraded :pkg pkg :from from-str :to to-str)))
                       (run-hook-with-args 'elpaish-install-package-updated-functions
                                           pkg 'upgraded res)
                       res))
                 (user-error
                  (list :status :up-to-date :pkg pkg))
                 (error
                  (signal (car sub-err) (cdr sub-err))))))
            (t
             (let* ((installed-desc (cadr (assq pkg package-alist)))
                    (archive-desc (cadr (assq pkg package-archive-contents)))
                    (inst-ver (and installed-desc (package-desc-version installed-desc)))
                    (arch-ver (and archive-desc (package-desc-version archive-desc))))
               (cond
                ((and inst-ver arch-ver (version-list-< inst-ver arch-ver))
                 (package-install pkg)
                 (when (fboundp 'package--reload-previously-loaded)
                   (when-let* ((new-desc (cadr (assq pkg package-alist))))
                     (condition-case nil
                         (package--reload-previously-loaded new-desc)
                       (error nil))))
                 (let* ((from-str (package-version-join inst-ver))
                        (to-str (package-version-join arch-ver))
                        (res (list :status :upgraded :pkg pkg :from from-str :to to-str)))
                   (run-hook-with-args 'elpaish-install-package-updated-functions
                                       pkg 'upgraded res)
                   res))
                (t
                 (list :status :up-to-date :pkg pkg))))))))
      (error
       (list :status :failed :pkg pkg :error (error-message-string err))))))

;;;; Summary and Completion Helpers

(defun elpaish-install--report-summary (res-list action)
  "Display summary message for RES-LIST and ACTION."
  (let ((installed nil)
        (upgraded nil)
        (up-to-date 0)
        (failed nil))
    (dolist (res res-list)
      (let ((p (plist-get res :pkg)))
        (pcase (plist-get res :status)
          (':installed (push p installed))
          (':upgraded  (push p upgraded))
          (':up-to-date (cl-incf up-to-date))
          (':already-installed (cl-incf up-to-date))
          (':failed    (push (cons p (plist-get res :error)) failed)))))
    (let ((parts nil))
      (when upgraded
        (push (format "%d upgraded (%s)"
                      (length upgraded)
                      (mapconcat #'symbol-name (nreverse upgraded) ", "))
              parts))
      (when installed
        (push (format "%d installed (%s)"
                      (length installed)
                      (mapconcat #'symbol-name (nreverse installed) ", "))
              parts))
      (when (> up-to-date 0)
        (push (format "%d up to date" up-to-date) parts))
      (when failed
        (push (format "%d failed (%s)"
                      (length failed)
                      (mapconcat (lambda (f) (format "%s" (car f))) (nreverse failed) ", "))
              parts))
      (cond
       ((and (null upgraded) (null installed) (null failed))
        (message "[elpaish] %s complete: all %d package(s) up to date."
                 (if (eq action 'install) "Installation" "Bootstrap")
                 up-to-date))
       (t
        (message "[elpaish] %s complete: %s."
                 (if (eq action 'install) "Installation" "Bootstrap")
                 (string-join (nreverse parts) ", ")))))))

(defun elpaish-install--remote-eval-form (pkg action parent-user-dir parent-archives refresh)
  "Generate Lisp form for remote sprite execution of PKG with ACTION.
ACTION is \\='install or \\='upgrade.
PARENT-USER-DIR and PARENT-ARCHIVES configure the remote package environment.
If REFRESH is non-nil, refreshes package contents remotely."
  `(progn
     (eval-when-compile (require 'package))
     (let ((package-user-dir ,parent-user-dir)
           (package-archives ',parent-archives))
       (require 'elpaish-install)
       (elpaish-install--process-package ',pkg ',action ,refresh))))

(defun elpaish-install--on-async-complete (res-list action callback)
  "Handle completion of async package operations RES-LIST for ACTION.
Reloads package contents in the main Emacs instance, activates updated packages,
and invokes CALLBACK."
  (condition-case nil
      (if (fboundp 'package-read-all-archive-contents)
          (package-read-all-archive-contents)
        (package-initialize))
    (error nil))
  (dolist (res res-list)
    (when (memq (plist-get res :status) '(:installed :upgraded))
      (when-let* ((pkg (plist-get res :pkg)))
        (condition-case nil
            (progn
              (package-activate pkg)
              (when (fboundp 'package--reload-previously-loaded)
                (when-let* ((desc (cadr (assq pkg package-alist))))
                  (package--reload-previously-loaded desc))))
          (error nil))
        (pcase (plist-get res :status)
          (:installed
           (message "[elpaish] Installed and activated %s" pkg))
          (:upgraded
           (message "[elpaish] Upgraded and reloaded %s" pkg)))
        (run-hook-with-args 'elpaish-install-package-updated-functions
                            pkg
                            (if (eq (plist-get res :status) :installed) 'installed 'upgraded)
                            res))))
  (elpaish-install--report-summary res-list action)
  (run-hook-with-args 'elpaish-install-bootstrap-finished-functions res-list action)
  (when callback (funcall callback res-list)))

;;;; Batch Execution

(cl-defun elpaish-install--do-packages (pkgs &key (action 'upgrade) refresh)
  "Perform package operation ACTION for PKGS synchronously.
If REFRESH is non-nil, call `package-refresh-contents' first."
  (let ((target-pkgs (cond
                      ((eq action 'install) (seq-remove #'package-installed-p pkgs))
                      (t pkgs))))
    (cond
     ((and (eq action 'install) (null target-pkgs))
      nil)
     (t
      (unless (bound-and-true-p package-archive-contents)
        (package-initialize))
      (when (or refresh (eq action 'upgrade) (null package-archive-contents))
        (message "[elpaish] Refreshing package archive contents...")
        (package-refresh-contents))
      (let* ((results nil)
             (total (length target-pkgs))
             (idx 0))
        (dolist (pkg target-pkgs)
          (cl-incf idx)
          (let ((res (elpaish-install--process-package pkg action nil)))
            (push res results)
            (pcase (plist-get res :status)
              (:installed
               (message "[elpaish] [%d/%d] Installed %s (activated)" idx total pkg))
              (:upgraded
               (if (and (plist-get res :from) (plist-get res :to))
                   (message "[elpaish] [%d/%d] Upgraded %s from %s to %s (reloaded)"
                            idx total pkg (plist-get res :from) (plist-get res :to))
                 (message "[elpaish] [%d/%d] Upgraded %s (reloaded)" idx total pkg)))
              (:failed
               (message "[elpaish] [%d/%d] Failed %s: %s"
                        idx total pkg (plist-get res :error)))
              (_
               (message "[elpaish] [%d/%d] %s is up to date" idx total pkg)))))
        (let ((final-res (nreverse results)))
          (elpaish-install--report-summary final-res action)
          (run-hook-with-args 'elpaish-install-bootstrap-finished-functions
                              final-res action)
          final-res))))))

(defun elpaish-install--do-install-packages (pkgs refresh)
  "Perform actual installation sequence for PKGS synchronously.
If REFRESH is non-nil, call `package-refresh-contents' first."
  (elpaish-install--do-packages pkgs :action 'install :refresh refresh))

(defun elpaish-install--do-upgrade-packages (pkgs)
  "Perform actual package upgrade sequence for PKGS synchronously."
  (elpaish-install--do-packages pkgs :action 'upgrade :refresh t))

(cl-defun elpaish-install--do-packages-async (pkgs &key (action 'upgrade) refresh concurrency delay pool-size callback)
  "Perform package operation ACTION for PKGS asynchronously.
By default, uses `gen-run-async' yielding to the Emacs event loop.
If POOL-SIZE is non-nil and `sprite-pool-mapcar' is available, uses
out-of-process sprite pool.  Otherwise, falls back to `run-at-time'.
Calls CALLBACK when complete and returns a cancellation handle function."
  (let ((target-pkgs (cond
                      ((eq action 'install) (seq-remove #'package-installed-p pkgs))
                      (t pkgs))))
    (cond
     ((and (eq action 'install) (null target-pkgs))
      (when callback (funcall callback nil))
      (lambda (&optional _) nil))
     ((and pool-size
           (require 'sprite nil t)
           (require 'sprite-fleet nil t)
           (fboundp 'sprite-pool-mapcar))
      (let ((parent-user-dir package-user-dir)
            (parent-archives package-archives))
        (message "[elpaish] %s %d package(s) asynchronously using sprite pool..."
                 (if (eq action 'install) "Installing" "Bootstrapping")
                 (length target-pkgs))
        (let ((future (sprite-pool-mapcar
                       (lambda (pkg)
                         (elpaish-install--remote-eval-form pkg action parent-user-dir parent-archives refresh))
                       target-pkgs
                       :pool-size pool-size
                       :async t)))
          (sprite-future-then
           future
           (lambda (res-list)
             (elpaish-install--on-async-complete res-list action callback))))
        (lambda (&optional _action) nil)))
     ((fboundp 'gen-run-async)
      (unless (bound-and-true-p package-archive-contents)
        (package-initialize))
      (when (or refresh (eq action 'upgrade) (null package-archive-contents))
        (message "[elpaish] Refreshing package archive contents...")
        (package-refresh-contents))
      (let* ((results nil)
             (total (length target-pkgs))
             (idx 0))
        (message "[elpaish] %s %d package(s) asynchronously..."
                 (if (eq action 'install) "Installing" "Bootstrapping")
                 total)
        (let ((runner
               (gen-run-async
                (lambda (pkg done-cb)
                  (cl-incf idx)
                  (let ((res (elpaish-install--process-package pkg action nil)))
                    (push res results)
                    (pcase (plist-get res :status)
                      (:installed
                       (message "[elpaish] [%d/%d] Installed %s (activated)" idx total pkg))
                      (:upgraded
                       (if (and (plist-get res :from) (plist-get res :to))
                           (message "[elpaish] [%d/%d] Upgraded %s from %s to %s (reloaded)"
                                    idx total pkg (plist-get res :from) (plist-get res :to))
                         (message "[elpaish] [%d/%d] Upgraded %s (reloaded)" idx total pkg)))
                      (:failed
                       (message "[elpaish] [%d/%d] Failed %s: %s"
                                idx total pkg (plist-get res :error)))
                      (_
                       (message "[elpaish] [%d/%d] %s is up to date" idx total pkg)))
                    (funcall done-cb)))
                target-pkgs
                :concurrency (or concurrency elpaish-install-concurrency 1)
                :delay (or delay elpaish-install-yield-delay 0.0)
                :on-done
                (lambda ()
                  (setq elpaish-install--active-cancel-handle nil)
                  (let ((final-res (nreverse results)))
                    (elpaish-install--report-summary final-res action)
                    (run-hook-with-args 'elpaish-install-bootstrap-finished-functions
                                        final-res action)
                    (when callback
                      (funcall callback final-res))))
                :on-error
                (lambda (err)
                  (message "[elpaish] Error during package bootstrapping: %S" err)))))
          (setq elpaish-install--active-cancel-handle runner)
          runner)))
     (t
      ;; Fallback when gen-run-async is not available
      (run-at-time 0 nil
                   (lambda ()
                     (let ((res (elpaish-install--do-packages target-pkgs :action action :refresh refresh)))
                       (when callback (funcall callback res)))))
      (lambda (&optional _) nil)))))

;;;###autoload
(defun elpaish-install-cancel ()
  "Cancel any in-flight asynchronous package installation or bootstrap operations."
  (interactive)
  (let ((handle elpaish-install--active-cancel-handle))
    (setq elpaish-install--active-cancel-handle nil)
    (when (functionp handle)
      (ignore-errors (funcall handle :cancel)))))

;;;; Public Commands and Dispatchers

;;;###autoload
(cl-defun elpaish-install-packages (&rest args)
  "Ensure packages are installed on the system, blocking until complete.
Only refreshes package archives if at least one package is missing.
Packages in ARGS (or `elpaish-install-packages-list' if no packages specified)
are registered into `elpaish-install-packages-list'.

This installation operation is synchronous (blocking) by default, matching the
semantics of `use-package <name> :ensure t :defer t' during startup.

ARGS can contain package symbols, lists of package symbols, or keyword options:
  :refresh BOOL     - When non-nil, force package archive refresh even if
                      all packages are already installed.
  :sync BOOL        - When non-nil, execute synchronously (default t).
  :async BOOL       - When non-nil, execute asynchronously.
  :action ACTION    - Operation action.  If \='upgrade, delegates to
                      `elpaish-install-upgrade-packages'."
  (interactive)
  (let* ((interactive-p (called-interactively-p 'any))
         (remaining (if interactive-p nil args))
         (pkgs nil)
         (force-refresh nil)
         (action 'install)
         (sync nil)
         (async nil)
         (concurrency nil)
         (delay nil)
         (pool-size nil)
         (callback nil))
    (while remaining
      (let ((arg (pop remaining)))
        (cond
         ((eq arg :refresh)     (setq force-refresh (pop remaining)))
         ((eq arg :action)      (setq action (pop remaining)))
         ((eq arg :sync)        (setq sync (pop remaining)))
         ((eq arg :async)       (setq async (pop remaining)))
         ((eq arg :concurrency) (setq concurrency (pop remaining)))
         ((eq arg :delay)       (setq delay (pop remaining)))
         ((eq arg :pool-size)   (setq pool-size (pop remaining)))
         ((or (eq arg :callback) (eq arg :on-done))
          (setq callback (pop remaining)))
         ((listp arg)
          (dolist (p (flatten-tree arg))
            (when (symbolp p) (push p pkgs))))
         ((symbolp arg)
          (push arg pkgs)))))
    (setq pkgs (delete-dups (nreverse pkgs)))
    (cond
     ((eq action 'upgrade)
      (apply #'elpaish-install-upgrade-packages
             (append pkgs
                     (list :refresh force-refresh :sync sync :async async
                           :concurrency concurrency :delay delay
                           :pool-size pool-size :callback callback))))
     (t
      (when pkgs
        (apply #'elpaish-install-register-package pkgs))
      (let* ((target-pkgs (or pkgs elpaish-install-packages-list))
             (missing (seq-remove #'package-installed-p target-pkgs))
             (is-async (and async (not sync))))
        (cond
         ((and (null missing) (not force-refresh))
          (when interactive-p
            (message "[elpaish] All %d package(s) are already installed."
                     (length target-pkgs)))
          (when callback (funcall callback nil))
          nil)
         (is-async
          (elpaish-install--do-packages-async
           (if force-refresh target-pkgs missing)
           :action 'install
           :refresh force-refresh
           :concurrency concurrency
           :delay delay
           :pool-size pool-size
           :callback callback))
         (t
          (unless (bound-and-true-p package-archive-contents)
            (package-initialize))
          (when (or force-refresh missing (null package-archive-contents))
            (message "[elpaish] Refreshing package archive contents...")
            (package-refresh-contents))
          (let* ((to-install (if force-refresh target-pkgs missing))
                 (results nil)
                 (total (length to-install))
                 (idx 0))
            (dolist (pkg to-install)
              (cl-incf idx)
              (let ((res (elpaish-install--process-package pkg 'install nil)))
                (push res results)
                (pcase (plist-get res :status)
                  (:installed
                   (message "[elpaish] [%d/%d] Installed %s (activated)" idx total pkg))
                  (:failed
                   (message "[elpaish] [%d/%d] Failed %s: %s"
                            idx total pkg (plist-get res :error)))
                  (_
                   (message "[elpaish] [%d/%d] %s is up to date" idx total pkg)))))
            (let ((final-res (nreverse results)))
              (elpaish-install--report-summary final-res 'install)
              (run-hook-with-args 'elpaish-install-bootstrap-finished-functions
                                  final-res 'install)
              (when callback (funcall callback final-res))
              final-res)))))))))

(defun elpaish-install--installed-version (pkg)
  "Return the installed version of PKG as a string, or a placeholder."
  (if-let* ((desc (cadr (assq pkg package-alist))))
      (package-version-join (package-desc-version desc))
    "not installed"))

(defun elpaish-install--upgrade-progress (buffer entries refresh-state quickstart-state)
  "Render upgrade ENTRIES and stage states in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "ELPAish package upgrade\n\n"
                (format "%s Refresh package archives\n\n" refresh-state))
        (dolist (entry entries)
          (insert (format "%s %s: %s%s\n"
                          (nth 1 entry) (car entry) (nth 2 entry)
                          (if (nth 3 entry) (format " — %s" (nth 3 entry)) ""))))
        (insert (format "\n%s Rebuild package quickstart\n" quickstart-state))
        (goto-char (point-min))))
    (redisplay)))

(defun elpaish-install--upgrade-run (pkgs refresh sync delay callback)
  "Upgrade PKGS, updating a progress buffer throughout the operation.
REFRESH controls archive refresh, SYNC runs immediately, DELAY sets the
pause between asynchronous steps, and CALLBACK receives the results."
  (let* ((buffer (get-buffer-create "*ELPAish Upgrade Progress*"))
         (entries (mapcar (lambda (pkg)
                            (list pkg "[ ]" (elpaish-install--installed-version pkg) nil))
                          pkgs))
         (remaining (copy-sequence pkgs))
         (refresh-state (if refresh "[ ]" "[-]"))
         (quickstart-state "[ ]")
         (stage 'refresh)
         (results nil)
         (timer nil)
         (step nil)
         (render (lambda () (elpaish-install--upgrade-progress
                              buffer entries refresh-state quickstart-state))))
    (with-current-buffer buffer (special-mode))
    (funcall render)
    (display-buffer buffer)
    (setq step
          (lambda ()
            (progn
              (condition-case err
                  (pcase stage
                    ('refresh
                     (setq refresh-state (if refresh "[...]" "[-]"))
                     (funcall render)
                     (unless (bound-and-true-p package-archive-contents)
                       (package-initialize))
                     (dolist (entry entries)
                       (setf (nth 2 entry) (elpaish-install--installed-version (car entry))))
                     (when refresh (package-refresh-contents))
                     (setq refresh-state (if refresh "[x]" "[-]") stage 'packages))
                    ('packages
                     (if remaining
                         (let* ((pkg (pop remaining))
                                (entry (assq pkg entries)))
                           (setf (nth 1 entry) "[...]")
                           (funcall render)
                           (let* ((res (elpaish-install--process-package pkg 'upgrade nil))
                                  (status (plist-get res :status))
                                  (version (or (plist-get res :to)
                                               (elpaish-install--installed-version pkg))))
                             (push res results)
                             (setf (nth 1 entry) (if (eq status :failed) "[!]" "[x]")
                                   (nth 2 entry) version
                                   (nth 3 entry) (if (eq status :failed)
                                                     (plist-get res :error)
                                                   (substring (symbol-name status) 1)))))
                       (setq stage 'quickstart)))
                    ('quickstart
                     (setq quickstart-state "[...]")
                     (funcall render)
                     (let* ((final-results (nreverse results))
                            (changed (seq-some (lambda (res)
                                                 (memq (plist-get res :status) '(:installed :upgraded)))
                                               final-results))
                            (quickstart-enabled
                             (memq #'elpaish-install--on-bootstrap-finished
                                   elpaish-install-bootstrap-finished-functions))
                            (elpaish-install-bootstrap-finished-functions
                             (remove #'elpaish-install--on-bootstrap-finished
                                     elpaish-install-bootstrap-finished-functions)))
                       (when (and changed quickstart-enabled)
                         (elpaish-install-flush-package-quickstart))
                       (setq quickstart-state (if (and changed quickstart-enabled) "[x]" "[-]")
                             results final-results stage 'done
                             elpaish-install--active-cancel-handle nil)
                       (elpaish-install--report-summary results 'upgrade)
                       (run-hook-with-args 'elpaish-install-bootstrap-finished-functions results 'upgrade)
                       (when callback (funcall callback results)))))
                (error
                 (pcase stage
                   ('refresh (setq refresh-state (format "[!] %s" (error-message-string err))))
                   ('quickstart (setq quickstart-state (format "[!] %s" (error-message-string err)))))
                 (unless (eq stage 'done)
                   (setq elpaish-install--active-cancel-handle nil))
                 (setq stage 'done)
                 (message "[elpaish] Upgrade failed: %s" (error-message-string err))
                 (funcall render)
                 (when sync (signal (car err) (cdr err)))))
              (funcall render)
              (if (eq stage 'done)
                  nil
                (unless sync
                  (setq timer (run-at-time (or delay elpaish-install-yield-delay) nil step)))))))
    (if sync
        (progn (while (not (eq stage 'done)) (funcall step)) results)
      (setq timer (run-at-time 0 nil step))
      (let ((cancel (lambda (&optional _)
                      (when (timerp timer) (cancel-timer timer))
                      (setq remaining nil stage 'quickstart
                            elpaish-install--active-cancel-handle #'ignore)
                      (setq timer (run-at-time 0 nil step)))))
        (setq elpaish-install--active-cancel-handle cancel)
        cancel))))

;;;###autoload
(cl-defun elpaish-install-upgrade-packages (&rest args)
  "Upgrade on-demand packages to their latest versions.
Upgrades packages configured in `elpaish-install-upgrade-packages-list',
which must be a strict subset of `elpaish-install-packages-list'.
The upgrade operation is lazy and runs in the background asynchronously
with cooperative yielding.  When called interactively with a prefix argument
(e.g. \[universal-argument]), execution is synchronous.

ARGS can contain package symbols, lists of package symbols, or keyword options:
  :sync BOOL        - When non-nil, execute synchronously.
  :async BOOL       - When non-nil (default t), execute asynchronously.
  :refresh BOOL     - When non-nil (default t), refresh package archives.
  :concurrency N    - Accepted for compatibility; upgrades run sequentially.
  :delay SECS       - Yield delay in seconds
                      (default `elpaish-install-yield-delay').
  :callback FN      - Function called with result list on completion.
  :pool-size N      - Accepted for compatibility; upgrades run in this Emacs.

When no packages are specified in ARGS, defaults to
`elpaish-install-upgrade-packages-list'."
  (interactive "P")
  (let* ((interactive-p (called-interactively-p 'any))
         (prefix-arg-val (when interactive-p (car args)))
         (remaining (if interactive-p nil args))
         (pkgs nil)
         (refresh t)
         (sync (not (null prefix-arg-val)))
         (async :default)
         (concurrency nil)
         (delay nil)
         (pool-size nil)
         (callback nil))
    (while remaining
      (let ((arg (pop remaining)))
        (cond
         ((eq arg :sync)       (setq sync (pop remaining)))
         ((eq arg :async)      (setq async (pop remaining)))
         ((eq arg :refresh)    (setq refresh (pop remaining)))
         ((memq arg '(:action :status))
          (pop remaining))
         ((eq arg :concurrency)(setq concurrency (pop remaining)))
         ((eq arg :delay)      (setq delay (pop remaining)))
         ((eq arg :pool-size)  (setq pool-size (pop remaining)))
         ((or (eq arg :callback) (eq arg :on-done))
          (setq callback (pop remaining)))
         ((listp arg)
          (dolist (p (flatten-tree arg))
            (when (symbolp p) (push p pkgs))))
         ((symbolp arg)
          (push arg pkgs)))))
    (setq pkgs (delete-dups (nreverse pkgs)))
    (when elpaish-install--active-cancel-handle
      (user-error "An ELPAish package operation is already running"))
    (when pkgs
      (apply #'elpaish-install-register-upgrade-package pkgs))
    (let* ((raw-targets (or pkgs (elpaish-install--get-upgrade-packages)))
           (target-pkgs (seq-filter (lambda (p) (memq p elpaish-install-packages-list))
                                    raw-targets))
           (is-sync (or sync (eq async nil))))
      (cond
       ((null target-pkgs)
        (message "[elpaish] No packages configured in `elpaish-install-upgrade-packages-list' to upgrade.")
        (when callback (funcall callback nil))
        nil)
       (t
        (ignore concurrency pool-size)
        (elpaish-install--upgrade-run target-pkgs refresh is-sync delay callback))))))

;;;###autoload
(defalias 'elpaish-upgrade-packages #'elpaish-install-upgrade-packages)

;;;###autoload
(cl-defun elpaish-install-bootstrap-packages (&rest args)
  "Install or upgrade bootstrap packages (legacy dispatcher).
When :action is \='install, delegates to `elpaish-install-packages'.
Otherwise, installs missing packages and upgrades existing ones.

ARGS can contain package symbols, lists of package symbols, or keyword options:
  :sync BOOL        - When non-nil, execute synchronously.
  :async BOOL       - When non-nil (default t), execute asynchronously.
  :refresh BOOL     - When non-nil (default t), refresh package archives.
  :action ACTION    - Operation action: \='upgrade (default) or \='install.
  :concurrency N    - Concurrent jobs for `gen-run-async' (default 1).
  :delay SECS       - Yield delay in seconds
                      (default `elpaish-install-yield-delay').
  :callback FN      - Function called with result list on completion.
  :pool-size N      - When specified and sprite is available, use sprite pool.

When no packages are specified in ARGS, defaults to
`elpaish-install-packages-list'."
  (interactive "P")
  (let* ((interactive-p (called-interactively-p 'any))
         (prefix-arg-val (when interactive-p (car args)))
         (remaining (if interactive-p nil args))
         (action 'upgrade))
    (while remaining
      (let ((arg (pop remaining)))
        (when (eq arg :action)
          (setq action (pop remaining)))))
    (cond
     ((eq action 'install)
      (apply #'elpaish-install-packages args))
     (t
      (let* ((remaining (if interactive-p nil args))
             (pkgs nil)
             (refresh t)
             (sync (not (null prefix-arg-val)))
             (async :default)
             (concurrency nil)
             (delay nil)
             (pool-size nil)
             (callback nil))
        (while remaining
          (let ((arg (pop remaining)))
            (cond
             ((eq arg :sync)       (setq sync (pop remaining)))
             ((eq arg :async)      (setq async (pop remaining)))
             ((eq arg :refresh)    (setq refresh (pop remaining)))
             ((eq arg :action)     (pop remaining))
             ((eq arg :concurrency)(setq concurrency (pop remaining)))
             ((eq arg :delay)      (setq delay (pop remaining)))
             ((eq arg :pool-size)  (setq pool-size (pop remaining)))
             ((or (eq arg :callback) (eq arg :on-done))
              (setq callback (pop remaining)))
             ((listp arg)
              (dolist (p (flatten-tree arg))
                (when (symbolp p) (push p pkgs))))
             ((symbolp arg)
              (push arg pkgs)))))
        (let* ((target-pkgs (or (delete-dups (nreverse pkgs))
                                elpaish-install-packages-list))
               (is-sync (or sync (eq async nil))))
          (cond
           (is-sync
            (let ((res (elpaish-install--do-packages target-pkgs :action 'upgrade :refresh refresh)))
              (when callback (funcall callback res))
              res))
           (t
            (elpaish-install--do-packages-async
             target-pkgs
             :action 'upgrade
             :refresh refresh
             :concurrency concurrency
             :delay delay
             :pool-size pool-size
             :callback callback)))))))))

;;;###autoload
(cl-defun elpaish-install-packages-async (pkgs &key refresh pool-size callback)
  "Install PKGS asynchronously out-of-process using a sprite pool if available.
If REFRESH is non-nil, refresh archive contents.  POOL-SIZE is the sprite pool
concurrency.  Calls CALLBACK when complete."
  (elpaish-install-bootstrap-packages pkgs :action 'install :refresh refresh :pool-size (or pool-size 2) :callback callback :async t))

(define-obsolete-function-alias
  'elpaish-install-upgrade-packages-async
  #'elpaish-install-packages-async
  "0.2.0")

;;;; Quickstart management and optimization

(defvar elpaish-install--quickstart-refresh-timer nil
  "Timer used to debounce calls to `package-quickstart-refresh'.")

(defcustom elpaish-install-quickstart-refresh-delay 1.0
  "Idle time in seconds to wait before regenerating `package-quickstart-file'."
  :type 'number
  :group 'elpaish-install)

;;;###autoload
(defun elpaish-install-sync-package-load-paths ()
  "Prune stale package directories from `load-path'.
Also ensures current ELPA directories and autoloads are loaded."
  (let ((elpa-dir (expand-file-name "elpa" user-emacs-directory)))
    (when (file-directory-p elpa-dir)
      (setq load-path (seq-filter #'file-directory-p load-path))
      (dolist (dir (directory-files elpa-dir t "^[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)
          (let* ((pkg-base (file-name-nondirectory dir))
                 (pkg-name (replace-regexp-in-string "-[0-9].*\\'" "" pkg-base))
                 (autoloads-file (expand-file-name (concat pkg-name "-autoloads") dir)))
            (when (or (file-exists-p (concat autoloads-file ".el"))
                      (file-exists-p (concat autoloads-file ".elc")))
              (load autoloads-file t t))))))))

;;;###autoload
(defun elpaish-install-optimize-quickstart-file (file)
  "Transform FILE (package-quickstart.el) into a streamlined format.
Consolidates `load-path' additions into a static pre-expanded list,
strips docstrings, removes definition prefixes, strips unused let* scopes,
removes autoload feature provisions, and eliminates extraneous whitespace."
  (when (file-exists-p file)
    (with-temp-buffer
      (delay-mode-hooks (emacs-lisp-mode))
      (insert-file-contents file)
      (let ((dirs nil))
        ;; 1. Collect directories and delete individual (add-to-list 'load-path ...) forms
        (goto-char (point-min))
        (while (re-search-forward "(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))" nil t)
          (let ((match-beg (match-beginning 0))
                (match-end (match-end 0)))
            (save-excursion
              (when (re-search-backward "load-file-name \"\\([^\"]+\\)\"" nil t)
                (push (file-name-directory (match-string 1)) dirs)))
            (delete-region match-beg match-end)))

        ;; 2. Strip definition prefixes (register-definition-prefixes ...)
        (goto-char (point-min))
        (while (re-search-forward "(register-definition-prefixes" nil t)
          (backward-up-list 1)
          (let ((beg (point)))
            (forward-sexp 1)
            (when (looking-at "\n") (forward-char 1))
            (delete-region beg (point))))

        ;; 3. Strip docstrings from autoload definitions:
        ;; Match (autoload ... \"file\" \"docstring...\") and replace \"docstring...\" with nil
        (goto-char (point-min))
        (while (re-search-forward "(autoload[ \t\n\r]+['#][^ \t\n\r]+[ \t\n\r]+\"[^\"]+\"[ \t\n\r]+\"" nil t)
          (backward-char 1)
          (let ((beg (point)))
            (forward-sexp 1)
            (delete-region beg (point))
            (insert "nil")))

        ;; 4. Strip (provide '...-autoloads) forms
        (goto-char (point-min))
        (while (re-search-forward "(provide '[^)]+-autoloads)" nil t)
          (delete-region (match-beginning 0) (match-end 0)))

        ;; 5. Replace (let* ((load-file-name ...)(load-true-file-name load-file-name)) with (progn
        (goto-char (point-min))
        (while (re-search-forward "(let\\*\\s-*((\\s-*load-file-name\\s-+\"[^\"]+\"\\s-*)\\s-*(\\s-*load-true-file-name\\s-+load-file-name\\s-*))" nil t)
          (replace-match "(progn"))

        ;; 6. Clean up form-feeds and collapse blank lines
        (goto-char (point-min))
        (while (re-search-forward "[\x0c\r]" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (while (re-search-forward "\n[ \t]*\n[ \t]*\n+" nil t)
          (replace-match "\n\n"))

        ;; 7. Insert pre-expanded, deduplicated static load-path list at the top
        (goto-char (point-min))
        (forward-line 3)
        (let ((clean-dirs (delete-dups (mapcar #'expand-file-name (mapcar #'directory-file-name (nreverse dirs))))))
          (insert "(setq load-path\n      (append '")
          (prin1 clean-dirs (current-buffer))
          (insert "\n              load-path))\n\n"))

        ;; Save the optimized file
        (write-region (point-min) (point-max) file nil 'silent)))))

(defun elpaish-install--package-quickstart-refresh-optimize (orig-fn &rest args)
  "Optimize quickstart file format and compile bytecode."
  (require 'bytecomp)
  (let ((real-byte-compile (symbol-function 'byte-compile-file)))
    (cl-letf (((symbol-function 'byte-compile-file)
               (lambda (file)
                 (elpaish-install-optimize-quickstart-file file)
                 (with-temp-buffer
                   (let ((byte-compile-log-buffer (buffer-name))
                         (display-buffer-alist
                          (cons (cons (regexp-quote (buffer-name))
                                      '(display-buffer-no-window))
                                display-buffer-alist))
                         (warning-inhibit-types (cons '(bytecomp) (and (boundp 'warning-inhibit-types) warning-inhibit-types)))
                         (warning-suppress-types (cons '(bytecomp) (and (boundp 'warning-suppress-types) warning-suppress-types)))
                         (warning-suppress-log-types (cons '(bytecomp) (and (boundp 'warning-suppress-log-types) warning-suppress-log-types)))
                         (inhibit-message t)
                         (message-log-max nil))
                     (funcall real-byte-compile file)))
                 file)))
      (apply orig-fn args))))

;;;###autoload
(defun elpaish-install-package-quickstart-stale-p ()
  "Return non-nil when quickstart file or its .elc is missing or stale."
  (let ((elc (concat (file-name-sans-extension package-quickstart-file) ".elc")))
    (or (not (file-exists-p package-quickstart-file))
        (not (file-exists-p elc))
        (time-less-p (file-attribute-modification-time (file-attributes elc))
                     (file-attribute-modification-time (file-attributes package-quickstart-file)))
        (and (file-directory-p package-user-dir)
             (let ((quickstart-mtime (file-attribute-modification-time
                                      (file-attributes elc))))
               (seq-some (lambda (pkg-dir)
                           (and (file-directory-p pkg-dir)
                                (file-exists-p (expand-file-name (package--description-file pkg-dir) pkg-dir))
                                (time-less-p quickstart-mtime
                                             (file-attribute-modification-time (file-attributes pkg-dir)))))
                         (directory-files package-user-dir t "\\`[^.]" t)))))))

;;;###autoload
(defun elpaish-install-refresh-package-quickstart (&optional force)
  "Regenerate `package-quickstart-file' and clear the debounce timer.
When FORCE is nil, do nothing unless quickstart file is stale."
  (interactive "P")
  (setq elpaish-install--quickstart-refresh-timer nil)
  (when (or force (elpaish-install-package-quickstart-stale-p))
    (message "[elpaish] %s; refreshing package quickstart"
             (if force "Package changes detected or refresh requested"
               "Package quickstart is stale"))
    (require 'package)
    (elpaish-install-sync-package-load-paths)
    (package-quickstart-refresh)))

;;;###autoload
(defun elpaish-install-flush-package-quickstart ()
  "Ensure any pending `package-quickstart-refresh' runs immediately."
  (when (timerp elpaish-install--quickstart-refresh-timer)
    (cancel-timer elpaish-install--quickstart-refresh-timer)
    (setq elpaish-install--quickstart-refresh-timer nil))
  (elpaish-install-refresh-package-quickstart t))

;;;###autoload
(defun elpaish-install-schedule-quickstart-refresh (&rest _)
  "Debounce regeneration of `package-quickstart-file' after package operations."
  (if noninteractive
      (elpaish-install-refresh-package-quickstart t)
    (when (timerp elpaish-install--quickstart-refresh-timer)
      (cancel-timer elpaish-install--quickstart-refresh-timer))
    (setq elpaish-install--quickstart-refresh-timer
          (run-with-idle-timer elpaish-install-quickstart-refresh-delay
                               nil
                               (lambda () (elpaish-install-refresh-package-quickstart t))))))

(defun elpaish-install--on-bootstrap-finished (results _action)
  "Schedule quickstart refresh when RESULTS include changes."
  (when (seq-some (lambda (result)
                    (memq (plist-get result :status) '(:installed :upgraded)))
                  results)
    (elpaish-install-schedule-quickstart-refresh)))

(advice-add 'package-quickstart-refresh :around #'elpaish-install--package-quickstart-refresh-optimize)

(add-hook 'elpaish-install-bootstrap-finished-functions
          #'elpaish-install--on-bootstrap-finished)
(add-hook 'kill-emacs-hook #'elpaish-install-flush-package-quickstart)

;; Backward compatibility aliases for dot-emacs config
(defalias 'bootstrap-optimize-quickstart-file #'elpaish-install-optimize-quickstart-file)
(defalias 'ad:package-quickstart-refresh--optimize #'elpaish-install--package-quickstart-refresh-optimize)
(defalias 'bootstrap-sync-package-load-paths #'elpaish-install-sync-package-load-paths)
(defalias 'bootstrap-package-quickstart-stale-p #'elpaish-install-package-quickstart-stale-p)
(defalias 'bootstrap-refresh-package-quickstart #'elpaish-install-refresh-package-quickstart)
(defalias 'bootstrap-force-refresh-package-quickstart #'elpaish-install-refresh-package-quickstart)
(defalias 'bootstrap-flush-package-quickstart #'elpaish-install-flush-package-quickstart)
(defalias 'ad:refresh-package-quickstart #'elpaish-install-schedule-quickstart-refresh)
(defalias 'bootstrap-package-quickstart (lambda () (elpaish-install-refresh-package-quickstart nil)))

(provide 'elpaish-install)

;; Local Variables:
;; package-lint-main-file: "pkg/elpaish.el"
;; End:

;;; elpaish-install.el ends here
