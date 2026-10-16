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
(declare-function elpaish--recipe-source-path "elpaish" (recipe))
(declare-function sprite-future-then "sprite" (future callback))
(declare-function gen-run-async "gen" (fn sequence &rest args))
(declare-function package--reload-previously-loaded "package" (pkg-desc &optional warn))

(defgroup elpaish-install nil
  "Package bootstrapping and upgrade utilities."
  :group 'package
  :prefix "elpaish-install-")

(define-obsolete-variable-alias
  'elpaish-install-bootstrap-packages
  'elpaish-install-bootstrap-packages-list
  "0.2.0")

(defcustom elpaish-install-bootstrap-packages-list
  '(compat web-server htmlize annotated-completing-read transient package-lint)
  "List of package symbols to install or upgrade during bootstrapping.
Used as default target package list for `elpaish-install-bootstrap-packages'
when no explicit packages are specified."
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

(defvar elpaish-install--active-cancel-handle nil
  "Cancellation handle function for active async package installation.")

;;;###autoload
(defun elpaish-install-register-bootstrap-package (&rest pkgs)
  "Append unique package symbols from PKGS to bootstrap package list.
PKGS can be package symbols or lists of package symbols.  Updates
`elpaish-install-bootstrap-packages-list' and returns it."
  (interactive)
  (let ((new-pkgs nil))
    (dolist (p (flatten-tree pkgs))
      (when (and (symbolp p)
                 p
                 (not (memq p elpaish-install-bootstrap-packages-list))
                 (not (memq p new-pkgs)))
        (push p new-pkgs)))
    (when new-pkgs
      (setq elpaish-install-bootstrap-packages-list
            (append elpaish-install-bootstrap-packages-list (nreverse new-pkgs)))))
  elpaish-install-bootstrap-packages-list)

;;;###autoload
(defalias 'elpaish-install-register-bootstrap-packages #'elpaish-install-register-bootstrap-package)

;;;###autoload
(define-obsolete-function-alias
  'elpaish-install-add-bootstrap-packages
  #'elpaish-install-register-bootstrap-package
  "0.2.0")

(defun elpaish-install--extract-header-requires (file)
  "Extract package requirements list from FILE header using `package-buffer-info'."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let* ((pkg-info (condition-case nil (package-buffer-info) (error nil))))
        (package-desc-reqs pkg-info)))))

;;;###autoload
(defun elpaish-install-ensure-package-dependencies (dir-or-recipe)
  "Ensure dependencies declared in Package-Requires of DIR-OR-RECIPE are installed.
DIR-OR-RECIPE can be an `elpaish-recipe' struct or a directory string containing
package sources.  Extracts dependencies from package header and invokes
`package-install' for any uninstalled dependency."
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
                 (let* ((source-dir (if recipe
                                        (elpaish--recipe-source-path recipe)
                                      (file-name-as-directory (expand-file-name dir-or-recipe))))
                        (name (if recipe (elpaish-recipe-name recipe) (file-name-nondirectory (directory-file-name source-dir))))
                        (main-file (expand-file-name (format "%s.el" name) source-dir)))
                   (elpaish-install--extract-header-requires main-file))))))
    (when reqs
      (let ((missing nil))
        (dolist (req reqs)
          (let ((dep-pkg (car req)))
            (unless (or (eq dep-pkg 'emacs) (package-installed-p dep-pkg))
              (push dep-pkg missing))))
        (when missing
          (message "Refreshing package archive contents to install missing dependencies: %S" missing)
          (package-refresh-contents)
          (dolist (dep-pkg missing)
            (message "Installing missing package dependency: %s" dep-pkg)
            (package-install dep-pkg)))))))

;;;; Core Single Package Operation

(defun elpaish-install--process-package (pkg &optional action refresh)
  "Perform single package PKG operation for ACTION (\\='upgrade or \\='install).
ACTION defaults to \\='upgrade, which installs PKG if missing or upgrades it
if an update is available.  If REFRESH is non-nil, refresh archive contents."
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
             (message "Package %s is already installed." pkg)
             (list :status :already-installed :pkg pkg))
            (t
             (message "Installing package %s..." pkg)
             (package-install pkg)
             (when (fboundp 'package--reload-previously-loaded)
               (when-let* ((desc (cadr (assq pkg package-alist))))
                 (condition-case nil
                     (package--reload-previously-loaded desc)
                   (error nil))))
             (message "Successfully installed %s." pkg)
             (list :status :installed :pkg pkg))))
          (_
           (cond
            ((not (package-installed-p pkg))
             (message "Package %s is not installed; installing..." pkg)
             (package-install pkg)
             (when (fboundp 'package--reload-previously-loaded)
               (when-let* ((desc (cadr (assq pkg package-alist))))
                 (condition-case nil
                     (package--reload-previously-loaded desc)
                   (error nil))))
             (message "Successfully installed %s." pkg)
             (list :status :installed :pkg pkg))
            ((fboundp 'package-upgrade)
             (condition-case sub-err
                 (progn
                   (package-upgrade pkg)
                   (message "Successfully upgraded %s." pkg)
                   (list :status :upgraded :pkg pkg))
               (user-error
                (message "Package %s is up to date." pkg)
                (list :status :up-to-date :pkg pkg))
               (error
                (signal (car sub-err) (cdr sub-err)))))
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
                 (message "Successfully upgraded %s from %s to %s."
                          pkg (package-version-join inst-ver) (package-version-join arch-ver))
                 (list :status :upgraded :pkg pkg))
                (t
                 (message "Package %s is up to date." pkg)
                 (list :status :up-to-date :pkg pkg))))))))
      (error
       (message "Failed to %s %s: %s" act pkg (error-message-string err))
       (list :status :failed :pkg pkg :error (error-message-string err))))))

;;;; Summary and Completion Helpers

(defun elpaish-install--report-summary (res-list action)
  "Display summary message for RES-LIST and ACTION."
  (let ((installed 0) (upgraded 0) (up-to-date 0) (failed 0))
    (dolist (res res-list)
      (pcase (plist-get res :status)
        (':installed (cl-incf installed))
        (':upgraded  (cl-incf upgraded))
        (':up-to-date (cl-incf up-to-date))
        (':already-installed (cl-incf up-to-date))
        (':failed    (cl-incf failed))))
    (cond
     ((eq action 'install)
      (message "Package installation complete: %d installed, %d failed."
               installed failed))
     (t
      (message "Package bootstrap complete: %d installed, %d upgraded, %d up to date, %d failed."
               installed upgraded up-to-date failed)))))

(defun elpaish-install--remote-eval-form (pkg action parent-user-dir parent-archives refresh)
  "Generate Lisp form for remote sprite execution of PKG with ACTION.
ACTION is \\='install or \\='upgrade.
PARENT-USER-DIR and PARENT-ARCHIVES configure the remote package environment.
If REFRESH is non-nil, refreshes package contents remotely."
  `(progn
     (require 'package)
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
          (error nil)))))
  (elpaish-install--report-summary res-list action)
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
        (message "Refreshing package archive contents...")
        (package-refresh-contents))
      (let ((results nil))
        (dolist (pkg target-pkgs)
          (push (elpaish-install--process-package pkg action nil) results))
        (let ((final-res (nreverse results)))
          (elpaish-install--report-summary final-res action)
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
        (message "%s %d package(s) asynchronously using sprite pool..."
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
        (message "Refreshing package archive contents...")
        (package-refresh-contents))
      (let* ((results nil)
             (total (length target-pkgs)))
        (message "%s %d package(s) asynchronously..."
                 (if (eq action 'install) "Installing" "Bootstrapping")
                 total)
        (let ((runner
               (gen-run-async
                (lambda (pkg done-cb)
                  (let ((res (elpaish-install--process-package pkg action nil)))
                    (push res results)
                    (funcall done-cb)))
                target-pkgs
                :concurrency (or concurrency elpaish-install-concurrency 1)
                :delay (or delay elpaish-install-yield-delay 0.0)
                :on-done
                (lambda ()
                  (setq elpaish-install--active-cancel-handle nil)
                  (let ((final-res (nreverse results)))
                    (elpaish-install--report-summary final-res action)
                    (when callback
                      (funcall callback final-res))))
                :on-error
                (lambda (err)
                  (message "Error during package bootstrapping: %S" err)))))
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
  (when (functionp elpaish-install--active-cancel-handle)
    (ignore-errors (funcall elpaish-install--active-cancel-handle :cancel)))
  (setq elpaish-install--active-cancel-handle nil))

;;;; Public Commands and Collapsed Dispatcher

;;;###autoload
(cl-defun elpaish-install-bootstrap-packages (&rest args)
  "Install or upgrade bootstrap packages with non-blocking cooperative yielding.
Packages are installed if missing, or upgraded if newer versions exist.
By default, execution is asynchronous and yielding using `gen.el'.
When called interactively with a prefix argument (e.g. \\[universal-argument]),
execution is synchronous.

ARGS can contain package symbols, lists of package symbols, or keyword options:
  :sync BOOL        - When non-nil, execute synchronously.
  :async BOOL       - When non-nil (default t), execute asynchronously.
  :refresh BOOL     - When non-nil (default t), refresh package archives.
  :action ACTION    - Operation action: \\='upgrade (default) or \\='install.
  :concurrency N    - Concurrent jobs for `gen-run-async' (default 1).
  :delay SECS       - Yield delay in seconds
                      (default `elpaish-install-yield-delay').
  :callback FN      - Function called with result list on completion.
  :pool-size N      - When specified and sprite is available, use sprite pool.

When no packages are specified in ARGS, defaults to
`elpaish-install-bootstrap-packages-list'."
  (interactive "P")
  (let* ((interactive-p (called-interactively-p 'any))
         (prefix-arg-val (when interactive-p (car args)))
         (remaining (if interactive-p nil args))
         (pkgs nil)
         (refresh t)
         (sync (not (null prefix-arg-val)))
         (async :default)
         (action 'upgrade)
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
         ((eq arg :action)     (setq action (pop remaining)))
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
                            elpaish-install-bootstrap-packages-list))
           (is-sync (or sync (eq async nil))))
      (cond
       (is-sync
        (let ((res (elpaish-install--do-packages target-pkgs :action action :refresh refresh)))
          (when callback (funcall callback res))
          res))
       (t
        (elpaish-install--do-packages-async
         target-pkgs
         :action action
         :refresh refresh
         :concurrency concurrency
         :delay delay
         :pool-size pool-size
         :callback callback))))))

;;;###autoload
(defalias 'elpaish-install-packages #'elpaish-install-bootstrap-packages)

;;;###autoload
(defalias 'elpaish-install-upgrade-packages #'elpaish-install-bootstrap-packages)

;;;###autoload
(defalias 'elpaish-upgrade-packages #'elpaish-install-bootstrap-packages)

;;;###autoload
(cl-defun elpaish-install-packages-async (pkgs &key refresh pool-size callback)
  "Install PKGS asynchronously out-of-process using a sprite pool if available.
If sprite is not available, falls back to cooperative async execution.
Calls CALLBACK when complete."
  (elpaish-install-bootstrap-packages pkgs :action 'install :refresh refresh :pool-size (or pool-size 2) :callback callback :async t))

;;;###autoload
(cl-defun elpaish-install-upgrade-packages-async (pkgs &key pool-size callback)
  "Upgrade PKGS asynchronously out-of-process using a sprite pool if available.
If sprite is not available, falls back to cooperative async execution.
Calls CALLBACK when complete."
  (elpaish-install-bootstrap-packages pkgs :action 'upgrade :refresh t :pool-size (or pool-size 2) :callback callback :async t))

(provide 'elpaish-install)

;; Local Variables:
;; package-lint-main-file: "pkg/elpaish.el"
;; End:

;;; elpaish-install.el ends here
