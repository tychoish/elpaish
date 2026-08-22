;;; elpaish-recipes.el --- Package recipes for the Tychoish ecosystem -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maintenance, tools, elpa, package

;;; Commentary:
;; Declarative package recipes for packages maintained across the tychoish
;; ecosystem.  Provides automatic local-or-remote path resolution so builds
;; execute seamlessly on the developer workstation or in clean CI runners.

;;; Code:

(require 'elpaish)

(defcustom elpaish-recipe-local-search-dirs '("~/src/")
  "Local checkout root directories searched by `elpaish-recipe-path'.
Each recipe's bare directory NAME is looked up under every root, in
order (after which \"external/NAME\" is tried under
`user-emacs-directory' and under `default-directory') — the first
existing directory wins. Customize this instead of hardcoding a
maintainer's personal directory layout into individual recipes."
  :type '(repeat directory)
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
                    (let ((candidate (expand-file-name name root)))
                      (and (file-directory-p candidate) candidate)))
                  roots)
        remote-url)))

;;;###autoload
(defun elpaish-recipes-register-all ()
  "Register all Tychoish ecosystem package recipes in `elpaish-registry'."
  (interactive)

  ;; 1. annotated-completing-read
  (elpaish-register-package
   'annotated-completing-read
   (elpaish-recipe-path "annotated-completing-read"
                        "https://github.com/tychoish/annotated-completing-read.git")
   :branch "main"
   :files '("annotated-completing-read.el")
   :test-dir "test"
   :summary "Annotated completing-read interface with aligned annotations"
   :url "https://github.com/tychoish/annotated-completing-read"
   :keywords '("convenience" "completion" "matching"))

  ;; 2. agent-shell-queue
  (elpaish-register-package
   'agent-shell-queue
   (elpaish-recipe-path "agent-shell-queue"
                        "https://github.com/tychoish/agent-shell-queue.git")
   :branch "main"
   :files '("agent-shell-queue.el"
            "agent-shell-queue-org.el"
            "agent-shell-queue-db.el"
            "agent-shell-queue-persistence.el"
            "agent-shell-menu.el")
   :test-dir "test"
   :preflight-skip '(ert)
   :summary "Emacs queue manager for AI agent tasks"
   :url "https://github.com/tychoish/agent-shell-queue"
   :keywords '("tools" "convenience"))

  ;; 3. magit-dash
  (elpaish-register-package
   'magit-dash
   (elpaish-recipe-path "magit-dash"
                        "https://github.com/tychoish/magit-dash.git")
   :branch "main"
   :files '("magit-dash.el"
            "magit-dash-gh.el"
            "magit-dash-gh-pr.el"
            "magit-dash-gh-actions.el"
            "magit-dash-gh-ci.el"
            "magit-dash-open.el"
            "magit-dash-submodules.el"
            "magit-dash-timer.el")
   :test-dir "test"
   :summary "Personal multi-repository dashboard for Magit and GitHub"
   :url "https://github.com/tychoish/magit-dash"
   :keywords '("tools" "vc" "git"))

  ;; 4. sprite
  (elpaish-register-package
   'sprite
   (elpaish-recipe-path "sprite"
                        "https://github.com/tychoish/sprite.git")
   :branch "main"
   :files '("sprite.el"
            "sprite-direct.el"
            "sprite-fleet.el"
            "sprite-future.el"
            "sprite-heartbeat.el"
            "sprite-list.el"
            "sprite-session.el")
   :test-dir "test"
   :preflight-skip '(ert)
   :summary "Fast ephemeral Emacs child-daemon manager"
   :url "https://github.com/tychoish/sprite"
   :keywords '("processes" "tools"))

  ;; 5. agent-shell-notifications
  (elpaish-register-package
   'agent-shell-notifications
   (elpaish-recipe-path "agent-shell-notifications"
                        "https://github.com/zackattackz/agent-shell-notifications.git")
   :branch "main"
   :files '("agent-shell-notifications.el"
            "agent-shell-notifications-knockknock.el"
            "agent-shell-notifications-libnotify.el")
   :preflight-skip '(byte-compile)
   :summary "Notification routing for agent shell sessions"
   :url "https://github.com/zackattackz/agent-shell-notifications"
   :keywords '("tools" "notifications"))

  ;; 6. xtdlib
  (elpaish-register-package
   'xtdlib
   (elpaish-recipe-path "xtdlib"
                        "https://github.com/tychoish/xtdlib.el")
   :branch "main"
   :files '("xtdlib.el"
            "xtd-dash.el"
            "xtd-f.el"
            "xtd-ht.el"
            "xtd-macro.el"
            "xtd-project.el"
            "xtd-s.el")
   :summary "Extended standard library and macros for Emacs Lisp"
   :url "https://github.com/tychoish/xtdlib"
   :keywords '("extensions" "lisp"))

  ;; 7. xlib
  (elpaish-register-package
   'xlib
   (elpaish-recipe-path "xlib.el"
                        "https://github.com/tychoish/xlib.el.git")
   :branch "main"
   :files '("xlib.el")
   :test-dir "test"
   :summary "Extended elisp utility library"
   :url "https://github.com/tychoish/xlib.el"
   :keywords '("extensions" "utility"))

  ;; 8. elpaish-keyring
  (elpaish-register-package
   'elpaish-keyring
   (elpaish-recipe-path "elpaish"
                        "https://github.com/tychoish/elpaish.git")
   :branch "main"
   :source-dir "pkg"
   :files '("elpaish-keyring.el")
   :preflight-skip t
   :summary "GPG keyring and trust anchors for ELPAish package archives"
   :url "https://github.com/tychoish/elpaish"
   :keywords '("package" "security" "maintenance" "elpa"))

  ;; 9. elpaish (self-hosting)
  (elpaish-register-package
   'elpaish
   (elpaish-recipe-path "elpaish"
                        "https://github.com/tychoish/elpaish.git")
   :branch "main"
   :source-dir "pkg"
   :files '("elpaish.el"
            "elpaish-recipes.el"
            "elpaish-keyring.el"
            "elpaish-signing-keys.el")
   :test-dir "test"
   :summary "Multi-track signed ELPA package archive builder and server"
   :url "https://github.com/tychoish/elpaish"
   :keywords '("tools" "elpa" "package" "distribution"))

  (message "Registered %d ELPAish recipes." (hash-table-count elpaish-registry)))

;; Automatically register recipes when loaded
(elpaish-recipes-register-all)

(provide 'elpaish-recipes)
;;; elpaish-recipes.el ends here
