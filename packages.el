;;; packages.el --- Package definitions for ELPAish archive -*- lexical-binding: t; -*-

;; Pure package recipe definitions for the ELPAish repository.
;; Loaded at build time by elpaish build runners or via (elpaish-load-packages).

(require 'elpaish)
(require 'elpaish-recipes)

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
 :preflight-skip '(byte-compile ert package-lint)
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
          "magit-dash-agent.el"
          "magit-dash-gh.el"
          "magit-dash-git.el"
          "magit-dash-layout.el"
          "magit-dash-menu.el"
          "magit-dash-process.el"
          "magit-dash-worktree.el")
 :test-dir "test"
 :preflight-skip '(ert)
 :summary "Status and management dashboard for Magit repositories and worktrees"
 :url "https://github.com/tychoish/magit-dash"
 :keywords '("tools" "vc" "git"))

;; 4. sprite
(elpaish-register-package
 'sprite
 (elpaish-recipe-path "sprite"
                      "https://github.com/tychoish/sprite.git")
 :branch "main"
 :files '("sprite.el"
          "sprite-registry.el"
          "sprite-monitor.el"
          "sprite-manager.el"
          "sprite-inspector.el"
          "sprite-hud.el"
          "sprite-history.el"
          "sprite-filter.el"
          "sprite-dashboard.el"
          "sprite-custom.el")
 :test-dir "test"
 :preflight-skip '(ert)
 :summary "System process management, telemetry, and supervision interface"
 :url "https://github.com/tychoish/sprite"
 :keywords '("processes" "tools"))

;; 5. agent-shell-notifications
(elpaish-register-package
 'agent-shell-notifications
 (elpaish-recipe-path "agent-shell-notifications"
                      "https://github.com/zackattackz/agent-shell-notifications.git")
 :branch "main"
 :files '("agent-shell-notifications.el"
          "agent-shell-notifications-sound.el")
 :test-dir "test"
 :preflight-skip '(byte-compile ert package-lint)
 :summary "Audio and desktop alerts for long-running AI agent tasks"
 :url "https://github.com/zackattackz/agent-shell-notifications"
 :keywords '("tools" "notifications"))

;; 6. xtdlib
(elpaish-register-package
 'xtdlib
 (elpaish-recipe-path "xtdlib"
                      "https://github.com/tychoish/xtdlib.el.git")
 :branch "main"
 :files '("xtdlib.el"
          "xtd-s.el"
          "xtd-f.el"
          "xtd-ht.el"
          "xtd-seq.el"
          "xtd-sys.el")
 :test-dir "test"
 :summary "Comprehensive standard library extension for Emacs Lisp"
 :url "https://github.com/tychoish/xtdlib.el"
 :keywords '("extensions" "lisp"))

;; 7. elpaish-keyring
(elpaish-register-package
 'elpaish-keyring
 (elpaish-recipe-path "elpaish"
                      "https://github.com/tychoish/elpaish.git")
 :source-dir "pkg"
 :branch "main"
 :files '("elpaish-keyring.el" "elpaish.pub.asc")
 :summary "GPG Keyring and trust anchors for ELPAish package archives"
 :url "https://github.com/tychoish/elpaish"
 :keywords '("package" "security" "maintenance" "elpa"))

;; 8. elpaish (self-hosting)
(elpaish-register-package
 'elpaish
 (elpaish-recipe-path "elpaish"
                      "https://github.com/tychoish/elpaish.git")
 :source-dir "pkg"
 :branch "main"
 :files '("elpaish.el"
          "elpaish-recipes.el"
          "elpaish-check.el"
          "elpaish-signing-keys.el"
          "elpaish-keyring.el")
 :test-dir "test"
 :summary "Multi-track signed ELPA package archive builder and server"
 :url "https://github.com/tychoish/elpaish"
 :keywords '("tools" "elpa" "package" "distribution"))

(provide 'packages)
;;; packages.el ends here
