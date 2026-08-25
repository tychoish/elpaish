;;; packages.el --- Package definitions for ELPAish archive -*- lexical-binding: t; -*-

;; Pure package recipe definitions for the ELPAish repository.
;; Loaded at build time by elpaish build runners or via (elpaish-load-packages).

(require 'elpaish)
(require 'elpaish-recipes)

(elpaish-register-package
 'annotated-completing-read
 (elpaish-recipe-path "annotated-completing-read"
                      "https://github.com/tychoish/annotated-completing-read.git")
 :branch "main"
 :files '("annotated-completing-read.el")
 :preflight-skip '(ert)
 :test-dir "test"
 :summary "Annotated completing-read interface with aligned annotations"
 :url "https://github.com/tychoish/annotated-completing-read"
 :keywords '("convenience" "completion" "matching"))

(elpaish-register-package
 'agent-shell-queue
 (elpaish-recipe-path "agent-shell-queue"
                      "https://github.com/tychoish/agent-shell-queue.git")
 :branch "main"
 :test-dir "test"
 :preflight-skip '(byte-compile ert package-lint)
 :summary "Emacs queue manager for AI agent tasks"
 :url "https://github.com/tychoish/agent-shell-queue"
 :keywords '("tools" "agent-shell"))

(elpaish-register-package
 'agent-shell-notifications
 (elpaish-recipe-path "agent-shell-notifications"
                      "https://github.com/zackattackz/agent-shell-notifications.git")
 :branch "main"
 :files '("agent-shell-notifications.el"
	  "agent-shell-notifications-knockknock.el"
	  "agent-shell-notifications-libnotify.el")
 :preflight-skip '(byte-compile ert package-lint)
 :summary "Libnotify notifications for agent-shell"
 :url "https://github.com/zackattackz/agent-shell-notifications"
 :keywords '("tools" "agent-shell"))

(elpaish-register-package
 'magit-dash
 (elpaish-recipe-path "magit-dash"
                      "https://github.com/tychoish/magit-dash.git")
 :branch "main"
 :test-dir "test"
 :summary "Status and management dashboard for Magit repositories and worktrees"
 :url "https://github.com/tychoish/magit-dash"
 :keywords '("tools" "vc" "git"))

(elpaish-register-package
 'sprite
 (elpaish-recipe-path "sprite"
                      "https://github.com/tychoish/sprite.git")
 :branch "main"
 :test-dir "test"
 :preflight-skip '(ert package-lint)
 :summary "System process management, telemetry, and supervision interface"
 :url "https://github.com/tychoish/sprite"
 :keywords '("processes" "tools"))

(elpaish-register-package
 'xtdlib
 (elpaish-recipe-path "xtdlib"
                      "https://github.com/tychoish/xtdlib.el.git")
 :branch "main"
 :test-dir "test"
 :preflight-skip '(ert package-lint)
 :summary "Comprehensive standard library extension for Emacs Lisp"
 :url "https://github.com/tychoish/xtdlib.el"
 :keywords '("extensions" "lisp"))

(elpaish-register-package
 'elpaish-keyring
 (elpaish-recipe-path "elpaish"
                      "https://github.com/tychoish/elpaish.git")
 :source-dir "pkg"
 :branch "main"
 :files '("elpaish.pub.asc" "elpaish-keyring-pkg.el")
 :preflight-skip t
 :summary "GPG Keyring and trust anchors for ELPAish package archives"
 :url "https://github.com/tychoish/elpaish"
 :keywords '("package" "security" "maintenance" "elpa"))

(elpaish-register-package
 'elpaish
 (elpaish-recipe-path
  "elpaish"
  "https://github.com/tychoish/elpaish.git")
 :source-dir "pkg"
 :branch "main"
 :test-dir "test"
 :exclude-files '("elpaish-keyring-pkg.el")
 :summary "Multi-track signed ELPA package archive builder and server"
 :url "https://github.com/tychoish/elpaish"
 :keywords '("tools" "elpa" "package" "distribution"))

(provide 'packages)
;;; packages.el ends here
