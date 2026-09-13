;;; packages.el --- Package definitions for ELPAish archive -*- lexical-binding: t; -*-

;; Pure package recipe definitions for the ELPAish repository.
;; Loaded at build time by elpaish build runners or via (elpaish-load-packages).

(require 'elpaish)
(require 'elpaish-recipes)

(elpaish-register-package
 'annotated-completing-read
 (elpaish-recipe-path "https://github.com/tychoish/annotated-completing-read.git")
 :branch "main"
 :files '("annotated-completing-read.el")
 :test-dir "test"
 :summary "Annotated completing-read interface with aligned annotations"
 :url "https://github.com/tychoish/annotated-completing-read"
 :keywords '("convenience" "completion" "matching"))

(elpaish-register-package
 'agent-shell-queue
 (elpaish-recipe-path "https://github.com/tychoish/agent-shell-queue.git")
 :branch "main"
 :test-dir "test"
 :summary "Emacs queue manager for AI agent tasks"
 :url "https://github.com/tychoish/agent-shell-queue"
 :keywords '("tools" "agent-shell"))

(elpaish-register-package
 'agent-shell-notifications
 (elpaish-recipe-path "https://github.com/zackattackz/agent-shell-notifications.git")
 :external t
 :preflight-skip t ;; it's external -- yolo
 :branch "main"
 :files '("agent-shell-notifications.el"
	  "agent-shell-notifications-knockknock.el"
	  "agent-shell-notifications-libnotify.el")
 :summary "Libnotify notifications for agent-shell"
 :url "https://github.com/zackattackz/agent-shell-notifications"
 :keywords '("tools" "agent-shell"))

(elpaish-register-package
 'ollama
 (elpaish-recipe-path "https://github.com/tychoish/ollama.el.git")
 :fork t
 :preflight-skip t ;; it's a fork -- yolo
 :branch "patch-1"
 :files '("ollama.el"
	  "ollama-api.el"
	  "ollama-status.el"
	  "ollama-transient.el"
	  "ollama-utils.el")
 :summary "Manage Ollama models from Emacs"
 :url "https://github.com/tychoish/ollama.el"
 :keywords '("tools" "ai" "ollama"))

(elpaish-register-package
 'magit-dash
 (elpaish-recipe-path "https://github.com/tychoish/magit-dash.git")
 :branch "main"
 :test-dir "test"
 :requires '(magit projectile)
 :summary "Status and management dashboard for Magit repositories and worktrees"
 :url "https://github.com/tychoish/magit-dash"
 :keywords '("tools" "vc" "git"))

(elpaish-register-package
 'sprite
 (elpaish-recipe-path "https://github.com/tychoish/sprite.git")
 :branch "main"
 :test-dir "test"
 :summary "System process management, telemetry, and supervision interface"
 :url "https://github.com/tychoish/sprite"
 :keywords '("processes" "tools"))

(elpaish-register-package
 'xtdlib
 (elpaish-recipe-path "https://github.com/tychoish/xtdlib.el.git")
 :branch "main"
 :test-dir "test"
 :preflight-skip '(package-lint)
 :summary "Comprehensive standard library extension for Emacs Lisp"
 :url "https://github.com/tychoish/xtdlib.el"
 :keywords '("extensions" "lisp"))

(elpaish-register-package
 'elpaish-keyring
 (elpaish-recipe-path "https://github.com/tychoish/elpaish.git")
 :source-dir "pkg"
 :branch "main"
 :files '("elpaish.pub.asc" "elpaish-keyring-pkg.el")
 :preflight-skip t
 :summary "GPG Keyring and trust anchors for ELPAish package archives"
 :url "https://github.com/tychoish/elpaish"
 :keywords '("package" "security" "maintenance" "elpa"))

(elpaish-register-package
 'elpaish
 (elpaish-recipe-path "https://github.com/tychoish/elpaish.git")
 :source-dir "pkg"
 :branch "main"
 :test-dir "test"
 :exclude-files '("elpaish-keyring-pkg.el")
 :summary "Multi-stream signed ELPA package archive builder and server"
 :url "https://github.com/tychoish/elpaish"
 :keywords '("tools" "elpa" "package" "distribution"))

(elpaish-register-package
 'telega-bot
 (elpaish-recipe-path "https://github.com/tychoish/telega-bot.git")
 :branch "main"
 :test-dir "test"
 :requires '(telega alert)
 :summary "High-level event-driven Telegram Bot engine with Telega"
 :url "https://github.com/tychoish/telega-bot"
 :keywords '("comm" "tools" "telegram"))

(elpaish-register-package
 'ollama-tailnet
 (elpaish-recipe-path "https://github.com/tychoish/ollama-tailnet.git")
 :branch "main"
 :test-dir "test"
 :requires '(gptel annotated-completing-read transient)
 :summary "Ollama tailnet orchestration, gptel integration, and model dashboard in Emacs"
 :url "https://github.com/tychoish/ollama-tailnet"
 :keywords '("hypermedia" "tools" "ai" "tailscale" "systemd" "gptel"))

(elpaish-register-package
 'arch
 (elpaish-recipe-path "https://github.com/tychoish/arch.el.git")
 :branch "main"
 :test-dir "test"
 :requires '(transient annotated-completing-read yaml compat)
 :summary "Arch Linux package management UI, package sets, and ELPA manager in Emacs"
 :url "https://github.com/tychoish/arch.el"
 :keywords '("tools" "package" "arch" "elpa"))

(elpaish-register-package
 'mcpkit
 (elpaish-recipe-path "https://github.com/tychoish/mcpkit.el.git")
 :branch "main"
 :test-dir "test"
 :requires '(web-server compat)
 :summary "Model Context Protocol (MCP) service framework"
 :url "https://github.com/tychoish/mcpkit.el"
 :keywords '("comm" "tools" "mcp" "rpc"))

(elpaish-register-package
 'tailscale
 (elpaish-recipe-path "https://github.com/tychoish/tailscale.git")
 :branch "main"
 :test-dir "test"
 :summary "Tailscale CLI integration and network control"
 :url "https://github.com/tychoish/tailscale"
 :keywords '("comm" "network" "tools"))

(elpaish-register-package
 'org-docsgen
 (elpaish-recipe-path "https://github.com/tychoish/org-docsgen.git")
 :branch "main"
 :test-dir "test"
 :requires '(xtdlib compat)
 :summary "Generate org API documentation from Emacs Lisp source"
 :url "https://github.com/tychoish/org-docsgen"
 :keywords '("docs" "lisp" "org" "tools"))

(provide 'packages)
;;; packages.el ends here
