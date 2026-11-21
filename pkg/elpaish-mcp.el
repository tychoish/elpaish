;;; elpaish-mcp.el --- MCP tools for managing ELPAish -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa, mcp
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Registers the `elpaish' MCP service, over `elpaish-ops':
;;
;; - `elpaish_status': per-recipe commit delta against the last build.
;; - `elpaish_rebuild': rebuild archives (one, all, or only those behind).
;; - `elpaish_upgrade': upgrade installed packages (one, several or all).
;; - `elpaish_workflow_dispatch': dispatch the repository's GitHub workflow.
;; - `elpaish_recipe': format an `elpaish-register-package' form.
;;
;; Building, linting, testing and compiling a package are `builder' tools
;; (`elisp_preflight', `builder_run_tests', `builder_package_build'); this
;; service only covers what is specific to ELPAish and to installed packages.
;;
;; Tools register when this file loads.  Start the service with
;; `mcpkit-start-service' or `elpaish-mcp-start'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'mcpkit)
(require 'elpaish-ops)

(defgroup elpaish-mcp nil
  "ELPAish MCP service."
  :group 'mcpkit
  :prefix "elpaish-mcp-")

(defcustom elpaish-mcp-port 8768
  "Default TCP port for the elpaish MCP service."
  :type 'integer
  :group 'elpaish-mcp)

(defvar elpaish-mcp-service
  (or (mcpkit-get-service 'elpaish)
      (mcpkit-define-service 'elpaish
        :port elpaish-mcp-port
        :description "ELPAish status, rebuild and package upgrade tools"))
  "The `elpaish' `mcpkit-service' instance.")

(defun elpaish-mcp--list (value)
  "Return JSON array VALUE (vector or list) as a list, or nil."
  (cond ((vectorp value) (append value nil))
        ((listp value) value)))

(defun elpaish-mcp--true (args key)
  "Return non-nil when ARGS has boolean KEY set to JSON true."
  (eq (plist-get args key) t))

(mcpkit-register-tool 'elpaish_status 'elpaish
  :description "Report, per registered ELPAish recipe, how many commits its git HEAD is ahead of the last build (delta, behind). Local only: never clones or fetches, so a recipe without a checkout is reported as uncloned. Use behind_only to list just the packages needing a rebuild."
  :input-schema '(:type "object"
                  :properties (:packages (:type "array" :items (:type "string")
                                          :description "Limit the report to these package names")
                               :behind_only (:type "boolean"
                                             :description "Return only packages behind git HEAD"))
                  :required [])
  (elpaish-ops-status :packages (elpaish-mcp--list (plist-get args :packages))
                      :behind-only (elpaish-mcp--true args :behind_only)))

(mcpkit-register-tool 'elpaish_rebuild 'elpaish
  :description "Rebuild ELPAish package archives locally (archive-contents, checksums, signatures). Rebuilds one package, only the packages behind git HEAD, or everything when neither package nor behind_only is given."
  :input-schema '(:type "object"
                  :properties (:package (:type "string" :description "Package to rebuild; omit or \"all\" for every package")
                               :stream (:type "string" :enum ["snapshot" "stable" "staging" "all"]
                                        :description "Release stream (default snapshot)")
                               :behind_only (:type "boolean"
                                             :description "Rebuild only packages that have commits ahead of the archive"))
                  :required [])
  (elpaish-ops-rebuild :package (plist-get args :package)
                       :stream (intern (or (plist-get args :stream) "snapshot"))
                       :behind-only (elpaish-mcp--true args :behind_only)))

(mcpkit-register-tool 'elpaish_upgrade 'elpaish
  :description "Upgrade installed Emacs packages. Name them in packages, or set all to upgrade everything with a newer archive version; archives are refreshed at most once and only packages with newer versions are touched. reinstall forces a reinstall of the named packages. async hands the work to a background pool and returns immediately."
  :input-schema '(:type "object"
                  :properties (:packages (:type "array" :items (:type "string")
                                          :description "Packages to upgrade")
                               :all (:type "boolean" :description "Upgrade every upgradable package (default when packages is omitted)")
                               :refresh (:type "boolean" :description "Refresh package archives first")
                               :reinstall (:type "boolean" :description "Reinstall the named packages even if current")
                               :async (:type "boolean" :description "Run in a background worker pool and return immediately")
                               :pool_size (:type "integer" :description "Worker pool size for async (default 4)"))
                  :required [])
  (let ((packages (elpaish-mcp--list (plist-get args :packages))))
    (elpaish-ops-upgrade :packages packages
                         :all (or (elpaish-mcp--true args :all) (null packages))
                         :refresh (elpaish-mcp--true args :refresh)
                         :reinstall (elpaish-mcp--true args :reinstall)
                         :async (elpaish-mcp--true args :async)
                         :pool-size (plist-get args :pool_size))))

(mcpkit-register-tool 'elpaish_workflow_dispatch 'elpaish
  :description "Dispatch a GitHub Actions workflow of the ELPAish repository (default pages.yml on elpaish-github-repo-slug), via magit-dash or the gh CLI."
  :input-schema '(:type "object"
                  :properties (:workflow (:type "string" :description "Workflow filename or name (default pages.yml)")
                               :repo (:type "string" :description "owner/repo slug (default the ELPAish repository)")
                               :ref (:type "string" :description "Git ref (default main)"))
                  :required [])
  (elpaish-ops-workflow-run (plist-get args :workflow)
                            (plist-get args :repo)
                            (plist-get args :ref)))

(mcpkit-register-tool 'elpaish_recipe 'elpaish
  :description "Format the elpaish-register-package form that registers a package, for pasting into packages.el."
  :input-schema '(:type "object"
                  :properties (:package (:type "string" :description "Package name")
                               :summary (:type "string" :description "One-line description")
                               :requires (:type "array" :items (:type "string")
                                          :description "Package dependencies")
                               :keywords (:type "array" :items (:type "string")
                                          :description "Package keywords")
                               :url (:type "string" :description "Repository URL (default GitHub owner/package)")
                               :owner (:type "string" :description "GitHub owner for the default URL"))
                  :required ["package"])
  (let ((pkg (plist-get args :package)))
    (unless (and (stringp pkg) (not (string-empty-p pkg)))
      (user-error "Package is required"))
    (list :snippet (elpaish-ops-recipe-snippet
                    pkg
                    :summary (plist-get args :summary)
                    :requires (mapcar #'intern (elpaish-mcp--list (plist-get args :requires)))
                    :keywords (elpaish-mcp--list (plist-get args :keywords))
                    :url (plist-get args :url)
                    :owner (plist-get args :owner)))))

;;;###autoload
(defun elpaish-mcp-register ()
  "Ensure `elpaish-mcp-service' is registered in `mcpkit-registry' and return it."
  (interactive)
  (unless (mcpkit-get-service 'elpaish)
    (mcpkit-register-service elpaish-mcp-service))
  elpaish-mcp-service)

;;;###autoload
(defun elpaish-mcp-start (&optional port)
  "Start or merge the `elpaish' MCP service on PORT.
When PORT is nil and a shared server is already active, attach to it;
otherwise fall back to `elpaish-mcp-port'."
  (interactive "P")
  (elpaish-mcp-register)
  (if (and (null port) (fboundp 'mcpkit-active-port) (mcpkit-active-port))
      (mcpkit-start-service 'elpaish)
    (mcpkit-start-service 'elpaish :port (or port elpaish-mcp-port))))

(elpaish-mcp-register)

(provide 'elpaish-mcp)
;;; elpaish-mcp.el ends here
