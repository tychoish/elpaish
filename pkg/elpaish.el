;;; elpaish.el --- Multi-track ELPA repository builder and CI automation -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maintenance, tools, local, package, elpa
;; Package-Requires: ((emacs "27.1") (magit "3.0.0") (map "3.0") (seq "2.0"))

;;; Commentary:
;; This package manages, builds, signs, and publishes a multi-track MELPA-style
;; ELPA package repository hosted on GitHub Pages or local web servers.
;;
;; Architecture & Tracks:
;; - `elpaish`: Primary snapshot archive tracking the TIP of the default branch
;;   with pure date-based version strings (YYYYMMDD.HHMMSS).
;; - `elpaish-stable`: Official releases built strictly from clean semver Git tags
;;   (vX.Y.Z -> X.Y.Z). Repositories lacking clean tags are omitted entirely.
;; - `elpaish-staging`: Pre-release builds and release candidates derived from
;;   non-stable Git tags (e.g. -rc, -pre, -beta) and `git describe` versions.
;;
;; Features:
;; - Pure Emacs Lisp orchestration without external build tool dependencies.
;; - In-memory version header injection and dynamic <pkg>-pkg.el tarball generation
;;   without mutating or dirtying upstream Git trees.
;; - Subkey GPG signing pipeline supporting headless CI with loopback pinentry.
;; - Full GPG key lifecycle tooling, automated secret synchronization via GitHub CLI
;;   (`gh secret set`), and emergency revocation publishing.
;; - Preflight package validation gates (check-parens, checkdoc, package-lint,
;;   isolated byte-compilation, ERT tests) with automatic package quarantine.
;; - Built-in local HTTP preview server for testing in isolated `emacs -Q` sessions.
;; - Interactive `tabulated-list-mode` status UI (`*elpaish-status*`).

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'epg)
(require 'magit)
(require 'map)
(require 'package)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'timer)
(require 'elpaish-check nil t)
(require 'annotated-completing-read nil t)

(defgroup elpaish nil
  "Multi-track ELPA package repository builder."
  :group 'development)

(defcustom elpaish-output-dir (expand-file-name "public/" default-directory)
  "Root directory where package archives, keys, and indexes are written."
  :type 'directory
  :group 'elpaish)

(defcustom elpaish-work-dir (expand-file-name "repos/" default-directory)
  "Directory where remote Git repositories are cloned."
  :type 'directory
  :group 'elpaish)

(defcustom elpaish-sign-packages nil
  "When non-nil, sign generated package archives and `archive-contents' with GPG."
  :type 'boolean
  :group 'elpaish)

(defcustom elpaish-force-rebuild nil
  "When non-nil, rebuild package artifacts even if the commit hash is unchanged."
  :type 'boolean
  :group 'elpaish)

(defcustom elpaish-gpg-key nil
  "GPG key ID, fingerprint, or email used to sign packages.
If nil, checks `ELPAISH_KEY_ID' or `ELPAISH_GPG_KEY' environment variables,
falling back to the first available secret key in the GPG keyring."
  :type '(choice (const :tag "Default / Environment Key" nil)
                 (string :tag "Key ID or Fingerprint"))
  :group 'elpaish)

(defcustom elpaish-gpg-passphrase nil
  "Optional passphrase for GPG signing key (or from `ELPAISH_GPG_PASSPHRASE')."
  :type '(choice (const :tag "None / GPG Agent" nil)
                 (string :tag "Passphrase"))
  :group 'elpaish)

(defcustom elpaish-release-mode 'all
  "Default release mode / track for building packages.
Can be `all' (builds elpaish, elpaish-stable, and elpaish-staging),
`elpaish' (snapshot date versions), `elpaish-stable' (semver tags only),
or `elpaish-staging' (pre-release tags and git describe)."
  :type '(choice (const :tag "All Tracks (elpaish, stable, staging)" all)
                 (const :tag "elpaish (Snapshot date versions)" elpaish)
                 (const :tag "elpaish-stable (Clean semver tags only)" elpaish-stable)
                 (const :tag "elpaish-staging (Pre-release & describe)" elpaish-staging))
  :group 'elpaish)

(defcustom elpaish-run-preflight t
  "When non-nil, execute preflight quality gates before building packages."
  :type 'boolean
  :group 'elpaish)

(defcustom elpaish-default-branch "main"
  "Default Git branch to track for recipes that do not specify one."
  :type 'string
  :group 'elpaish)

(defconst elpaish-tracks '(elpaish elpaish-stable elpaish-staging)
  "List of supported package archive tracks.")

;;; Registry Data Structure

(cl-defstruct (elpaish-recipe (:constructor elpaish-recipe-create))
  "Structure representing an ELPA package build recipe."
  (name nil :type string :documentation "Package name.")
  (repo nil :type string :documentation "Local directory path or Git URL.")
  (branch "main" :type string :documentation "Git branch to track.")
  (files '("*.el") :type list :documentation "List of file patterns to include.")
  (source-dir "." :type string :documentation "Subdirectory within REPO holding the package source.")
  (test-dir nil :type (choice null string) :documentation "Optional custom test directory.")
  (preflight-skip nil :type (choice boolean list) :documentation "Checks to skip in preflight.")
  (summary nil :type (choice null string) :documentation "Package summary description.")
  (url nil :type (choice null string) :documentation "Upstream homepage or repository URL.")
  (keywords nil :type list :documentation "List of keywords.")
  (requires nil :type list :documentation "Declared dependencies ((dep min-ver) ...).")
  (built-version-elpaish nil :type (choice null string) :documentation "Last built version for elpaish track.")
  (built-version-stable nil :type (choice null string) :documentation "Last built version for stable track.")
  (built-version-staging nil :type (choice null string) :documentation "Last built version for staging track.")
  (built-hash nil :type (choice null string) :documentation "Git commit hash when last built.")
  (built-type 'single :type symbol :documentation "Package archive type ('single or 'tar)."))

(defvar elpaish-registry (make-hash-table :test 'equal)
  "Registry storing package recipes keyed by package name string.")

(defvar elpaish-timer nil
  "Timer object for scheduled repository auto-rebuilds.")

(defvar elpaish-server-process nil
  "Process handle for local preview HTTP server.")

;; Compatibility accessors for single built-version references
(defun elpaish-recipe-built-version (recipe)
  "Return most recent built version for RECIPE across tracks."
  (or (elpaish-recipe-built-version-elpaish recipe)
      (elpaish-recipe-built-version-stable recipe)
      (elpaish-recipe-built-version-staging recipe)))

(gv-define-setter elpaish-recipe-built-version (val recipe)
  `(setf (elpaish-recipe-built-version-elpaish ,recipe) ,val))

;;;###autoload
(cl-defun elpaish-register-package (name repo &key (branch "main") (files '("*.el"))
                                              (source-dir ".")
                                              test-dir preflight-skip summary url keywords requires)
  "Register package NAME with REPO local directory path or remote Git URL.
BRANCH defaults to \"main\" and FILES defaults to \\='(\"*.el\").
SOURCE-DIR is the subdirectory within REPO holding the package (default \".\"),
for packages that live inside a monorepo or a nested \"lisp/\" folder.
TEST-DIR, PREFLIGHT-SKIP, SUMMARY, URL, KEYWORDS, and REQUIRES provide metadata."
  (let* ((raw-name (if (symbolp name) (symbol-name name) (string-trim name)))
         (name-str (string-remove-suffix ".el" raw-name))
         (recipe (elpaish-recipe-create
                  :name name-str
                  :repo (if (and (stringp repo) (not (string-match-p "\\`https?://" repo)) (not (string-match-p "\\`git@" repo)))
                            (expand-file-name repo)
                          repo)
                  :branch (or branch elpaish-default-branch)
                  :files (or files '("*.el"))
                  :source-dir (or source-dir ".")
                  :test-dir test-dir
                  :preflight-skip preflight-skip
                  :summary (or summary "No description")
                  :url url
                  :keywords (or keywords '("tools"))
                  :requires requires
                  :built-version-elpaish nil
                  :built-version-stable nil
                  :built-version-staging nil
                  :built-hash nil
                  :built-type 'single)))
    (puthash name-str recipe elpaish-registry)
    recipe))

(defun elpaish-clear-registry ()
  "Clear all registered recipes from the registry."
  (interactive)
  (clrhash elpaish-registry))

;;; Monorepo Package Discovery

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
(defun elpaish-discover-recipes (root &optional branch)
  "Scan ROOT for monorepo subpackages and register a recipe for each.
Looks for immediate subdirectories containing a `<dir-name>.el' file with a
package header, and registers each with the appropriate `:source-dir'.
BRANCH defaults to `elpaish-default-branch'."
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
          :files (list (format "%s.el" name)))))
     candidates)
    (message "Discovered and registered %d monorepo package(s) under %s."
             (length candidates) expanded-root)
    candidates))

;;; Track & Directory Resolution

(defun elpaish-canonical-track (track)
  "Return canonical track symbol for TRACK (`elpaish', `elpaish-stable', `elpaish-staging')."
  (pcase track
    ((or 'elpaish 'snapshot 'unstable) 'elpaish)
    ((or 'elpaish-stable 'stable) 'elpaish-stable)
    ((or 'elpaish-staging 'staging 'pre) 'elpaish-staging)
    ('all 'all)
    (_ 'elpaish)))

(defun elpaish-track-dir (track &optional root-dir)
  "Return destination directory for TRACK under ROOT-DIR (or `elpaish-output-dir')."
  (let ((base (file-name-as-directory (or root-dir elpaish-output-dir)))
        (canon (elpaish-canonical-track track)))
    (if (eq canon 'all)
        base
      (expand-file-name (symbol-name canon) base))))

(defun elpaish-recipe-version-for-track (recipe track)
  "Return stored built version string for RECIPE on TRACK."
  (pcase (elpaish-canonical-track track)
    ('elpaish (elpaish-recipe-built-version-elpaish recipe))
    ('elpaish-stable (elpaish-recipe-built-version-stable recipe))
    ('elpaish-staging (elpaish-recipe-built-version-staging recipe))
    (_ (elpaish-recipe-built-version recipe))))

;;; Git & Path Resolution
(defvar elpaish--resolved-repo-path-cache (make-hash-table :test 'eq)
  "Cache of RECIPE -> resolved local repo directory for the current build run.
Avoids redundant `git fetch'/clone operations when the same recipe's path
is resolved more than once (its own preflight/build, plus as a sibling
load-path entry for every other recipe's preflight check).")

(defun elpaish--resolve-repo-path (recipe)
  "Return working directory for RECIPE, cloning or fetching if remote Git URL."
  (or (map-elt elpaish--resolved-repo-path-cache recipe)
      (setf (map-elt elpaish--resolved-repo-path-cache recipe)
            (elpaish--resolve-repo-path-1 recipe))))

(defun elpaish--resolve-repo-path-1 (recipe)
  "Uncached implementation of `elpaish--resolve-repo-path'."
  (let ((repo-target (elpaish-recipe-repo recipe)))
    (if (and (stringp repo-target)
             (not (string-match-p "\\`https?://" repo-target))
             (not (string-match-p "\\`git@" repo-target))
             (file-directory-p (expand-file-name repo-target)))
        (expand-file-name repo-target)
      ;; Remote Git repository target
      (let* ((name (elpaish-recipe-name recipe))
             (branch (or (elpaish-recipe-branch recipe) "main"))
             (pkg-dir (expand-file-name name elpaish-work-dir)))
        (make-directory elpaish-work-dir t)
        (if (file-exists-p (expand-file-name ".git" pkg-dir))
            (let ((default-directory pkg-dir))
              (call-process "git" nil nil nil "fetch" "origin")
              (call-process "git" nil nil nil "checkout" branch)
              (call-process "git" nil nil nil "reset" "--hard" (concat "origin/" branch)))
          (call-process "git" nil nil nil "clone" "--branch" branch repo-target pkg-dir))
        pkg-dir))))

(defun elpaish--recipe-source-dir-relative (recipe)
  "Return RECIPE's `:source-dir' relative path, or nil when it is the repo root."
  (let ((source-dir (or (elpaish-recipe-source-dir recipe) ".")))
    (unless (string= source-dir ".")
      (string-remove-suffix "/" source-dir))))

(defun elpaish--recipe-source-path (recipe)
  "Return the absolute source directory for RECIPE, honoring `:source-dir'."
  (let ((repo-dir (elpaish--resolve-repo-path recipe))
        (rel (elpaish--recipe-source-dir-relative recipe)))
    (if rel
        (expand-file-name rel repo-dir)
      repo-dir)))

(defun elpaish--current-hash (repo-dir &optional source-dir-rel)
  "Get current HEAD hash in REPO-DIR, optionally scoped to SOURCE-DIR-REL."
  (let ((default-directory repo-dir))
    (or (if (and source-dir-rel (file-directory-p (expand-file-name ".git" repo-dir)))
            (magit-git-string "log" "-1" "--format=%H" "--" source-dir-rel)
          (magit-git-string "rev-parse" "HEAD"))
        "uncommitted")))

(defun elpaish--commit-delta (repo-dir built-hash &optional source-dir-rel)
  "Calculate commit count between BUILT-HASH and HEAD in REPO-DIR.
When SOURCE-DIR-REL is non-nil, scope the count to that subtree."
  (let ((default-directory repo-dir))
    (if (seq-contains-p '(nil "" "uncommitted") built-hash)
        "New"
      (or (if source-dir-rel
              (magit-git-string "rev-list" "--count" (concat built-hash "..HEAD") "--" source-dir-rel)
            (magit-git-string "rev-list" "--count" (concat built-hash "..HEAD")))
          "0"))))

;;; Track Version Derivation Engine

(defun elpaish--get-snapshot-version (repo-dir &optional source-dir-rel)
  "Return pure UTC date version string (YYYYMMDD.HHMMSS) for REPO-DIR.
When SOURCE-DIR-REL is non-nil, scope the Git log query to that subtree so
that unrelated monorepo packages do not bump each other's snapshot version.
Derives deterministic UTC date strings from Git commit timestamps."
  (let ((default-directory repo-dir))
    (or (and (file-directory-p (expand-file-name ".git" repo-dir))
             (when-let* ((epoch-str (if source-dir-rel
                                        (magit-git-string "log" "-1" "--format=%ct" "--" source-dir-rel)
                                      (magit-git-string "log" "-1" "--format=%ct"))))
               (unless (string-empty-p (string-trim epoch-str))
                 (format-time-string "%Y%m%d.%H%M%S" (seconds-to-time (string-to-number epoch-str)) t))))
        (format-time-string "%Y%m%d.%H%M%S" nil t))))

(defun elpaish--stable-tag-p (tag)
  "Return non-nil if TAG is a clean semver release tag (excluding pre-releases)."
  (and (stringp tag)
       (string-match-p "\\`v?[0-9]+\\.[0-9]+\\(?:\\.[0-9]+\\)*\\'" tag)
       (not (string-match-p "[-._]\\(?:rc\\|pre\\|beta\\|alpha\\|dev\\|preview\\)" tag))))

(defun elpaish--clean-semver-string (tag)
  "Strip leading \\='v\\=' from TAG."
  (if (string-prefix-p "v" tag)
      (substring tag 1)
    tag))

(defun elpaish--get-stable-version (repo-dir)
  "Return highest clean stable semver tag version in REPO-DIR, or nil if none."
  (let* ((default-directory repo-dir))
    (when (file-directory-p (expand-file-name ".git" repo-dir))
      (let* ((raw-tags-str (or (magit-git-string "tag" "-l" "--sort=-v:refname") ""))
             (all-tags (split-string raw-tags-str "\n" t))
             (stable-tags (seq-filter #'elpaish--stable-tag-p all-tags)))
        (when stable-tags
          (elpaish--clean-semver-string (car stable-tags)))))))

(defun elpaish--normalize-staging-version (raw-ver)
  "Normalize RAW-VER string so it parses cleanly into a valid `version-to-list'."
  (let ((clean (if (string-prefix-p "v" raw-ver) (substring raw-ver 1) raw-ver)))
    ;; Handle git-describe format: 1.2.0-4-gabcdef -> 1.2.0.4
    (if (string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)[-. ]+\\([0-9]+\\)[-. ]+g[0-9a-fA-F]+\\'" clean)
        (format "%s.%s" (match-string 1 clean) (match-string 2 clean))
      ;; Replace hyphens with dots
      (setq clean (replace-regexp-in-string "-+" "." clean))
      ;; Clean up double dots or dotted pre-release: 1.2.0.rc.1 -> 1.2.0.rc1
      (setq clean (replace-regexp-in-string "\\.\\(rc\\|pre\\|beta\\|alpha\\)\\." ".\\1" clean))
      ;; Validate with version-to-list
      (condition-case nil
          (progn (version-to-list clean) clean)
        (error
         (let ((nums (seq-filter (lambda (s) (string-match-p "\\`[0-9]+\\'" s))
                                 (split-string clean "[^0-9a-zA-Z]+" t))))
           (if nums
               (string-join nums ".")
             (format-time-string "%Y%m%d.%H%M%S" nil t))))))))

(cl-defun elpaish--get-staging-version (repo-dir)
  "Return pre-release or git-describe version string for REPO-DIR."
  (let ((default-directory repo-dir))
    (unless (file-directory-p (expand-file-name ".git" repo-dir))
      (cl-return-from elpaish--get-staging-version
        (format "0.0.0.%s" (elpaish--get-snapshot-version repo-dir))))
    ;; 1. Check if any pre-release tags exist
    (let* ((raw-tags-str (or (magit-git-string "tag" "-l" "--sort=-v:refname") ""))
           (all-tags (split-string raw-tags-str "\n" t))
           (pre-tags (seq-filter (lambda (tg)
                                   (and (string-match-p "\\`v?[0-9]" tg)
                                        (string-match-p "[-._]\\(?:rc\\|pre\\|beta\\|alpha\\)" tg)))
                                 all-tags)))
      (if pre-tags
          (elpaish--normalize-staging-version (car pre-tags))
        ;; 2. Fall back to git describe or commit count
        (let ((desc (or (magit-git-string "describe" "--tags" "--always" "--long")
                        (magit-git-string "describe" "--always"))))
          (cond
           ((and desc (string-match "\\`v?\\([0-9]+\\.[0-9]+\\(?:\\.[0-9]+\\)*\\)-\\([0-9]+\\)-g\\([0-9a-fA-F]+\\)\\'" desc))
            (let ((tag-part (match-string 1 desc))
                  (commits-ahead (match-string 2 desc)))
              (if (string= commits-ahead "0")
                  (elpaish--clean-semver-string tag-part)
                (format "%s.%s" tag-part commits-ahead))))
           (t
            (let ((count (or (magit-git-string "rev-list" "--count" "HEAD") "1")))
              (format "0.0.0.%s" count)))))))))

(defun elpaish-derive-version (recipe track)
  "Derive the package version string for RECIPE on TRACK.
TRACK is one of `elpaish', `elpaish-stable', or `elpaish-staging'.
Returns nil for `elpaish-stable' if no clean stable tag is present."
  (let* ((repo-dir (elpaish--resolve-repo-path recipe))
         (source-dir-rel (elpaish--recipe-source-dir-relative recipe))
         (canon (elpaish-canonical-track track)))
    (pcase canon
      ('elpaish
       (elpaish--get-snapshot-version repo-dir source-dir-rel))
      ('elpaish-stable
       (elpaish--get-stable-version repo-dir))
      ('elpaish-staging
       (elpaish--get-staging-version repo-dir))
      (_
       (elpaish--get-snapshot-version repo-dir)))))

;;; In-Memory Version Header Injection & Packaging

(defun elpaish--inject-version-header (version-str)
  "Ensure current buffer has a `;; Version: VERSION-STR' header line."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^;;\\s-*\\(?:Package-\\)?Version:\\s-*.*$" nil t)
        (replace-match (format ";; Version: %s" version-str))
      (goto-char (point-min))
      (if (re-search-forward "^;;\\s-*Author:" nil t)
          (beginning-of-line)
        (forward-line 1))
      (insert (format ";; Version: %s\n" version-str)))))

(defun elpaish--collect-files (repo-dir patterns &optional pkg-name)
  "Collect relative file paths in REPO-DIR matching PATTERNS, excluding tests and generated descriptor files."
  (let ((default-directory repo-dir)
        (name-str (and pkg-name (if (symbolp pkg-name) (symbol-name pkg-name) pkg-name))))
    (thread-last (or patterns '("*.el"))
      (seq-mapcat #'file-expand-wildcards)
      (seq-filter #'file-regular-p)
      (seq-remove (lambda (f)
                    (let ((base (file-name-nondirectory f)))
                      (or (string-match-p "\\`\\.#" base)
                          (string-suffix-p ".elc" base)
                          (string-suffix-p "-autoloads.el" base)
                          (and name-str (string= base (format "%s-pkg.el" name-str)))
                          (string-match-p "\\`test/" f)
                          (string-match-p "\\`tests/" f)
                          (string-prefix-p "test-" base)
                          (string-suffix-p "-test.el" base)
                          (string-suffix-p "-tests.el" base)))))
      (seq-uniq))))

(defun elpaish--generate-pkg-file (dest-file name version-str summary reqs url keywords)
  "Write `<pkg>-pkg.el' descriptor at DEST-FILE."
  (with-temp-file dest-file
    (insert ";; -*- no-byte-compile: t -*-\n")
    (let ((req-forms (mapcar (lambda (r)
                               (let ((dep (car r))
                                     (ver (cadr r)))
                                 (list dep (if (stringp ver) ver (package-version-join ver)))))
                             reqs))
          (extra-kws (append (when url `(:url ,url))
                             (when keywords `(:keywords ,@keywords)))))
      (insert (format "(define-package %S %S %S\n  '%S\n"
                      name version-str (or summary "No description") req-forms))
      (when extra-kws
        (insert (format "  %s" (mapconcat (lambda (x) (format "%S" x)) extra-kws " "))))
      (insert ")\n"))))

(defun elpaish--create-tar-package (repo-dir dest-file pkg-name-ver files
                                                  name version-str summary reqs url keywords)
  "Create a tar package at `DEST-FILE' for `FILES' in `REPO-DIR' named `PKG-NAME-VER'."
  (let* ((temp-dir (make-temp-file "elpaish-pkg-" t))
         (pkg-subdir (expand-file-name pkg-name-ver temp-dir)))
    (unwind-protect
        (progn
          (make-directory pkg-subdir t)
          (dolist (rel-file files)
            (let ((dst (expand-file-name rel-file pkg-subdir)))
              (make-directory (file-name-directory dst) t)
              (copy-file (expand-file-name rel-file repo-dir) dst t)))
          ;; Generate <pkg>-pkg.el inside tarball root
          (let ((pkg-file (expand-file-name (format "%s-pkg.el" name) pkg-subdir)))
            (elpaish--generate-pkg-file pkg-file name version-str summary reqs url keywords))
          (let ((default-directory temp-dir))
            (call-process "tar" nil nil nil "-cf" dest-file pkg-name-ver)))
      (delete-directory temp-dir t))))

;;; GPG Package & Archive Signing Pipeline

(defun elpaish--detect-secret-key-id ()
  "Return the key ID or fingerprint of the first secret signing key in GPG keyring."
  (when (executable-find "gpg")
    (with-temp-buffer
      (when (zerop (call-process "gpg" nil t nil "--list-secret-keys" "--with-colons"))
        (goto-char (point-min))
        (let (fpr)
          (while (and (not fpr) (re-search-forward "^fpr:::::::::+\\([0-9A-Fa-f]+\\):" nil t))
            (setq fpr (match-string 1)))
          fpr)))))

(defun elpaish--get-signing-key ()
  "Resolve active GPG key ID from custom var, environment, or secret keyring."
  (or (and elpaish-gpg-key (not (string-prefix-p "-----BEGIN" elpaish-gpg-key)) elpaish-gpg-key)
      (let ((k (getenv "ELPAISH_KEY_ID")))
        (and k (not (string-prefix-p "-----BEGIN" k)) (not (string-empty-p k)) k))
      (let ((k (getenv "ELPAISH_GPG_KEY")))
        (and k (not (string-prefix-p "-----BEGIN" k)) (not (string-empty-p k)) k))
      (elpaish--detect-secret-key-id)
      nil))

(defun elpaish--get-signing-passphrase ()
  "Resolve GPG passphrase from custom var or environment."
  (or elpaish-gpg-passphrase
      (getenv "ELPAISH_GPG_PASSPHRASE")
      ""))

(defun elpaish--sign-with-gpg-cli (file sig-file key-id passphrase)
  "Sign FILE generating detached signature SIG-FILE using `gpg' CLI in non-interactive batch mode.
KEY-ID is the signing key. PASSPHRASE is the optional passphrase."
  (let* ((pass (or passphrase (elpaish--get-signing-passphrase)))
         (args (list "--batch" "--yes" "--detach-sign" "--pinentry-mode" "loopback"))
         (args (if (and key-id (not (string-prefix-p "-----BEGIN" key-id)))
                   (append args (list "--default-key" key-id))
                 args))
         (args (append args (list "--passphrase-fd" "0" "--output" sig-file file))))
    (with-temp-buffer
      (insert pass "\n")
      (apply #'call-process-region (point-min) (point-max) "gpg" t t nil args))))

(defun elpaish--sign-file (file)
  "Generate a detached GPG signature `FILE.sig' for FILE if signing is enabled.
Strictly non-interactive: executes in batch mode via loopback without prompting."
  (when elpaish-sign-packages
    (let ((key-id (elpaish--get-signing-key))
          (passphrase (elpaish--get-signing-passphrase))
          (sig-file (concat file ".sig")))
      (when (file-exists-p sig-file)
        (delete-file sig-file))
      (when (executable-find "gpg")
        (let ((exit-code (elpaish--sign-with-gpg-cli file sig-file key-id passphrase)))
          (if (and (numberp exit-code) (zerop exit-code) (file-exists-p sig-file))
              (message "Signed %s -> %s"
                       (file-name-nondirectory file)
                       (file-name-nondirectory sig-file))
            (message "Warning: Failed to sign %s" (file-name-nondirectory file))))))))

;;;###autoload
(cl-defun elpaish-sign-file-headless (file &key key-id passphrase output-file)
  "Sign FILE headlessly generating detached signature for encrypted or unencrypted keys.
Passphrase is provided non-interactively via PASSPHRASE argument,
`elpaish-gpg-passphrase', or `ELPAISH_GPG_PASSPHRASE' environment variable.
Never triggers interactive terminal or GUI pinentry dialogs.
OUTPUT-FILE defaults to FILE.sig. Returns the path to the generated signature, or nil."
  (let* ((sig-file (or output-file (concat file ".sig")))
         (resolved-key (or key-id (elpaish--get-signing-key)))
         (resolved-pass (or passphrase (elpaish--get-signing-passphrase))))
    (when (file-exists-p sig-file)
      (delete-file sig-file))
    (if (not (executable-find "gpg"))
        (progn
          (message "GPG binary not found in PATH")
          nil)
      (let ((exit-code (elpaish--sign-with-gpg-cli file sig-file resolved-key resolved-pass)))
        (if (and (numberp exit-code) (zerop exit-code) (file-exists-p sig-file))
            (progn
              (message "Headless signature created: %s" sig-file)
              sig-file)
          (message "Headless signing failed for %s (exit code %S)" file exit-code)
          nil)))))
(defun elpaish--ensure-gpg-agent-loopback ()
  "Ensure gpg-agent is configured to allow loopback pinentry for batch signing."
  (when (executable-find "gpg")
    (let* ((gnupg-dir (expand-file-name ".gnupg" (or (getenv "GNUPGHOME") (getenv "HOME"))))
           (agent-conf (expand-file-name "gpg-agent.conf" gnupg-dir)))
      (make-directory gnupg-dir t)
      (set-file-modes gnupg-dir #o700)
      (unless (and (file-exists-p agent-conf)
                   (with-temp-buffer
                     (insert-file-contents agent-conf)
                     (search-forward "allow-loopback-pinentry" nil t)))
        (with-temp-file agent-conf
          (when (file-exists-p agent-conf)
            (insert-file-contents agent-conf))
          (goto-char (point-max))
          (insert "\nallow-loopback-pinentry\n"))
        (set-file-modes agent-conf #o600)
        (when (executable-find "gpgconf")
          (call-process "gpgconf" nil nil nil "--kill" "gpg-agent"))
        (when (executable-find "gpg-connect-agent")
          (call-process "gpg-connect-agent" nil nil nil "reloadagent" "/bye"))))))

;;;###autoload
(defun elpaish-init-signing-from-env ()
  "Initialize GPG signing configuration from `ELPAISH_SIGNING_KEY' environment variable.
Imports key armor, configures loopback pinentry, detects secret key ID, and enables signing.
Returns the detected signing key ID or nil."
  (let ((key-armor (getenv "ELPAISH_SIGNING_KEY"))
        (passphrase (or (getenv "ELPAISH_GPG_PASSPHRASE") "")))
    (when (and key-armor (not (string-empty-p key-armor)) (executable-find "gpg"))
      (elpaish--ensure-gpg-agent-loopback)
      (with-temp-buffer
        (insert key-armor)
        (call-process-region (point-min) (point-max) "gpg" nil nil nil "--batch" "--import"))
      (setq elpaish-sign-packages t)
      (setq elpaish-gpg-passphrase passphrase)
      (setq elpaish-gpg-key (elpaish--detect-secret-key-id))
      (when elpaish-gpg-key
        (message "[elpaish] GPG signing initialized for key %s" elpaish-gpg-key))
      elpaish-gpg-key)))

(defun elpaish-setup-signing ()
  "Interactive wizard to guide the user through selecting a GPG signing key."
  (interactive)
  (unless (executable-find "gpg")
    (user-error "GPG executable not found in system PATH"))
  (let* ((keys (or (epg-list-keys (epg-make-context 'OpenPGP) "" t)
                   (user-error "No secret GPG keys found. Please generate a GPG key first using `gpg --full-generate-key'")))
         (table (thread-last keys
                  (seq-map (lambda (k)
                             (cons (epg-sub-key-id (car (epg-key-sub-key-list k)))
                                   (epg-user-id-string (car (epg-key-user-id-list k))))))))
         (key-id (if (fboundp 'annotated-completing-read)
                     (annotated-completing-read table
                                                :prompt "Select GPG key for signing ELPA packages: "
                                                :require-match t)
                   (completing-read "Select GPG key for signing ELPA packages: " table nil t))))
    (setq elpaish-gpg-key key-id
          elpaish-sign-packages t)
    (message "GPG package signing enabled! Selected Key ID: %s." key-id)))

;;; Key Lifecycle, Subkey Rotation & Secret Sync

(defun elpaish-export-keyring (&optional output-dir key-id)
  "Export binary `elpaish-keyring.gpg' and armored `elpaish.pub.asc' to OUTPUT-DIR."
  (let* ((target-dir (or output-dir elpaish-output-dir))
         (key (or key-id (elpaish--get-signing-key) ""))
         (gpg-bin (executable-find "gpg")))
    (when gpg-bin
      (make-directory target-dir t)
      (let ((binary-ring (expand-file-name "elpaish-keyring.gpg" target-dir))
            (armor-pub (expand-file-name "elpaish.pub.asc" target-dir)))
        (call-process gpg-bin nil nil nil "--batch" "--yes" "--output" binary-ring "--export" key)
        (call-process gpg-bin nil nil nil "--batch" "--yes" "--armor" "--output" armor-pub "--export" key)
        (message "Exported public keyrings to %s and %s" binary-ring armor-pub)))))

;;;###autoload
(cl-defun elpaish-rotate-keys (&key master-key-id repo-slug (output-dir elpaish-output-dir))
  "Rotate GPG signing subkey [S], sync with GitHub secrets, and export updated keyring.
MASTER-KEY-ID defaults to the primary certification key.
REPO-SLUG defaults to \"tychoish/elpaish\"."
  (interactive)
  (unless (executable-find "gpg")
    (user-error "GPG binary not found in PATH"))
  (let* ((master (or master-key-id
                     (if (called-interactively-p 'interactive)
                         (read-string "Primary / Master GPG Key ID or Fingerprint: " (elpaish--get-signing-key))
                       (elpaish--get-signing-key))
                     (user-error "No master key ID specified")))
         (target-repo (or repo-slug "tychoish/elpaish"))
         (gpg-bin (executable-find "gpg"))
         (gh-bin (executable-find "gh")))

    (message "Generating new 1-year signing subkey for %s..." master)
    (call-process gpg-bin nil nil nil "--batch" "--quick-add-key" master "ed25519" "sign" "1y")
    (elpaish-export-keyring output-dir master)

    (if (not gh-bin)
        (message "GitHub CLI `gh' not found; skipped automated secret sync.")
      (with-temp-buffer
        (let ((export-code (call-process gpg-bin nil t nil "--batch" "--armor" "--export-secret-subkeys" master)))
          (if (not (zerop export-code))
              (message "Warning: Failed to export secret subkeys for GitHub secret sync.")
            (let ((secret-str (buffer-string)))
              (with-temp-buffer
                (insert secret-str)
                (let ((gh-code (call-process-region (point-min) (point-max) gh-bin nil nil nil
                                                    "secret" "set" "ELPAISH_SIGNING_KEY" "-R" target-repo)))
                  (if (zerop gh-code)
                      (message "Successfully synchronized ELPAISH_SIGNING_KEY secret to %s!" target-repo)
                    (message "Warning: `gh secret set` failed with exit code %d" gh-code)))))))))

    (message "Key rotation complete for %s." master)))

;;;###autoload
(defun elpaish-revoke-key (key-id &optional output-dir)
  "Publish revocation certificate for KEY-ID to `elpaish.rev.asc' in OUTPUT-DIR."
  (interactive "sKey ID or Fingerprint to revoke: ")
  (unless (executable-find "gpg")
    (user-error "GPG binary not found in PATH"))
  (unless (and key-id (not (string-empty-p key-id)))
    (user-error "No key ID provided for revocation"))
  (let* ((target-dir (or output-dir elpaish-output-dir))
         (rev-file (expand-file-name "elpaish.rev.asc" target-dir))
         (gpg-bin (executable-find "gpg")))
    (make-directory target-dir t)
    (call-process gpg-bin nil nil nil "--batch" "--yes" "--armor" "--output" rev-file "--gen-revoke" key-id)
    (elpaish-export-keyring target-dir)
    (message "Published revocation certificate to %s" rev-file)))

;;; Preflight Package Quality Gates

(defun elpaish-preflight-package (recipe)
  "Execute preflight quality gates on RECIPE.
Returns t if checks pass, nil if quarantined."
  (if (not elpaish-run-preflight)
      t
    (let ((skip (elpaish-recipe-preflight-skip recipe)))
      (if (eq skip t)
          t
        (unless (featurep 'elpaish-check)
          (require 'elpaish-check nil t))
        (if (fboundp 'elpaish-check-package)
            (let* ((repo-dir (elpaish--recipe-source-path recipe))
                   (sibling-dirs (thread-last (hash-table-values elpaish-registry)
                                   (seq-remove (lambda (r) (eq r recipe)))
                                   (seq-map #'elpaish--recipe-source-path)))
                   (tdir (elpaish-recipe-test-dir recipe))
                   (res (elpaish-check-package repo-dir :test-dir tdir :skip-checks skip
                                               :extra-load-path sibling-dirs))
                   (passed (plist-get res :passed))
                   (errs (plist-get res :errors)))
              (unless passed
                (message "PREFLIGHT QUARANTINE for %s: %d error(s)"
                         (elpaish-recipe-name recipe) (length errs))
                (dolist (e errs)
                  (message "   - %s" e)))
              passed)
          t)))))

;;; Package Build Engine

(defun elpaish-read-archive-contents (target-dir)
  "Read and parse `archive-contents' from TARGET-DIR if it exists.
Returns a hash table of package-name -> entry-vector."
  (let ((ac-file (expand-file-name "archive-contents" target-dir))
        (tbl (make-hash-table :test 'equal)))
    (when (file-exists-p ac-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents ac-file)
            (let ((data (read (current-buffer))))
              (when (and (listp data) (eq (car data) 1))
                (dolist (entry (cdr data))
                  (when (and (consp entry) (symbolp (car entry)) (vectorp (cdr entry)))
                    (puthash (symbol-name (car entry)) (cdr entry) tbl))))))
        (error nil)))
    tbl))

(defun elpaish--extract-buffer-metadata (recipe)
  "Extract package metadata from current buffer or fallback to RECIPE defaults.
Returns a plist with :summary, :reqs, :url, and :keywords."
  (let ((pkg-info (condition-case nil (package-buffer-info) (error nil))))
    (list :summary (or (and pkg-info (package-desc-summary pkg-info))
                       (elpaish-recipe-summary recipe)
                       "No description")
          :reqs (or (and pkg-info (package-desc-reqs pkg-info))
                    (elpaish-recipe-requires recipe))
          :url (or (and pkg-info (cdr (assoc :url (package-desc-extras pkg-info))))
                   (elpaish-recipe-url recipe))
          :keywords (or (and pkg-info (cdr (assoc :keywords (package-desc-extras pkg-info))))
                        (elpaish-recipe-keywords recipe)
                        '("tools")))))

(cl-defun elpaish-build-package (recipe &optional track output-dir force)
  "Build, package, sign, and record status for RECIPE on TRACK.
TRACK is `elpaish', `elpaish-stable', or `elpaish-staging'.
OUTPUT-DIR defaults to track directory under `elpaish-output-dir'.
When FORCE is non-nil or `elpaish-force-rebuild' is non-nil, rebuild
the package even if the commit has not changed since the last build."
  (let* ((effective-track (elpaish-canonical-track (or track elpaish-release-mode)))
         (target-dir (or output-dir (elpaish-track-dir effective-track)))
         (repo-dir (elpaish--recipe-source-path recipe))
         (source-dir-rel (elpaish--recipe-source-dir-relative recipe))
         (name (elpaish-recipe-name recipe))
         (main-file-name (if (string-suffix-p ".el" name) name (concat name ".el")))
         (main-file (expand-file-name main-file-name repo-dir))
         (current-commit (elpaish--current-hash repo-dir source-dir-rel)))

    (unless (file-exists-p main-file)
      (error "Main file %s not found in %s" main-file-name repo-dir))

    ;; 1. Preflight Validation Gate
    (unless (elpaish-preflight-package recipe)
      (message "Skipping %s due to preflight quarantine." name)
      (cl-return-from elpaish-build-package nil))

    ;; 2. Derive Version
    (let ((version-str (elpaish-derive-version recipe effective-track)))
      (unless version-str
        (when (eq effective-track 'elpaish-stable)
          (message "Omitting %s from elpaish-stable: No clean semver Git tag." name))
        (cl-return-from elpaish-build-package nil))

      ;; 3. Build artifact
      (make-directory target-dir t)
      (let* ((files (elpaish--collect-files repo-dir (elpaish-recipe-files recipe) name))
             (is-tar (> (length files) 1))
             (pkg-type (if is-tar 'tar 'single))
             (pkg-name-ver (format "%s-%s" name version-str))
             (dest-file (expand-file-name (format "%s.%s" pkg-name-ver (if is-tar "tar" "el"))
                                          target-dir))
             (sig-file (concat dest-file ".sig"))
             (existing-ac-entry (gethash name (elpaish-read-archive-contents target-dir)))
             (existing-commit (when existing-ac-entry
                                (let ((extras (and (> (length existing-ac-entry) 4)
                                                   (aref existing-ac-entry 4))))
                                  (cdr (assq :commit extras)))))
             (existing-ver-list (when existing-ac-entry (aref existing-ac-entry 0)))
             (existing-ver-str (when existing-ver-list (package-version-join existing-ver-list)))
             (should-skip-rebuild
              (and (not force)
                   (not elpaish-force-rebuild)
                   (file-exists-p dest-file)
                   (or (not elpaish-sign-packages) (file-exists-p sig-file))
                   (not (equal current-commit "uncommitted"))
                   (or (equal (elpaish-recipe-built-hash recipe) current-commit)
                       (equal existing-commit current-commit))
                   (or (equal (elpaish-recipe-version-for-track recipe effective-track) version-str)
                       (equal existing-ver-str version-str)))))

        (if should-skip-rebuild
            (progn
              (pcase effective-track
                ('elpaish (setf (elpaish-recipe-built-version-elpaish recipe) version-str))
                ('elpaish-stable (setf (elpaish-recipe-built-version-stable recipe) version-str))
                ('elpaish-staging (setf (elpaish-recipe-built-version-staging recipe) version-str)))
              (setf (elpaish-recipe-built-type recipe) pkg-type
                    (elpaish-recipe-built-hash recipe) current-commit)
              (unless (elpaish-recipe-summary recipe)
                (with-temp-buffer
                  (insert-file-contents main-file)
                  (let ((meta (elpaish--extract-buffer-metadata recipe)))
                    (setf (elpaish-recipe-summary recipe) (plist-get meta :summary)
                          (elpaish-recipe-url recipe) (plist-get meta :url)
                          (elpaish-recipe-keywords recipe) (plist-get meta :keywords)
                          (elpaish-recipe-requires recipe) (plist-get meta :reqs)))))
              (message "Skipping rebuild for %s version %s on %s (commit %s unchanged)"
                       name version-str effective-track current-commit)
              dest-file)

          ;; Build artifact
          (let (meta summary reqs url keywords)
            (with-temp-buffer
              (insert-file-contents main-file)
              (elpaish--inject-version-header version-str)
              (setq meta (elpaish--extract-buffer-metadata recipe))
              (setq summary (plist-get meta :summary)
                    reqs (plist-get meta :reqs)
                    url (plist-get meta :url)
                    keywords (plist-get meta :keywords))

              (if is-tar
                  (elpaish--create-tar-package repo-dir dest-file pkg-name-ver files
                                              name version-str summary reqs url keywords)
                (write-region (point-min) (point-max) dest-file nil 'silent)))

            ;; 4. Sign artifact
            (elpaish--sign-file dest-file)

            ;; 5. Update recipe metadata & status
            (pcase effective-track
              ('elpaish (setf (elpaish-recipe-built-version-elpaish recipe) version-str))
              ('elpaish-stable (setf (elpaish-recipe-built-version-stable recipe) version-str))
              ('elpaish-staging (setf (elpaish-recipe-built-version-staging recipe) version-str)))

            (setf (elpaish-recipe-built-type recipe) pkg-type
                  (elpaish-recipe-summary recipe) summary
                  (elpaish-recipe-url recipe) url
                  (elpaish-recipe-keywords recipe) keywords
                  (elpaish-recipe-requires recipe) reqs
                  (elpaish-recipe-built-hash recipe) current-commit)

            (message "Successfully built %s version %s on %s" name version-str effective-track)
            dest-file))))))

;;; Archive Contents & Static HTML Generation

(defun elpaish-generate-archive-contents (&optional track output-dir)
  "Generate `archive-contents' and its signature for TRACK in OUTPUT-DIR."
  (let* ((effective-track (elpaish-canonical-track (or track elpaish-release-mode)))
         (target-dir (or output-dir (elpaish-track-dir effective-track)))
         (archive-file (expand-file-name "archive-contents" target-dir))
         (recipes (hash-table-values elpaish-registry))
         (entries
          (delq nil
                (mapcar
                 (lambda (recipe)
                   (when-let* ((ver-str (elpaish-recipe-version-for-track recipe effective-track)))
                     (let* ((name (intern (elpaish-recipe-name recipe)))
                            (ver (version-to-list ver-str))
                            (summary (or (elpaish-recipe-summary recipe) "No description"))
                            (pkg-type (or (elpaish-recipe-built-type recipe) 'single))
                            (reqs (elpaish-recipe-requires recipe))
                            (url (elpaish-recipe-url recipe))
                            (keywords (elpaish-recipe-keywords recipe))
                            (commit (elpaish-recipe-built-hash recipe))
                            (extras (delq nil
                                          (list (when url (cons :url url))
                                                (when commit (cons :commit commit))
                                                (when keywords (cons :keywords keywords))))))
                       `(,name . [,ver ,reqs ,summary ,pkg-type ,extras]))))
                 recipes))))
    (make-directory target-dir t)
    (with-temp-file archive-file
      (insert ";; -*- no-byte-compile: t -*-\n")
      (pp `(1 ,@entries) (current-buffer)))
    (elpaish--sign-file archive-file)
    archive-file))

(defun elpaish-generate-github-index (&optional track output-dir title)
  "Generate static `index.html' package catalog for TRACK in OUTPUT-DIR."
  (let* ((effective-track (elpaish-canonical-track (or track elpaish-release-mode)))
         (target-dir (or output-dir (elpaish-track-dir effective-track)))
         (track-label (pcase effective-track
                        ('elpaish "snapshot")
                        ('elpaish-stable "stable")
                        ('elpaish-staging "staging")
                        (_ (symbol-name effective-track))))
         (page-title (or title (format "ELPAish Repository — (%s)" track-label)))
         (recipes (hash-table-values elpaish-registry))
         (rows
          (delq nil
                (mapcar
                 (lambda (recipe)
                   (when-let* ((ver-str (elpaish-recipe-version-for-track recipe effective-track)))
                     (let* ((name (elpaish-recipe-name recipe))
                            (summary (or (elpaish-recipe-summary recipe) "No description"))
                            (is-tar (eq (elpaish-recipe-built-type recipe) 'tar))
                            (artifact (format "%s-%s.%s" name ver-str (if is-tar "tar" "el")))
                            (sig-file (expand-file-name (concat artifact ".sig") target-dir))
                            (sig-cell (if (file-exists-p sig-file)
                                          `(a ((href . ,(concat artifact ".sig"))) "sig")
                                        "—")))
                       `(tr nil
                            (td ((class . "pkg-name")) (b nil ,name))
                            (td ((class . "pkg-version")) (a ((href . ,artifact)) ,ver-str))
                            (td ((class . "pkg-desc")) ,summary)
                            (td ((class . "pkg-sig")) ,sig-cell)))))
                 recipes))))
    (make-directory target-dir t)
    (with-temp-file (expand-file-name "index.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (meta ((name . "viewport") (content . "width=device-width, initial-scale=1")))
                    (title nil ,page-title)
                    (link ((rel . "preconnect") (href . "https://fonts.googleapis.com")))
                    (link ((rel . "preconnect") (href . "https://fonts.gstatic.com") (crossorigin . "")))
                    (link ((rel . "stylesheet") (href . "https://fonts.googleapis.com/css2?family=Source+Code+Pro:wght@400;600;700&family=Source+Sans+3:wght@400;600;700&display=swap")))
                    (style nil "body{font-family:'Source Sans 3','Source Sans Pro',-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:36px auto;max-width:1240px;font-size:18px;line-height:1.6;color:#000000;background:#ffffff;padding:0 28px;} .table-wrapper{width:100%;overflow-x:auto;-webkit-overflow-scrolling:touch;margin-top:24px;} table{border-collapse:collapse;width:100%;min-width:1020px;font-size:1em;border:1px solid #c6c6c6;} th,td{padding:14px 20px;border-bottom:1px solid #d7d7d7;text-align:left;vertical-align:middle;} th{background:#e5e5e5;color:#000000;font-weight:700;font-size:1.05em;border-bottom:2px solid #707070;} tr:hover{background:#eef2f8;} .pkg-name{font-weight:700;font-size:1.05em;white-space:nowrap!important;min-width:320px;width:320px;} .pkg-name b{white-space:nowrap!important;word-break:keep-all;font-weight:700;} .pkg-version{font-family:'Source Code Pro',ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace;font-size:0.95em;white-space:nowrap!important;min-width:200px;width:200px;} .pkg-desc{min-width:440px;font-size:1em;} .pkg-sig{white-space:nowrap!important;text-align:center;width:80px;font-size:0.95em;} a{color:#0000aa;text-decoration:none;font-weight:600;} a:hover{color:#721045;text-decoration:underline;} code{font-family:'Source Code Pro',ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace;font-size:0.92em;background:#f2f2f2;color:#5317ac;padding:3px 8px;border-radius:4px;border:1px solid #d7d7d7;} .header{margin-bottom:28px;border-bottom:2px solid #707070;padding-bottom:18px;} h1{font-size:2.2em;font-weight:700;color:#000000;margin-top:0;margin-bottom:12px;} h2{font-size:1.6em;font-weight:700;color:#000000;margin-top:36px;margin-bottom:16px;}"))
              (body nil
                    (div ((class . "header"))
                         (h1 nil ,page-title)
                         (p nil "Track URL: " (code nil ,(format "https://tychoish.github.io/elpaish/%s/" effective-track)))
                         (p nil (a ((href . "../index.html")) "← Back to Archive Setup & Overview")))
                    (h2 nil "Packages")
                    ,(if (null rows)
                         '(p nil "No packages published in this track.")
                       `(div ((class . "table-wrapper"))
                             (table nil
                                    (tr nil
                                        (th ((class . "pkg-name")) "Package")
                                        (th ((class . "pkg-version")) "Version")
                                        (th ((class . "pkg-desc")) "Description")
                                        (th ((class . "pkg-sig")) "Signature"))
                                    ,@rows)))))))))

(defun elpaish-generate-top-index (&optional output-dir)
  "Generate top-level static `index.html' landing page in OUTPUT-DIR."
  (let ((target-dir (or output-dir elpaish-output-dir)))
    (make-directory target-dir t)
    (with-temp-file (expand-file-name "index.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (meta ((name . "viewport") (content . "width=device-width, initial-scale=1")))
                    (title nil "ELPAish: tychoish Emacs Lisp Package Archives")
                    (link ((rel . "preconnect") (href . "https://fonts.googleapis.com")))
                    (link ((rel . "preconnect") (href . "https://fonts.gstatic.com") (crossorigin . "")))
                    (link ((rel . "stylesheet") (href . "https://fonts.googleapis.com/css2?family=Source+Code+Pro:wght@400;600;700&family=Source+Sans+3:wght@400;600;700&display=swap")))
                    (style nil "body{font-family:'Source Sans 3','Source Sans Pro',-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:36px auto;max-width:1240px;font-size:18px;line-height:1.6;color:#000000;background:#ffffff;padding:0 28px;} h1{font-size:2.2em;font-weight:700;color:#000000;border-bottom:2px solid #707070;padding-bottom:14px;margin-top:0;margin-bottom:14px;} h2{font-size:1.6em;font-weight:700;color:#000000;margin-top:36px;margin-bottom:16px;} p{font-size:1.05em;margin:10px 0;} .track-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(340px,1fr));gap:24px;margin:28px 0;} .card{border:1px solid #c6c6c6;border-radius:8px;padding:24px;background:#f8f8f8;box-shadow:0 2px 6px rgba(0,0,0,0.06);} .card h2{font-size:1.45em;font-weight:700;margin-top:0;margin-bottom:12px;color:#002f5e;} .card p{font-size:1em;line-height:1.6;} pre{font-family:'Source Code Pro',ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace;font-size:0.95em;background:#f8f8f8;color:#000000;border:1px solid #c6c6c6;padding:18px 24px;border-radius:6px;overflow-x:auto;line-height:1.5;} code{font-family:'Source Code Pro',ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace;font-size:0.92em;background:#f2f2f2;color:#5317ac;padding:3px 8px;border-radius:4px;border:1px solid #d7d7d7;} pre code{background:transparent;color:inherit;padding:0;border:none;font-size:1em;} a{color:#0000aa;text-decoration:none;font-weight:600;} a:hover{color:#721045;text-decoration:underline;} .btn{display:inline-block;padding:10px 22px;font-size:1.02em;font-weight:700;background:#00538b;color:#ffffff;border-radius:5px;text-decoration:none;margin-top:10px;} .btn:hover{background:#003494;color:#ffffff;text-decoration:none;} ul{font-size:1.05em;padding-left:28px;} li{margin:8px 0;}"))
              (body nil
                    (h1 nil "ELPAish Emacs Package Archives")

                    (div ((class . "track-grid"))
                         (div ((class . "card"))
                              (h2 nil (a ((href . "elpaish/index.html")) "elpaish (Snapshots)"))
                              (p nil "Continuous development snapshots built from the default branch head with pure UTC date versioning (" (code nil "YYYYMMDD.HHMMSS") ").")
                              (a ((class . "btn") (href . "elpaish/index.html")) "Browse Snapshots"))
                         (div ((class . "card"))
                              (h2 nil (a ((href . "elpaish-stable/index.html")) "elpaish-stable (Releases)"))
                              (p nil "Official release builds strictly from clean semver Git tags (" (code nil "vX.Y.Z") "). Repositories without clean tags are omitted.")
                              (a ((class . "btn") (href . "elpaish-stable/index.html")) "Browse Stable"))
                         (div ((class . "card"))
                              (h2 nil (a ((href . "elpaish-staging/index.html")) "elpaish-staging (Pre-release)"))
                              (p nil "Staging release candidates (" (code nil "-rc") ", " (code nil "-pre") ") and " (code nil "git describe") " builds for integration testing.")
                              (a ((class . "btn") (href . "elpaish-staging/index.html")) "Browse Staging")))

                    (h2 nil "Emacs Configuration")
                    (p nil "Add your preferred track to " (code nil "package-archives") " in your " (code nil "init.el") ":")
                    (pre nil
                         (code nil
                               ";; Primary development snapshot track:\n(add-to-list 'package-archives '(\"elpaish\" . \"https://tychoish.github.io/elpaish/elpaish/\") t)\n\n;; Production stable release track:\n(add-to-list 'package-archives '(\"elpaish-stable\" . \"https://tychoish.github.io/elpaish/elpaish-stable/\") t)\n\n;; Pre-release / staging track:\n(add-to-list 'package-archives '(\"elpaish-staging\" . \"https://tychoish.github.io/elpaish/elpaish-staging/\") t)"))

                    (h2 nil "GPG Keyring Verification")
                    (p nil "Packages and index files are GPG signed. Import the public keyring or trust anchor:")
                    (ul nil
                        (li nil (a ((href . "elpaish-keyring.gpg")) "elpaish-keyring.gpg") " — Binary public keyring")
                        (li nil (a ((href . "elpaish.pub.asc")) "elpaish.pub.asc") " — Armored ASCII public key")
                        (li nil (a ((href . "elpaish.rev.asc")) "elpaish.rev.asc") " — Published revocation certificates (if any)"))
                    (pre nil
                         (code nil "gpg --import < elpaish.pub.asc"))))))))
;;; Build Orchestration

;;;###autoload
(cl-defun elpaish-build-all (&optional mode output-dir force)
  "Build registered packages, generate indexes, and sign archives.
MODE can be `all', `elpaish', `elpaish-stable', or `elpaish-staging'.
Defaults to `elpaish-release-mode'. OUTPUT-DIR defaults to `elpaish-output-dir'.
When FORCE is non-nil, force rebuilding all packages."
  (interactive)
  (clrhash elpaish--resolved-repo-path-cache)
  (let* ((effective-mode (or mode elpaish-release-mode))
         (target-root (or output-dir elpaish-output-dir))
         (tracks (if (eq effective-mode 'all)
                     elpaish-tracks
                   (list (elpaish-canonical-track effective-mode)))))
    (make-directory target-root t)
    (dolist (track tracks)
      (let ((track-dir (elpaish-track-dir track target-root)))
        (make-directory track-dir t)
        (dolist (recipe (hash-table-values elpaish-registry))
          (elpaish-build-package recipe track track-dir force))
        (elpaish-generate-archive-contents track track-dir)
        (elpaish-generate-github-index track track-dir)))

    ;; Generate top-level landing page and public keyrings
    (elpaish-generate-top-index target-root)
    (elpaish-export-keyring target-root)

    (message "ELPAish repository successfully generated at %s" target-root)
    (when (eq major-mode 'elpaish-status-mode)
      (elpaish-status-refresh))))

;;; Local Preview HTTP Server

;;;###autoload
(defun elpaish--http-mime-type (path)
  "Return MIME content-type string for PATH."
  (cond
   ((string-suffix-p ".html" path) "text/html; charset=utf-8")
   ((string-suffix-p ".el" path) "text/plain; charset=utf-8")
   ((string-suffix-p ".sig" path) "application/pgp-signature")
   ((string-suffix-p ".asc" path) "application/pgp-keys")
   ((string-suffix-p ".gpg" path) "application/pgp-keys")
   ((string-suffix-p ".tar" path) "application/x-tar")
   (t "text/plain")))

(defun elpaish--handle-http-request (request-str doc-root)
  "Process HTTP REQUEST-STR for document root DOC-ROOT.
Returns a cons cell (HEADERS . BODY-BYTES)."
  (if (string-match "\\`\\(GET\\|HEAD\\)[ \t]+\\([^ \t\r\n?]+\\)" request-str)
      (let* ((method (match-string 1 request-str))
             (req-path (match-string 2 request-str))
             (rel-path (if (or (string= req-path "/") (string-suffix-p "/" req-path))
                           (concat (string-remove-prefix "/" req-path) "index.html")
                         (string-remove-prefix "/" req-path)))
             (file-path (expand-file-name rel-path doc-root)))
        (if (and (file-exists-p file-path) (file-regular-p file-path))
            (let* ((content (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally file-path)
                              (buffer-string)))
                   (mime (elpaish--http-mime-type file-path))
                   (headers (format "HTTP/1.1 200 OK\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n"
                                    mime (length content))))
              (cons headers (if (string= method "HEAD") "" content)))
          (cons "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 9\r\nConnection: close\r\n\r\n"
                "Not Found")))
    (cons "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 11\r\nConnection: close\r\n\r\n"
          "Bad Request")))

;;;###autoload
(defun elpaish-serve-local (&optional port output-dir)
  "Start a local HTTP server serving the generated `public/' directory at PORT (default 8080)."
  (interactive "P")
  (elpaish-stop-server)
  (let* ((server-port (or port 8080))
         (doc-root (file-name-as-directory (or output-dir elpaish-output-dir))))
    (unless (file-directory-p doc-root)
      (make-directory doc-root t))
    (setq elpaish-server-process
          (make-network-process
           :name "elpaish-preview-server"
           :service server-port
           :server t
           :family 'ipv4
           :host "127.0.0.1"
           :filter
           (lambda (proc string)
             (let ((res (elpaish--handle-http-request string doc-root)))
               (process-send-string proc (car res))
               (when (and (cdr res) (not (string-empty-p (cdr res))))
                 (process-send-string proc (cdr res)))
               (delete-process proc)))))
    (message "ELPAish preview server running at http://127.0.0.1:%d/ (root: %s)" server-port doc-root)))

;;;###autoload
(defun elpaish-stop-server ()
  "Stop local preview HTTP server if active."
  (interactive)
  (when (process-live-p elpaish-server-process)
    (delete-process elpaish-server-process)
    (setq elpaish-server-process nil)
    (message "ELPAish preview server stopped.")))

;;; Automated Rebuild Timer

(defconst elpaish-auto-build-intervals
  '("1 min" "5 mins" "10 mins" "30 mins" "1 hour" "2 hours" "4 hours" "8 hours" "12 hours")
  "Preset interval options for `elpaish-start-auto-build'.")

;;;###autoload
(defun elpaish-start-auto-build (interval &optional idle)
  "Start scheduled background rebuilds of the ELPA repository.
INTERVAL can be seconds or a time string (e.g. \"1 hour\").
If IDLE is non-nil, run rebuilds when Emacs is idle for INTERVAL."
  (interactive (list (if (fboundp 'annotated-completing-read)
                         (annotated-completing-read
                          (thread-last elpaish-auto-build-intervals
                            (seq-map (lambda (i) (cons i (format "Rebuild every %s" i)))))
                          :prompt "Auto-build interval: "
                          :require-match nil
                          :default "1 hour")
                       (completing-read "Auto-build interval: "
                                        elpaish-auto-build-intervals
                                        nil nil nil nil "1 hour"))
                     current-prefix-arg))
  (elpaish-stop-auto-build)
  (let* ((secs (cond
                ((numberp interval) interval)
                ((and (stringp interval) (string-match-p "^[0-9]+$" interval))
                 (string-to-number interval))
                ((stringp interval)
                 (or (timer-duration interval) (string-to-number interval)))
                (t (error "Invalid interval: %S" interval)))))
    (setq elpaish-timer
          (if idle
              (run-with-idle-timer secs t #'elpaish-build-all)
            (run-at-time secs secs #'elpaish-build-all)))
    (message "ELPA auto-build started (%s, running every %s)."
             (if idle "when idle" "periodic")
             interval)))

;;;###autoload
(defun elpaish-stop-auto-build ()
  "Stop the periodic background rebuild timer if active."
  (interactive)
  (when (timerp elpaish-timer)
    (cancel-timer elpaish-timer)
    (setq elpaish-timer nil)
    (message "ELPA auto-build timer stopped.")))

;;; UI: Tabulated List Registry View

(defvar elpaish-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'elpaish-status-refresh)
    (define-key map (kbd "b") #'elpaish-status-build-at-point)
    (define-key map (kbd "a") #'elpaish-build-all)
    (define-key map (kbd "p") #'elpaish-status-preflight-at-point)
    (define-key map (kbd "P") #'elpaish-status-preflight-all)
    (define-key map (kbd "s") #'elpaish-setup-signing)
    (define-key map (kbd "r") #'elpaish-rotate-keys)
    (define-key map (kbd "w") #'elpaish-serve-local)
    map)
  "Keymap for `elpaish-status-mode'.")

(define-derived-mode elpaish-status-mode tabulated-list-mode "ELPAish-Builder"
  "Major mode for inspecting and managing ELPAish package tracks."
  (setq tabulated-list-format
        [("Package Name" 24 t)
         ("Path / Repository" 30 t)
         ("Snapshot (elpaish)" 18 t)
         ("Stable" 10 t)
         ("Staging" 14 t)
         ("Hash" 9 nil)
         ("Delta" 10 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun elpaish-status-refresh ()
  "Refresh the package registry status table."
  (interactive)
  (setq tabulated-list-entries
        (thread-last (hash-table-values elpaish-registry)
          (seq-map (lambda (recipe)
                     (let* ((name (elpaish-recipe-name recipe))
                            (repo-dir (elpaish--resolve-repo-path recipe))
                            (exists (file-exists-p repo-dir))
                            (curr-hash (if exists (substring (elpaish--current-hash repo-dir) 0 7) "N/A"))
                            (delta (if exists
                                       (elpaish--commit-delta repo-dir (elpaish-recipe-built-hash recipe)
                                                               (elpaish--recipe-source-dir-relative recipe))
                                     "Uncloned"))
                            (snap-ver (or (elpaish-recipe-built-version-elpaish recipe) "—"))
                            (stab-ver (or (elpaish-recipe-built-version-stable recipe) "—"))
                            (stage-ver (or (elpaish-recipe-built-version-staging recipe) "—")))
                       (list name
                             (vector name
                                     (elpaish-recipe-repo recipe)
                                     snap-ver
                                     stab-ver
                                     stage-ver
                                     curr-hash
                                     (format "+%s" delta))))))))
  (tabulated-list-print t))

;;;###autoload
(defun elpaish-status-build-at-point ()
  "Build the package at point across all tracks."
  (interactive)
  (if-let* ((name (tabulated-list-get-id))
            (recipe (gethash name elpaish-registry)))
      (progn
        (dolist (track elpaish-tracks)
          (elpaish-build-package recipe track)
          (elpaish-generate-archive-contents track)
          (elpaish-generate-github-index track))
        (elpaish-generate-top-index)
        (elpaish-status-refresh)
        (message "Rebuilt %s across all tracks." name))
    (user-error "No recipe found at point")))

;;;###autoload
(defun elpaish-status-preflight-at-point ()
  "Run preflight checks for package at point."
  (interactive)
  (if-let* ((name (tabulated-list-get-id))
            (recipe (gethash name elpaish-registry)))
      (if (elpaish-preflight-package recipe)
          (message "✓ Preflight checks passed for %s" name)
        (message "✗ Preflight checks failed for %s" name))
    (user-error "No recipe found at point")))

;;;###autoload
(defun elpaish-status-preflight-all ()
  "Run preflight checks for all registered packages."
  (interactive)
  (let ((passed-count 0)
        (failed-count 0))
    (dolist (recipe (hash-table-values elpaish-registry))
      (if (elpaish-preflight-package recipe)
          (cl-incf passed-count)
        (cl-incf failed-count)))
    (message "Preflight results: %d passed, %d quarantined." passed-count failed-count)))

;;;###autoload
(defun elpaish-status ()
  "Open the ELPAish Builder management buffer."
  (interactive)
  (let ((buf (get-buffer-create "*elpaish-status*")))
    (with-current-buffer buf
      (elpaish-status-mode)
      (elpaish-status-refresh))
    (switch-to-buffer buf)))

;;;###autoload
(defun elpaish-run-checks ()
  "Run package quality checks for current repository."
  (interactive)
  (require 'elpaish-check)
  (elpaish-check-all))

(provide 'elpaish)
;;; elpaish.el ends here
