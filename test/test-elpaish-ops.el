;;; test-elpaish-ops.el --- Tests for elpaish-ops -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; Status, rebuild, upgrade, workflow and snippet operations and the MCP
;; tools over them.  Git fixtures are throwaway repositories in /tmp.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'elpaish)
(require 'elpaish-ops)
(require 'elpaish-install)

(defun elpaish-ops-test--git (dir &rest args)
  "Run git ARGS in DIR and return trimmed output, signaling on failure."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil
                            "-c" "user.name=t" "-c" "user.email=t@example.org"
                            "-c" "commit.gpgsign=false" args))
        (error "git %s failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun elpaish-ops-test--commit (dir &optional message)
  "Create an empty commit in DIR with MESSAGE; return its hash."
  (elpaish-ops-test--git dir "commit" "--allow-empty" "-q" "-m" (or message "c"))
  (elpaish-ops-test--git dir "rev-parse" "HEAD"))

(defun elpaish-ops-test--repo (parent name)
  "Create git repository NAME (branch main, one commit) under PARENT."
  (let ((dir (expand-file-name name parent)))
    (make-directory dir t)
    (elpaish-ops-test--git dir "init" "-q" "-b" "main")
    (elpaish-ops-test--commit dir "init")
    dir))

(defmacro elpaish-ops-test--with-registry (var &rest body)
  "Run BODY with an empty registry and VAR bound to a temporary directory."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (make-temp-file "elpaish-ops-" t)))
          (elpaish-registry (make-hash-table :test 'equal))
          (elpaish--resolved-repo-path-cache (make-hash-table :test 'eq))
          (elpaish-work-dir (expand-file-name "work/" ,var)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun elpaish-ops-test--register (name repo &optional built-hash)
  "Register NAME at REPO with BUILT-HASH and return its recipe."
  (elpaish-register-package name repo :branch "main")
  (let ((recipe (gethash (symbol-name name) elpaish-registry)))
    (setf (elpaish-recipe-built-hash recipe) built-hash)
    recipe))

(defun elpaish-ops-test--package (result name)
  "Return NAME's record in status RESULT."
  (seq-find (lambda (p) (equal (plist-get p :package) name))
            (append (plist-get result :packages) nil)))

;;;; HEAD without spawning git

(ert-deftest elpaish-ops-test-head-hash-matches-git ()
  "The file-based HEAD lookup agrees with git for branches, detached HEADs,
packed refs and linked worktrees."
  (elpaish-ops-test--with-registry tmp
    (let* ((repo (elpaish-ops-test--repo tmp "repo")))
      (should (equal (elpaish-ops--head-hash repo)
                     (elpaish-ops-test--git repo "rev-parse" "HEAD")))
      (elpaish-ops-test--commit repo)
      (elpaish-ops-test--git repo "pack-refs" "--all")
      (should (equal (elpaish-ops--head-hash repo)
                     (elpaish-ops-test--git repo "rev-parse" "HEAD")))
      (let ((wt (expand-file-name "wt" tmp)))
        (elpaish-ops-test--git repo "worktree" "add" "-q" "-b" "topic" wt)
        (elpaish-ops-test--commit wt)
        (should (equal (elpaish-ops--head-hash wt)
                       (elpaish-ops-test--git wt "rev-parse" "HEAD")))
        (should-not (equal (elpaish-ops--head-hash wt) (elpaish-ops--head-hash repo))))
      (elpaish-ops-test--git repo "checkout" "-q" "--detach" "HEAD~1")
      (should (equal (elpaish-ops--head-hash repo)
                     (elpaish-ops-test--git repo "rev-parse" "HEAD"))))))

(ert-deftest elpaish-ops-test-head-hash-nil-outside-a-repository ()
  "A directory without git metadata yields nil, not an error."
  (elpaish-ops-test--with-registry tmp
    (should-not (elpaish-ops--head-hash tmp))))

(ert-deftest elpaish-ops-test-git-parallel-collects-all-results ()
  "Concurrent git jobs return each key's output, nil for failures."
  (elpaish-ops-test--with-registry tmp
    (let* ((repo (elpaish-ops-test--repo tmp "repo"))
           (elpaish-parallel-jobs 2)
           (jobs (append (mapcar (lambda (i) (list i repo "rev-list" "--count" "HEAD"))
                                 (number-sequence 1 5))
                         (list (list 'bad repo "rev-list" "--count" "nosuchref..HEAD"))))
           (results (elpaish-ops--git-parallel jobs)))
      (should (= (length results) 6))
      (dolist (i (number-sequence 1 5))
        (should (equal (alist-get i results) "1")))
      (should (null (alist-get 'bad results))))))

;;;; Status

(ert-deftest elpaish-ops-test-status-reports-deltas ()
  "Status reports New, a commit count and Uncloned, and never clones."
  (elpaish-ops-test--with-registry tmp
    (let* ((fresh (elpaish-ops-test--repo tmp "fresh"))
           (behind (elpaish-ops-test--repo tmp "behind"))
           (built (elpaish-ops-test--git behind "rev-parse" "HEAD"))
           (current (elpaish-ops-test--repo tmp "current")))
      (elpaish-ops-test--commit behind)
      (elpaish-ops-test--commit behind)
      (elpaish-ops-test--register 'fresh fresh)
      (elpaish-ops-test--register 'behind behind built)
      (elpaish-ops-test--register 'current current
                                  (elpaish-ops-test--git current "rev-parse" "HEAD"))
      (elpaish-register-package 'remote "https://example.invalid/remote.git" :branch "main")
      (cl-letf (((symbol-function 'elpaish--resolve-repo-path)
                 (lambda (&rest _) (error "status must not resolve (clone or fetch) repos"))))
        (let ((res (elpaish-ops-status)))
          (should (= (plist-get res :total_packages) 4))
          (should (= (plist-get res :behind_count) 2))
          (should (equal (mapcar (lambda (p) (plist-get p :package))
                                 (append (plist-get res :packages) nil))
                         '("behind" "current" "fresh" "remote")))
          (let ((p (elpaish-ops-test--package res "behind")))
            (should (equal (plist-get p :delta) "+2"))
            (should (eq (plist-get p :behind) t))
            (should (equal (plist-get p :current_hash)
                           (substring (elpaish-ops-test--git behind "rev-parse" "HEAD") 0 7))))
          (should (equal (plist-get (elpaish-ops-test--package res "current") :delta) "+0"))
          (should (eq (plist-get (elpaish-ops-test--package res "current") :behind) :json-false))
          (should (equal (plist-get (elpaish-ops-test--package res "fresh") :delta) "+New"))
          (let ((p (elpaish-ops-test--package res "remote")))
            (should (equal (plist-get p :delta) "+Uncloned"))
            (should (eq (plist-get p :exists) :json-false))
            (should (equal (plist-get p :current_hash) "N/A")))))
      (should-not (file-exists-p elpaish-work-dir)))))

(ert-deftest elpaish-ops-test-status-filters ()
  "PACKAGES and BEHIND-ONLY narrow the rows but not the totals' meaning."
  (elpaish-ops-test--with-registry tmp
    (let* ((a (elpaish-ops-test--repo tmp "a"))
           (b (elpaish-ops-test--repo tmp "b")))
      (elpaish-ops-test--register 'a a (elpaish-ops-test--git a "rev-parse" "HEAD"))
      (elpaish-ops-test--register 'b b)
      (let ((res (elpaish-ops-status :behind-only t)))
        (should (= (plist-get res :total_packages) 2))
        (should (equal (mapcar (lambda (p) (plist-get p :package))
                               (append (plist-get res :packages) nil))
                       '("b"))))
      (let ((res (elpaish-ops-status :packages '(a))))
        (should (= (plist-get res :total_packages) 1))
        (should (= (length (plist-get res :packages)) 1))))))

(ert-deftest elpaish-ops-test-status-scopes-to-source-directory ()
  "A recipe with a source directory counts only commits touching it."
  (elpaish-ops-test--with-registry tmp
    (let* ((repo (elpaish-ops-test--repo tmp "mono"))
           (sub (expand-file-name "pkg/" repo)))
      (make-directory sub t)
      (with-temp-file (expand-file-name "a.el" sub) (insert "1"))
      (elpaish-ops-test--git repo "add" ".")
      (elpaish-ops-test--git repo "commit" "-q" "-m" "touch pkg")
      (let ((built (elpaish-ops-test--git repo "rev-parse" "HEAD")))
        (elpaish-ops-test--commit repo "elsewhere")
        (elpaish-register-package 'mono repo :branch "main" :source-dir "pkg")
        (setf (elpaish-recipe-built-hash (gethash "mono" elpaish-registry)) built)
        (let ((p (elpaish-ops-test--package (elpaish-ops-status) "mono")))
          (should (equal (plist-get p :delta) "+0"))
          (should (eq (plist-get p :behind) :json-false)))))))

(ert-deftest elpaish-ops-test-dashboard-shares-status-entries ()
  "The interactive status table is built from the same entries."
  (elpaish-ops-test--with-registry tmp
    (let* ((repo (elpaish-ops-test--repo tmp "repo"))
           (built (elpaish-ops-test--git repo "rev-parse" "HEAD")))
      (elpaish-ops-test--commit repo)
      (elpaish-ops-test--register 'repo repo built)
      (with-temp-buffer
        (elpaish-status-mode)
        (elpaish-status-refresh)
        (let ((row (cadr (assoc "repo" tabulated-list-entries))))
          (should (equal (aref row 6) "+1"))
          (should (equal (aref row 5)
                         (substring (elpaish-ops-test--git repo "rev-parse" "HEAD") 0 7))))))))

;;;; Rebuild

(ert-deftest elpaish-ops-test-rebuild-dispatch ()
  "Single, behind-only and full rebuilds call the matching builders."
  (elpaish-ops-test--with-registry tmp
    (let* ((a (elpaish-ops-test--repo tmp "a"))
           (b (elpaish-ops-test--repo tmp "b"))
           calls)
      (elpaish-ops-test--register 'a a (elpaish-ops-test--git a "rev-parse" "HEAD"))
      (elpaish-ops-test--register 'b b)
      (cl-letf (((symbol-function 'elpaish-build-single)
                 (lambda (recipe stream) (push (list 'single (elpaish-recipe-name recipe) stream) calls)))
                ((symbol-function 'elpaish-build-package)
                 (lambda (recipe stream &rest _) (push (list 'package (elpaish-recipe-name recipe) stream) calls)))
                ((symbol-function 'elpaish-build-all)
                 (lambda (stream) (push (list 'all stream) calls)))
                ((symbol-function 'elpaish-generate-archive-contents)
                 (lambda (stream &rest _) (push (list 'contents stream) calls)))
                ((symbol-function 'elpaish-generate-stream-index)
                 (lambda (stream &rest _) (push (list 'index stream) calls))))
        (let ((res (elpaish-ops-rebuild :package "a" :stream 'stable)))
          (should (equal (plist-get res :status) "ok"))
          (should (equal (plist-get res :rebuilt) ["a"]))
          (should (equal (nreverse calls) '((single "a" stable)))))
        (setq calls nil)
        (let ((res (elpaish-ops-rebuild :behind-only t)))
          (should (equal (plist-get res :rebuilt) ["b"]))
          (should (equal (nreverse calls)
                         '((package "b" snapshot) (contents snapshot) (index snapshot)))))
        (setq calls nil)
        (let ((res (elpaish-ops-rebuild)))
          (should (= (plist-get res :count) 2))
          (should (equal (nreverse calls) '((all snapshot)))))
        (setq calls nil)
        (let ((res (elpaish-ops-rebuild :behind-only t :package "all")))
          (should (equal (plist-get res :rebuilt) ["b"])))
        (should-error (elpaish-ops-rebuild :package "missing") :type 'user-error)))))

(ert-deftest elpaish-ops-test-rebuild-collects-errors ()
  "A failing package is reported without losing the others."
  (elpaish-ops-test--with-registry tmp
    (let ((a (elpaish-ops-test--repo tmp "a"))
          (b (elpaish-ops-test--repo tmp "b")))
      (elpaish-ops-test--register 'a a)
      (elpaish-ops-test--register 'b b)
      (cl-letf (((symbol-function 'elpaish-build-package)
                 (lambda (recipe &rest _)
                   (when (equal (elpaish-recipe-name recipe) "a") (error "boom"))))
                ((symbol-function 'elpaish-generate-archive-contents) #'ignore)
                ((symbol-function 'elpaish-generate-stream-index) #'ignore))
        (let ((res (elpaish-ops-rebuild :behind-only t)))
          (should (equal (plist-get res :status) "partial_error"))
          (should (equal (plist-get res :rebuilt) ["b"]))
          (should (equal (plist-get (aref (plist-get res :errors) 0) :error) "boom"))))
      (cl-letf (((symbol-function 'elpaish-build-all) (lambda (&rest _) (error "no gpg"))))
        (let ((res (elpaish-ops-rebuild)))
          (should (equal (plist-get res :status) "error"))
          (should (= (plist-get res :count) 0)))))))

;;;; Upgrade

(ert-deftest elpaish-ops-test-upgrade-only-touches-candidates ()
  "Upgrading everything processes only packages with newer versions."
  (let* ((processed nil) (refreshed 0) (hook-ran 0)
        (package-alist '((alpha) (beta) (gamma)))
        (elpaish-ops-after-upgrade-hook (list (lambda () (cl-incf hook-ran)))))
    (cl-letf (((symbol-function 'package-refresh-contents) (lambda (&rest _) (cl-incf refreshed)))
              ((symbol-function 'elpaish-install--package-up-to-date-p)
               (lambda (pkg &optional _) (eq pkg 'beta)))
              ((symbol-function 'elpaish-install--process-package)
               (lambda (pkg &optional _action _refresh)
                 (push pkg processed)
                 (pcase pkg
                   ('alpha (list :status :upgraded :pkg pkg :from "1" :to "2"))
                   ('gamma (list :status :failed :pkg pkg :error "no network")))))
              ((symbol-function 'elpaish-ops--ensure-archives)
               (lambda (refresh) (when refresh (package-refresh-contents)))))
      (let ((res (elpaish-ops-upgrade :all t :refresh t)))
        (should (equal (nreverse processed) '(alpha gamma)))
        (should (= refreshed 1))
        (should (= hook-ran 1))
        (should (equal (plist-get res :status) "partial_error"))
        (should (equal (plist-get (aref (plist-get res :upgraded) 0) :to) "2"))
        (should (equal (plist-get (aref (plist-get res :failed) 0) :error) "no network"))))))

(ert-deftest elpaish-ops-test-upgrade-named-packages-and-reinstall ()
  "Named packages are processed as given; reinstall uses package-reinstall."
  (let* ((package-alist '((alpha))) processed reinstalled (hook-ran 0)
        (elpaish-ops-after-upgrade-hook (list (lambda () (cl-incf hook-ran)))))
    (cl-letf (((symbol-function 'elpaish-ops--ensure-archives) #'ignore)
              ((symbol-function 'elpaish-install--process-package)
               (lambda (pkg &rest _) (push pkg processed) (list :status :up-to-date :pkg pkg)))
              ((symbol-function 'package-reinstall)
               (lambda (pkg) (push pkg reinstalled))))
      (let ((res (elpaish-ops-upgrade :packages '("alpha"))))
        (should (equal processed '(alpha)))
        (should (= (plist-get res :up_to_date) 1))
        (should (equal (plist-get res :upgraded) []))
        (should (= hook-ran 0))
        (should (equal (plist-get res :status) "ok")))
      (let ((res (elpaish-ops-upgrade :packages '("alpha") :reinstall t)))
        (should (equal reinstalled '(alpha)))
        (should (equal (plist-get (aref (plist-get res :upgraded) 0) :status) "reinstalled"))
        (should (= hook-ran 1))))))

(ert-deftest elpaish-ops-test-upgrade-async-returns-immediately ()
  "ASYNC hands the targets to the background pool and reports started."
  (let (args)
    (cl-letf (((symbol-function 'elpaish-ops--ensure-archives) #'ignore)
              ((symbol-function 'elpaish-install--do-packages-async)
               (lambda (&rest a) (setq args a))))
      (let ((res (elpaish-ops-upgrade :packages '("alpha" "beta") :async t :pool-size 2)))
        (should (equal (plist-get res :status) "started"))
        (should (equal (plist-get res :packages) ["alpha" "beta"]))
        (should (equal (car args) '(alpha beta)))
        (should (= (plist-get (cdr args) :pool-size) 2))))))

;;;; Workflows and snippets

(ert-deftest elpaish-ops-test-workflow-prefers-magit-dash ()
  "Workflow dispatch uses magit-dash when it is available."
  (let (called)
    (cl-letf (((symbol-function 'elpaish-ops--magit-dash-workflow-p) (lambda () t))
              ((symbol-function 'magit-dash-gh-workflow-run)
               (lambda (&rest a) (setq called a))))
      (let ((res (elpaish-ops-workflow-run nil nil "dev")))
        (should (equal (plist-get res :method) "magit-dash"))
        (should (equal (car called) elpaish-ops-workflow))
        (should (equal (nth 1 called) elpaish-github-repo-slug))
        (should (equal (nth 2 called) "dev"))))))

(ert-deftest elpaish-ops-test-workflow-falls-back-to-gh ()
  "Without magit-dash the gh CLI runs; failures carry its output."
  (let (cmd result)
    (cl-letf (((symbol-function 'elpaish-ops--magit-dash-workflow-p) (lambda () nil))
              ((symbol-function 'elpaish-ops--gh)
               (lambda (&rest args) (setq cmd args) result)))
      (setq result '(1 . "no such workflow"))
      (let ((res (elpaish-ops-workflow-run "x.yml" "o/r" "main")))
        (should (equal (plist-get res :status) "error"))
        (should (equal (plist-get res :error) "no such workflow"))
        (should (equal cmd '("workflow" "run" "x.yml" "--ref" "main" "-R" "o/r"))))
      (setq result '(0 . ""))
      (should (equal (plist-get (elpaish-ops-workflow-run) :method) "gh-cli"))
      (setq result nil)
      (should (string-match-p "Neither" (plist-get (elpaish-ops-workflow-run) :error))))))

(ert-deftest elpaish-ops-test-recipe-snippet ()
  "The snippet is a readable registration form with the right defaults."
  (let* ((elpaish-github-repo-slug "someone/elpaish")
         (text (elpaish-ops-recipe-snippet 'demo :summary "A demo" :requires '(dash)
                                           :keywords '("tools")))
         (form (read text)))
    (should (eq (car form) 'elpaish-register-package))
    (should (string-match-p "https://github.com/someone/demo.git" text))
    (should (equal (plist-get (nthcdr 3 form) :requires) '(quote (dash))))
    (should (string-match-p ":summary \"A demo\"" text))
    (should (string-match-p "https://elsewhere.example/demo"
                            (elpaish-ops-recipe-snippet "demo" :url "https://elsewhere.example/demo")))
    (should (string-match-p "github.com/me/demo"
                            (elpaish-ops-recipe-snippet "demo" :owner "me")))))

(provide 'test-elpaish-ops)
;;; test-elpaish-ops.el ends here
