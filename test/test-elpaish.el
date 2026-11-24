;;; test-elpaish.el --- ERT Tests for ELPAish package repository toolkit -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: tychoish
;; Keywords: test, elpa, package

;;; Code:

(require 'cl-lib)
(unless (macrop 'incf)
  (defmacro incf (place &optional delta)
    "Increment PLACE by DELTA (default 1)."
    (declare (debug (gv-place &optional form)))
    (if (and (symbolp place) (null delta))
        (list 'setq place (list '1+ place))
      (list 'cl-incf place delta))))

(unless (macrop 'decf)
  (defmacro decf (place &optional delta)
    "Decrement PLACE by DELTA (default 1)."
    (declare (debug (gv-place &optional form)))
    (if (and (symbolp place) (null delta))
        (list 'setq place (list '1- place))
      (list 'cl-decf place delta))))

(unless (macrop 'static-when)
  (defmacro static-when (condition &rest body)
    "A conditional compilation macro."
    (declare (indent 1) (debug t))
    (when (eval condition lexical-binding)
      (cons 'progn body))))

(require 'package)
(let ((ci-dir (expand-file-name "elpa-ci" default-directory)))
  (when (file-directory-p ci-dir)
    (setq package-user-dir ci-dir)
    (dolist (d (file-expand-wildcards (expand-file-name "*" ci-dir)))
      (when (file-directory-p d)
        (add-to-list 'load-path d)))))
(let* ((local-elpa (expand-file-name "elpa" default-directory))
       (ci-elpa (expand-file-name "elpa-ci" default-directory))
       (target-user-dir (cond
                         ((file-directory-p ci-elpa) ci-elpa)
                         ((file-directory-p local-elpa) local-elpa)
                         (t (and (boundp 'package-user-dir) package-user-dir))))
       (package-user-dir (or target-user-dir (expand-file-name "elpa" default-directory))))
  (package-initialize))
(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name (expand-file-name "test/dummy" default-directory))))
       (root-dir (file-name-directory (directory-file-name this-dir)))
       (gen-dir (expand-file-name "../gen.el" root-dir))
       (logx-dir (expand-file-name "../logx" root-dir))
       (pkg-dir (expand-file-name "../pkg" this-dir)))
  (when (file-directory-p gen-dir)
    (setq load-path (cons (file-name-as-directory gen-dir) load-path)))
  (when (file-directory-p logx-dir)
    (setq load-path (cons (file-name-as-directory logx-dir) load-path)))
  (when (file-directory-p pkg-dir)
    (setq load-path (cons (file-name-as-directory pkg-dir) load-path))))
(require 'ert)
(require 'compat)
(require 'gen nil t)
(require 'logx)
(require 'elpaish)
(require 'elpaish-recipes)
(require 'elpaish-check)
(require 'elpaish-website)
(require 'elpaish-async)
(require 'elpaish-install)
(require 'elpaish-keyring)
(require 'elpaish-signing-keys)
(require 'url)

(defmacro elpaish-test-with-temp-env (&rest body)
  "Execute BODY within an isolated temporary registry, work, and output directory."
  `(let* ((temp-dir (make-temp-file "elpaish-test-" t))
          (elpaish-output-dir (expand-file-name "public/" temp-dir))
          (elpaish-work-dir (expand-file-name "repos/" temp-dir))
          (elpaish-release-mode 'all)
          (elpaish-base-url "https://example.com/elpaish/")
          (elpaish-default-branch "main")
          (elpaish-packages-file "packages.el")
          (elpaish-packages-files '("packages.el"))
          (elpaish-modus-theme 'modus-operandi)
          (orig-registry-snapshot (when (hash-table-p elpaish-registry)
                                    (copy-hash-table elpaish-registry)))
          (elpaish-registry (make-hash-table :test 'equal))
          (elpaish-sign-packages nil)
          (elpaish-run-preflight nil)
          (elpaish-gpg-key nil)
          (elpaish-gpg-passphrase "")
          (elpaish-timer nil)
          (elpaish-server-process nil)
          (elpaish--resolved-repo-path-cache (make-hash-table :test 'eq))
          (elpaish-check-buffer-name (format " *elpaish-test-check-%s*" (make-temp-name "")))
          (elpaish-install-bootstrap-finished-functions
           (remove #'elpaish-install--on-bootstrap-finished
                   elpaish-install-bootstrap-finished-functions))
          (package-archives (copy-sequence package-archives))
          (package-quickstart-file (expand-file-name "package-quickstart.el" temp-dir))
          (package-check-signature nil)
          (logx-async-mode nil)
          (noninteractive t)
          (elpaish-install--active-cancel-handle nil))
     (unwind-protect
         (progn ,@body)
       (when (functionp elpaish-install--active-cancel-handle)
         (funcall elpaish-install--active-cancel-handle))
       (setq elpaish-install--active-cancel-handle nil)
      (when (elpaish-server-running-p)
        (elpaish-stop-server))
       (when (timerp elpaish-timer)
         (cancel-timer elpaish-timer))
       (when orig-registry-snapshot
         (setq elpaish-registry orig-registry-snapshot))
       (delete-directory temp-dir t))))
(defun elpaish-test-create-dummy-pkg (dir name version summary &optional reqs)
  "Create a dummy package file in DIR."
  (make-directory dir t)
  (let ((file (expand-file-name (concat name ".el") dir)))
    (with-temp-file file
      (insert (format ";;; %s.el --- %s -*- lexical-binding: t; -*-\n\n" name summary))
      (when version
        (insert ";; Version: " version "\n"))
      (insert (format ";; Package-Requires: %S\n" (or reqs '((emacs "24.1")))))
      (insert ";; Keywords: test, tools\n")
      (insert ";; URL: https://github.com/tychoish/" name "\n\n")
      (insert ";;; Commentary:\n;; Test commentary.\n\n;;; Code:\n\n")
      (insert "(provide '" name ")\n")
      (insert (format ";;; %s.el ends here\n" name)))))

;;; Tests

(ert-deftest elpaish-test-registration ()
  "Test package registration with symbol, string, and recipe attributes."
  (elpaish-test-with-temp-env
   (elpaish-register-package 'pkg-a "/path/to/a"
                                  :summary "Package A"
                                  :url "https://github.com/test/pkg-a"
                                  :keywords '("convenience"))
   (elpaish-register-package "pkg-b" "/path/to/b"
                                  :branch "develop"
                                  :files '("*.el" "src/*.el")
                                  :test-dir "test"
                                  :preflight-skip '(checkdoc))

   (let ((recipe-a (gethash "pkg-a" elpaish-registry))
         (recipe-b (gethash "pkg-b" elpaish-registry)))
     (should recipe-a)
     (should (equal (elpaish-recipe-name recipe-a) "pkg-a"))
     (should (equal (elpaish-recipe-branch recipe-a) "main"))
     (should (equal (elpaish-recipe-summary recipe-a) "Package A"))
     (should (equal (elpaish-recipe-keywords recipe-a) '("convenience")))

     (should recipe-b)
     (should (equal (elpaish-recipe-branch recipe-b) "develop"))
     (should (equal (elpaish-recipe-files recipe-b) '("*.el" "src/*.el")))
     (should (equal (elpaish-recipe-test-directory-path recipe-b) "test"))
     (should (equal (elpaish-recipe-preflight-skip recipe-b) '(checkdoc))))))

(ert-deftest elpaish-test-registration-remote-urls ()
  "Test package registration preserving remote Git URLs without local file expansion."
  (elpaish-test-with-temp-env
   (elpaish-register-package 'pkg-https "https://github.com/tychoish/elpaish.git")
   (elpaish-register-package 'pkg-http "http://example.com/repo.git")
   (elpaish-register-package 'pkg-ssh "git@github.com:tychoish/elpaish.git")
   (elpaish-register-package 'pkg-git "git://github.com/tychoish/elpaish.git")
   (elpaish-register-package 'pkg-local "relative/path/to/pkg")

   (let ((rec-https (gethash "pkg-https" elpaish-registry))
         (rec-http (gethash "pkg-http" elpaish-registry))
         (rec-ssh (gethash "pkg-ssh" elpaish-registry))
         (rec-git (gethash "pkg-git" elpaish-registry))
         (rec-local (gethash "pkg-local" elpaish-registry)))
     (should (equal (elpaish-recipe-repository-path rec-https) "https://github.com/tychoish/elpaish.git"))
     (should (equal (elpaish-recipe-repository-path rec-http) "http://example.com/repo.git"))
     (should (equal (elpaish-recipe-repository-path rec-ssh) "git@github.com:tychoish/elpaish.git"))
     (should (equal (elpaish-recipe-repository-path rec-git) "git://github.com/tychoish/elpaish.git"))
     (should (equal (elpaish-recipe-repository-path rec-local) (expand-file-name "relative/path/to/pkg"))))))

(ert-deftest elpaish-test-pure-date-version ()
  "Test pure UTC date versioning on elpaish snapshot track."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "date-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "date-pkg" "0.1.0" "Date Test")
     (let ((ver (elpaish--get-snapshot-version pkg-dir)))
       (should (stringp ver))
       ;; Should match pure date format YYYYMMDD.HHMMSS (no header version prefix)
       (should (string-match-p "\\`[0-9]\\{8\\}\\.[0-9]\\{6\\}\\'" ver))
       (should (version-to-list ver))))))

(ert-deftest elpaish-test-stable-semver-tag-filtering ()
  "Test clean semver tag resolution and pre-release tag exclusion on elpaish-stable."
  ;; Test clean semver predicate
  (should (elpaish--stable-tag-p "v1.2.3"))
  (should (elpaish--stable-tag-p "1.0.0"))
  (should (elpaish--stable-tag-p "v2.1.0.4"))
  (should-not (elpaish--stable-tag-p "v1.2.0-rc1"))
  (should-not (elpaish--stable-tag-p "v2.0.0-beta.2"))
  (should-not (elpaish--stable-tag-p "v0.9.0-alpha"))
  (should-not (elpaish--stable-tag-p "v1.0.0-dev"))
  (should-not (elpaish--stable-tag-p "untagged-commit"))

  ;; Test clean semver string
  (should (equal (elpaish--clean-semver-string "v1.2.3") "1.2.3"))
  (should (equal (elpaish--clean-semver-string "1.2.3") "1.2.3")))

(ert-deftest elpaish-test-staging-version-derivation ()
  "Test pre-release tag normalization and git describe versioning on elpaish-staging."
  ;; Pre-release tags normalized for version-to-list
  (should (equal (elpaish--normalize-staging-version "v1.2.0-rc1") "1.2.0.rc1"))
  (should (equal (elpaish--normalize-staging-version "1.2.0-beta2") "1.2.0.beta2"))
  (should (equal (elpaish--normalize-staging-version "v2.0.0-pre1") "2.0.0.pre1"))
  ;; Ensure all normalized versions parse cleanly into version lists
  (should (version-to-list (elpaish--normalize-staging-version "v1.2.0-rc1")))
  (should (version-to-list (elpaish--normalize-staging-version "1.2.0-beta2")))
  (should (version-to-list (elpaish--normalize-staging-version "v2.0.0-pre1"))))

(ert-deftest elpaish-test-in-memory-version-injection ()
  "Test in-memory ;; Version: header injection without modifying source files."
  (with-temp-buffer
    (insert ";;; foo.el --- Test -*- lexical-binding: t; -*-\n\n;; Author: Test\n(provide 'foo)\n")
    (elpaish--inject-version-header "20260817.143022")
    (should (search-backward ";; Version: 20260817.143022" nil t)))
  (with-temp-buffer
    (insert ";;; foo.el --- Test -*- lexical-binding: t; -*-\n;; Version: 1.0.0\n;; Author: Test\n")
    (elpaish--inject-version-header "3.1.4")
    (should (search-backward ";; Version: 3.1.4" nil t))))

(ert-deftest elpaish-test-single-file-package-build ()
  "Test building single-file package on elpaish track."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "single-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "single-pkg" "1.0.0" "Single File Test")
     (elpaish-register-package 'single-pkg pkg-dir)
     (let* ((recipe (gethash "single-pkg" elpaish-registry))
            (dest (elpaish-build-package recipe 'elpaish)))
       (should dest)
       (should (file-exists-p dest))
       (should (string-suffix-p ".el" dest))
       (should (elpaish-recipe-built-version-snapshot recipe))
       (with-temp-buffer
         (insert-file-contents dest)
         (should (search-forward ";; Version:" nil t)))))))

(ert-deftest elpaish-test-multi-file-tar-build ()
  "Test multi-file package tar packaging and <pkg>-pkg.el descriptor generation."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "multi-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "multi-pkg" "1.0.0" "Multi File Test" '((emacs "28.1") (seq "2.0")))
     (with-temp-file (expand-file-name "multi-pkg-extra.el" pkg-dir)
       (insert ";;; multi-pkg-extra.el -*- lexical-binding: t; -*-\n(provide 'multi-pkg-extra)\n"))
     (elpaish-register-package 'multi-pkg pkg-dir
                                    :files '("*.el")
                                    :url "https://github.com/tychoish/multi-pkg"
                                    :keywords '("tools" "convenience"))
     (let* ((recipe (gethash "multi-pkg" elpaish-registry))
            (dest (elpaish-build-package recipe 'elpaish)))
       (should dest)
       (should (file-exists-p dest))
       (should (string-suffix-p ".tar" dest))
       (should (eq (elpaish-recipe-built-type recipe) 'tar))))))

(ert-deftest elpaish-test-archive-contents-generation ()
  "Test `archive-contents' generation across tracks."
  (elpaish-test-with-temp-env
   (let ((pkg1 (expand-file-name "pkg1" temp-dir))
         (pkg2 (expand-file-name "pkg2" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg1 "pkg1" "1.0.0" "First Package")
     (elpaish-test-create-dummy-pkg pkg2 "pkg2" "2.0.0" "Second Package")
     (elpaish-register-package 'pkg1 pkg1)
     (elpaish-register-package 'pkg2 pkg2)

     (elpaish-build-package (gethash "pkg1" elpaish-registry) 'elpaish)
     (elpaish-build-package (gethash "pkg2" elpaish-registry) 'elpaish)

     (let ((ac-file (elpaish-generate-archive-contents 'elpaish)))
       (should (file-exists-p ac-file))
       (with-temp-buffer
         (insert-file-contents ac-file)
         (let ((data (read (current-buffer))))
           (should (eq (car data) 1))
           (should (assoc 'pkg1 (cdr data)))
           (should (assoc 'pkg2 (cdr data)))
           (let ((entry (cdr (assoc 'pkg1 (cdr data)))))
             (should (vectorp entry))
             ;; Entry: [VER REQS SUMMARY KIND EXTRAS]
             (should (listp (aref entry 0)))
             (should (stringp (aref entry 2)))
             (should (eq (aref entry 3) 'single)))))))))

(ert-deftest elpaish-test-html-indexes-and-landing-page ()
  "Test generation of track catalogs and top-level landing page."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "catalog-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "catalog-pkg" "1.0.0" "Catalog Package")
     (elpaish-register-package 'catalog-pkg pkg-dir)
     (elpaish-build-package (gethash "catalog-pkg" elpaish-registry) 'elpaish)

     (elpaish-generate-stream-index 'elpaish)
     (elpaish-generate-top-index)

     (let ((stream-index (expand-file-name "snapshot/index.html" elpaish-output-dir))
           (about-page (expand-file-name "about.html" elpaish-output-dir))
           (top-index (expand-file-name "index.html" elpaish-output-dir)))
       (should (file-exists-p stream-index))
       (should (file-exists-p about-page))
       (should (file-exists-p top-index))
       (with-temp-buffer
         (insert-file-contents stream-index)
         (goto-char (point-min))
         (should (search-forward "catalog-pkg" nil t))
         (goto-char (point-min))
         (should (search-forward "ELPAish Repository — (snapshot)" nil t))
         (goto-char (point-min))
         (should (search-forward "Source Sans" nil t))
         (goto-char (point-min))
         (should (search-forward "Source Code Pro" nil t))
         (goto-char (point-min))
         (should (search-forward "max-width:1240px" nil t))
         (goto-char (point-min))
         (should (search-forward "font-size:20px" nil t))
         (goto-char (point-min))
         (should (search-forward "table-wrapper" nil t))
         (goto-char (point-min))
         (should (search-forward "SHA256 Checksum" nil t))
         (goto-char (point-min))
         (should (search-forward "pkg-name-cell" nil t)))
       (with-temp-buffer
         (insert-file-contents about-page)
         (goto-char (point-min))
         (should (search-forward "About ELPAish" nil t))
         (goto-char (point-min))
         (should (search-forward "Overview" nil t)))
       (with-temp-buffer
         (insert-file-contents top-index)
         (goto-char (point-min))
         (should (search-forward "Browse Snapshot Packages" nil t))
         (goto-char (point-min))
         (should (search-forward "Browse Stable Packages" nil t))
         (goto-char (point-min))
         (should (search-forward "Browse Staging Packages" nil t))
         (goto-char (point-min))
         (should (search-forward "Source Sans" nil t))
         (goto-char (point-min))
         (should (search-forward "max-width:1240px" nil t)))))))

(ert-deftest elpaish-test-modus-operandi-html-styling ()
  "Test Modus Operandi accessible color palette in generated HTML."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "annotated-completing-read" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "annotated-completing-read" "1.0.0" "Long Package Name Test")
     (elpaish-register-package 'annotated-completing-read pkg-dir)
     (elpaish-build-package (gethash "annotated-completing-read" elpaish-registry) 'elpaish)
     (elpaish-generate-stream-index 'elpaish)
     (elpaish-generate-top-index)

     (let ((stream-index (expand-file-name "snapshot/index.html" elpaish-output-dir))
           (top-index (expand-file-name "index.html" elpaish-output-dir)))
       (with-temp-buffer
         (insert-file-contents stream-index)
         (goto-char (point-min))
         (should (search-forward (format "color:%s" (elpaish-css-color 'fg-link)) nil t))
         (goto-char (point-min))
         (should (search-forward "annotated-completing-read" nil t)))))))

(ert-deftest elpaish-test-build-all-streams ()
  "Test building all repository streams with `elpaish-build-all`."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "all-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "all-pkg" "1.0.0" "All Stream Test")
     (elpaish-register-package 'all-pkg pkg-dir)

     (elpaish-build-all 'all)

     ;; Snapshot stream should exist
     (should (file-exists-p (expand-file-name "snapshot/archive-contents" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "snapshot/index.html" elpaish-output-dir)))
     ;; Staging stream should exist
     (should (file-exists-p (expand-file-name "staging/archive-contents" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "staging/index.html" elpaish-output-dir)))
     ;; Top index should exist
     (should (file-exists-p (expand-file-name "index.html" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "about.html" elpaish-output-dir))))))

(ert-deftest elpaish-test-preflight-gate-quarantine ()
  "Test that packages failing preflight validation are quarantined and omitted."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "broken-pkg" temp-dir))
         (elpaish-run-preflight t))
     (make-directory pkg-dir t)
     ;; Create package with unbalanced parentheses to trigger check-parens failure
     (with-temp-file (expand-file-name "broken-pkg.el" pkg-dir)
       (insert ";;; broken-pkg.el --- Broken -*- lexical-binding: t; -*-\n")
       (insert "(defun broken (x (missing-close-paren)\n")
       (insert "(provide 'broken-pkg)\n"))

     (elpaish-register-package 'broken-pkg pkg-dir)
     (let* ((recipe (gethash "broken-pkg" elpaish-registry))
            (built (elpaish-build-package recipe 'elpaish)))
       ;; Build should fail and return nil due to preflight quarantine
       (should-not built)
       (should-not (elpaish-recipe-built-version-snapshot recipe))))))
(ert-deftest elpaish-test-timer-controls ()
  "Test starting and stopping auto-build background timer."
  (elpaish-test-with-temp-env
   (unwind-protect
       (progn
         (elpaish-start-auto-build "3600")
         (should (timerp elpaish-timer))
         (elpaish-stop-auto-build)
         (should-not elpaish-timer)
         (elpaish-start-auto-build "5 mins" t)
         (should (timerp elpaish-timer))
         (elpaish-stop-auto-build))
     (elpaish-stop-auto-build))))

(ert-deftest elpaish-test-status-ui ()
  "Test status buffer creation and stream column population."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "ui-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "ui-pkg" "0.5.0" "UI Test")
     (elpaish-register-package 'ui-pkg pkg-dir)

     (elpaish-status)
     (let ((buf (get-buffer "*elpaish-status*")))
       (should buf)
       (with-current-buffer buf
         (should (eq major-mode 'elpaish-status-mode))
         (should (search-forward "ui-pkg" nil t))))
     (when (get-buffer "*elpaish-status*")
       (kill-buffer "*elpaish-status*")))))

(ert-deftest elpaish-test-local-preview-server-and-install ()
  "Test local HTTP preview server, package-archives fetching, and installation."
  (elpaish-test-with-temp-env
   (let* ((pkg-dir (expand-file-name "preview-pkg" temp-dir))
          (test-port 18889))
     (elpaish-test-create-dummy-pkg pkg-dir "preview-pkg" "1.0.0" "Preview Server Test")
     (elpaish-register-package 'preview-pkg pkg-dir)
     (elpaish-build-all 'all)

     (unwind-protect
         (progn
           (elpaish-serve-local test-port elpaish-output-dir)
          (should (elpaish-server-running-p))

           ;; Verify HTTP request to preview server
           (let ((url-buf (url-retrieve-synchronously (format "http://127.0.0.1:%d/snapshot/archive-contents" test-port) t t 5)))
             (should url-buf)
             (with-current-buffer url-buf
               (goto-char (point-min))
               (should (search-forward "200 OK" nil t))
               (should (search-forward "preview-pkg" nil t))
               (kill-buffer url-buf))))
       (elpaish-stop-server)))))

(ert-deftest elpaish-test-gpg-signing-pipeline ()
  "Test GPG signing key/passphrase resolution and signature generation."
  (elpaish-test-with-temp-env
   (let ((elpaish-sign-packages t)
         (elpaish-gpg-key "TESTKEY123")
         (elpaish-gpg-passphrase "SECRET123"))
     (should (equal (elpaish--get-signing-key) "TESTKEY123"))
     (should (equal (elpaish--get-signing-passphrase) "SECRET123"))

     ;; Test environment variable overrides
     (setenv "ELPAISH_KEY_ID" "ENVKEY456")
     (setenv "ELPAISH_GPG_PASSPHRASE" "ENVPASS456")
     (setq elpaish-gpg-key nil
           elpaish-gpg-passphrase nil)
     (should (equal (elpaish--get-signing-key) "ENVKEY456"))
     (should (equal (elpaish--get-signing-passphrase) "ENVPASS456"))
     (setenv "ELPAISH_KEY_ID" nil)
     (setenv "ELPAISH_GPG_PASSPHRASE" nil)

     ;; Mock gpg CLI execution
     (let ((dummy-file (expand-file-name "test.el" temp-dir)))
       (with-temp-file dummy-file (insert "test"))
       (cl-letf (((symbol-function 'call-process-region)
                  (lambda (_start _end _program &optional _delete _destination _display &rest args)
                    (let ((out-file (cadr (member "--output" args))))
                      (when out-file
                        (with-temp-file out-file (insert "MOCK SIGNATURE"))))
                    0)))
         (elpaish--sign-file dummy-file)
         (should (file-exists-p (concat dummy-file ".sig"))))))))

(ert-deftest elpaish-test-key-rotation-and-revocation ()
  "Test GPG key rotation and emergency revocation publishing."
  (elpaish-test-with-temp-env
   (let ((gpg-bin (executable-find "gpg")))
     (if (not gpg-bin)
         (message "Skipping key rotation test: gpg not found in PATH")
       (cl-letf (((symbol-function 'call-process)
                  (lambda (_program &optional _infile _destination _display &rest args)
                    (when (member "--gen-revoke" args)
                      (let ((out-file (cadr (member "--output" args))))
                        (when out-file
                          (make-directory (file-name-directory out-file) t)
                          (with-temp-file out-file (insert "MOCK REV")))))
                    0))
                 ((symbol-function 'call-process-region) (lambda (&rest _) 0)))
         ;; Test rotation
         (elpaish-rotate-keys :master-key-id "MASTERKEY" :output-dir elpaish-output-dir)
         ;; Test revocation publishing
         (elpaish-revoke-key "MASTERKEY" elpaish-output-dir)
         (should (file-exists-p (expand-file-name "elpaish.rev.asc" elpaish-output-dir))))))))
(ert-deftest elpaish-test-empty-revocation-file-export ()
  "Test that an empty elpaish.rev.asc file is created if no revocation cert exists."
  (elpaish-test-with-temp-env
   (let ((rev-file (expand-file-name "elpaish.rev.asc" elpaish-output-dir)))
     (should-not (file-exists-p rev-file))
     (elpaish-export-keyring elpaish-output-dir)
     (should (file-exists-p rev-file))
     (should (zerop (file-attribute-size (file-attributes rev-file)))))))

(ert-deftest elpaish-test-gpg-signing-envvar-testing-value ()
  "Test handling ELPAISH_SIGNING_KEY set to dummy __testing_value from pages.yml."
  (elpaish-test-with-temp-env
   (let ((orig-key (getenv "ELPAISH_SIGNING_KEY"))
         (orig-gpg-key elpaish-gpg-key))
     (unwind-protect
         (progn
           (setenv "ELPAISH_SIGNING_KEY" "__testing_value")
           (setq elpaish-gpg-key nil)

           ;; Should not treat __testing_value as a key ID
           (should-not (equal (elpaish--get-signing-key) "__testing_value"))

           ;; Initializing from dummy env var should handle invalid armor gracefully
           (let ((res (elpaish-init-signing-from-env)))
             (should (or (null res) (stringp res)))))
       (setenv "ELPAISH_SIGNING_KEY" orig-key)
       (setq elpaish-gpg-key orig-gpg-key)))))

(ert-deftest elpaish-test-ci-workflow-env-signing-key ()
  "Verify reading and handling ELPAISH_SIGNING_KEY directly from workflow environment."
  (let ((key-env (getenv "ELPAISH_SIGNING_KEY")))
    (if (equal key-env "__testing_value")
        (progn
          (should (equal (getenv "ELPAISH_SIGNING_KEY") "__testing_value"))
          (should-not (equal (elpaish--get-signing-key) "__testing_value"))
          (let ((res (elpaish-init-signing-from-env)))
            (should (or (null res) (stringp res)))))
      ;; In local environments where the CI envvar isn't set, verify simulated state
      (elpaish-test-with-temp-env
       (let ((orig (getenv "ELPAISH_SIGNING_KEY")))
         (unwind-protect
             (progn
               (setenv "ELPAISH_SIGNING_KEY" "__testing_value")
               (should (equal (getenv "ELPAISH_SIGNING_KEY") "__testing_value"))
               (should-not (equal (elpaish--get-signing-key) "__testing_value"))
               (let ((res (elpaish-init-signing-from-env)))
                 (should (or (null res) (stringp res)))))
           (setenv "ELPAISH_SIGNING_KEY" orig)))))))
(ert-deftest elpaish-test-gpg-signing-envvar-real-key ()
  "Test complete GPG signing workflow using ELPAISH_SIGNING_KEY armored key environment variable."
  (elpaish-test-with-temp-env
   (let ((dummy-armor "-----BEGIN PGP PRIVATE KEY BLOCK-----\nVersion: Test\n\nmQGNBF...\n-----END PGP PRIVATE KEY BLOCK-----\n")
         (orig-armor (getenv "ELPAISH_SIGNING_KEY"))
         (orig-pass (getenv "ELPAISH_GPG_PASSPHRASE")))
     (unwind-protect
         (progn
           (setenv "ELPAISH_SIGNING_KEY" dummy-armor)
           (setenv "ELPAISH_GPG_PASSPHRASE" "test-passphrase")
           (setq elpaish-sign-packages nil
                 elpaish-gpg-key nil)

           ;; Mock gpg import and key detection
           (cl-letf (((symbol-function 'call-process-region)
                      (lambda (_start _end _program &optional _delete _destination _display &rest args)
                        (let ((out-file (cadr (member "--output" args))))
                          (when out-file
                            (with-temp-file out-file (insert "MOCK SIGNATURE"))))
                        0))
                     ((symbol-function 'elpaish--detect-secret-key-id)
                      (lambda () "MOCK_KEY_FPR_12345")))

             (elpaish-init-signing-from-env)
             (should elpaish-sign-packages)
             (should (equal elpaish-gpg-key "MOCK_KEY_FPR_12345"))
             (should (equal elpaish-gpg-passphrase "test-passphrase"))

             ;; Build package and archive-contents
             (let ((pkg-dir (expand-file-name "env-signed-pkg" temp-dir)))
               (elpaish-test-create-dummy-pkg pkg-dir "env-signed-pkg" "1.0.0" "Env Signed Test")
               (elpaish-register-package 'env-signed-pkg pkg-dir)
               (let ((dest (elpaish-build-package (gethash "env-signed-pkg" elpaish-registry) 'elpaish)))
                 (should dest)
                 (should (file-exists-p (concat dest ".sig")))
                 (let ((ac-file (elpaish-generate-archive-contents 'elpaish)))
                   (should (file-exists-p (concat ac-file ".sig"))))))))
       (setenv "ELPAISH_SIGNING_KEY" orig-armor)
       (setenv "ELPAISH_GPG_PASSPHRASE" orig-pass)))))

(ert-deftest elpaish-test-headless-encrypted-key-signing ()
  "Test headless non-interactive signing with a passphrase-protected encrypted key."
  (elpaish-test-with-temp-env
   (let ((dummy-file (expand-file-name "headless-payload.txt" temp-dir))
         (captured-stdin nil)
         (captured-args nil))
     (with-temp-file dummy-file (insert "headless encrypted payload content"))
     (cl-letf (((symbol-function 'call-process-region)
                (lambda (start end _program &optional _delete _destination _display &rest args)
                  (setq captured-stdin (buffer-substring-no-properties start end))
                  (setq captured-args args)
                  (let ((out-file (cadr (member "--output" args))))
                    (when out-file
                      (with-temp-file out-file (insert "MOCK HEADLESS SIGNATURE"))))
                  0)))
       (let ((sig (elpaish-sign-file-headless dummy-file :key-id "ENCRYPTED_KEY_ID" :passphrase "mysecretpass")))
         (should sig)
         (should (file-exists-p sig))
         ;; Verify passphrase was piped over stdin non-interactively
         (should (string-match-p "mysecretpass" captured-stdin))
         ;; Verify CLI args enforce batch loopback mode
         (should (member "--batch" captured-args))
         (should (member "--pinentry-mode" captured-args))
         (should (member "loopback" captured-args))
         (should (member "--passphrase-fd" captured-args))
         (should (member "0" captured-args))
         (should (member "ENCRYPTED_KEY_ID" captured-args)))))))

(ert-deftest elpaish-test-preflight-skip-options ()
  "Test preflight gate skipping specific checks and skipping all."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "skip-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "skip-pkg" "1.0.0" "Skip Test")
     ;; Add invalid docstring to trigger checkdoc warning
     (with-temp-buffer
       (insert ";;; skip-pkg.el --- invalid checkdoc -*- lexical-binding: t; -*-\n\n(defun skip-pkg-foo ()\n  \"Missing period in docstring\"\n  nil)\n\n(provide 'skip-pkg)\n;;; skip-pkg.el ends here\n")
       (write-region (point-min) (point-max) (expand-file-name "skip-pkg.el" pkg-dir)))

     ;; Test skipping checkdoc & package-lint explicitly
     (let ((res (elpaish-check-package pkg-dir :skip-checks '(checkdoc package-lint))))
       (should (plist-get res :passed)))

     ;; Test skipping all checks
     ;; Test skipping all checks
     (let ((res (elpaish-check-package pkg-dir :skip-checks t)))
       (should (plist-get res :passed)))

     ;; Test registered package with :preflight-skip t
     (let ((elpaish-registry (make-hash-table :test 'equal)))
       (elpaish-register-package "skip-pkg" pkg-dir :preflight-skip t)
       (let ((rec (gethash "skip-pkg" elpaish-registry)))
         (should (eq (elpaish-recipe-preflight-skip rec) t))
         (should (elpaish-preflight-package rec))
         (should (plist-get (elpaish-check-package pkg-dir) :passed)))))))
(ert-deftest elpaish-test-preflight-sibling-dependency-resolution ()
  "Test preflight handles optional sibling requires whose dependencies must be installed."
  (elpaish-test-with-temp-env
   (let ((pkg-a-dir (expand-file-name "pkg-a" temp-dir))
         (pkg-b-dir (expand-file-name "pkg-b" temp-dir))
         (elpa-dir (expand-file-name "elpa-mock" temp-dir))
         (installed-pkgs nil)
         (elpaish-run-preflight t))
     (make-directory pkg-a-dir t)
     (make-directory pkg-b-dir t)
     (make-directory elpa-dir t)
     ;; Create pkg-a which optionally requires pkg-b
     (with-temp-file (expand-file-name "pkg-a.el" pkg-a-dir)
       (insert ";;; pkg-a.el --- Pkg A -*- lexical-binding: t; -*-\n")
       (insert ";;; Version: 1.0.0\n")
       (insert ";;; URL: https://example.com/pkg-a\n")
       (insert ";;; Commentary:\n;; Pkg A commentary.\n")
       (insert ";;; Code:\n")
       (insert "(require 'pkg-b nil t)\n")
       (insert "(defun pkg-a-fn () t)\n")
       (insert "(provide 'pkg-a)\n")
       (insert ";;; pkg-a.el ends here\n"))

     ;; Create pkg-b which requires external agent-shell
     (with-temp-file (expand-file-name "pkg-b.el" pkg-b-dir)
       (insert ";;; pkg-b.el --- Pkg B -*- lexical-binding: t; -*-\n")
       (insert ";;; Version: 1.0.0\n")
       (insert ";;; URL: https://example.com/pkg-b\n")
       (insert ";;; Package-Requires: ((agent-shell \"1.0\"))\n")
       (insert ";;; Commentary:\n;; Pkg B commentary.\n")
       (insert ";;; Code:\n")
       (insert "(require 'agent-shell)\n")
       (insert "(defun pkg-b-fn () t)\n")
       (insert "(provide 'pkg-b)\n")
       (insert ";;; pkg-b.el ends here\n"))

     (let ((recipe-a (elpaish-register-package 'pkg-a pkg-a-dir)))
       (elpaish-register-package 'pkg-b pkg-b-dir :requires '((agent-shell "1.0")))

       (cl-letf (((symbol-function 'package-installed-p)
                  (lambda (p &rest _) (member p installed-pkgs)))
                 ((symbol-function 'package-refresh-contents) #'ignore)
                 ((symbol-function 'package-initialize) #'ignore)
                 ((symbol-function 'package-install)
                  (lambda (p &rest _)
                    (push p installed-pkgs)
                    ;; Create mock installed package directory in package-user-dir
                    (let ((dep-dir (expand-file-name (format "%s-1.0" p) elpa-dir)))
                      (make-directory dep-dir t)
                      (with-temp-file (expand-file-name (format "%s.el" p) dep-dir)
                        (insert (format ";;; %s.el --- Mock -*- lexical-binding: t; -*-\n(provide '%s)\n" p p)))))))
         (let ((package-user-dir elpa-dir))
           ;; Preflight pkg-a. It should ensure dependencies for all registered packages (including pkg-b -> agent-shell).
           (should (elpaish-preflight-package recipe-a))
           (should (member 'agent-shell installed-pkgs))))))))
(ert-deftest elpaish-test-staging-version-edge-cases ()
  "Test edge cases in version normalization and staging version derivation."
  (should (equal (elpaish--normalize-staging-version "v1.2.0-4-gabcdef") "1.2.0.4"))
  (should (equal (elpaish--normalize-staging-version "1.2.0-rc.1") "1.2.0.rc1"))
  (should (version-to-list (elpaish--normalize-staging-version "v1.2.0-4-gabcdef")))
  (should (version-to-list (elpaish--normalize-staging-version "1.2.0-rc.1"))))

(ert-deftest elpaish-test-http-server-edge-cases ()
  "Test HTTP preview server MIME types and 404/400/HEAD responses."
  (should (equal (elpaish--http-mime-type "foo.sig") "application/pgp-signature"))
  (should (equal (elpaish--http-mime-type "foo.asc") "application/pgp-keys"))
  (should (equal (elpaish--http-mime-type "foo.gpg") "application/pgp-keys"))
  (should (equal (elpaish--http-mime-type "foo.tar") "application/x-tar"))
  (should (equal (elpaish--http-mime-type "foo.html") "text/html; charset=utf-8"))

  (elpaish-test-with-temp-env
   (let ((doc-root temp-dir)
         (test-port (+ 18000 (random 2000))))
     (with-temp-file (expand-file-name "test.html" doc-root) (insert "<h1>Hello</h1>"))

     ;; 200 OK
     (let ((res (elpaish--handle-http-request "GET /test.html HTTP/1.1\r\n" doc-root)))
       (should (string-prefix-p "HTTP/1.1 200 OK" (car res)))
       (should (equal (cdr res) "<h1>Hello</h1>")))

     ;; HEAD request (200 OK headers, empty body)
     (let ((res (elpaish--handle-http-request "HEAD /test.html HTTP/1.1\r\n" doc-root)))
       (should (string-prefix-p "HTTP/1.1 200 OK" (car res)))
       (should (equal (cdr res) "")))

     ;; 404 Not Found
     (let ((res (elpaish--handle-http-request "GET /nonexistent.el HTTP/1.1\r\n" doc-root)))
       (should (string-prefix-p "HTTP/1.1 404 Not Found" (car res))))

     ;; 400 Bad Request
     (let ((res (elpaish--handle-http-request "POST /test.html HTTP/1.1\r\n" doc-root)))
       (should (string-prefix-p "HTTP/1.1 400 Bad Request" (car res))))

     ;; Live HTTP server tests via web-server
     (unwind-protect
         (progn
           (elpaish-serve-local test-port doc-root)
           (should (elpaish-server-running-p))

           ;; Live GET 200 OK
           (let ((url-buf (url-retrieve-synchronously (format "http://127.0.0.1:%d/test.html" test-port) t t 5)))
             (should url-buf)
             (with-current-buffer url-buf
               (goto-char (point-min))
               (should (search-forward "200 OK" nil t))
               (should (search-forward "<h1>Hello</h1>" nil t))
               (kill-buffer url-buf)))

           ;; Live HEAD 200 OK
           (let ((url-request-method "HEAD"))
             (let ((url-buf (url-retrieve-synchronously (format "http://127.0.0.1:%d/test.html" test-port) t t 5)))
               (should url-buf)
               (with-current-buffer url-buf
                 (goto-char (point-min))
                 (should (search-forward "200 OK" nil t))
                 (kill-buffer url-buf))))

           ;; Live GET 404
           (let ((url-buf (url-retrieve-synchronously (format "http://127.0.0.1:%d/nonexistent.el" test-port) t t 5)))
             (should url-buf)
             (with-current-buffer url-buf
               (goto-char (point-min))
               (should (search-forward "404 Not Found" nil t))
               (kill-buffer url-buf))))
       (elpaish-stop-server)))))

(ert-deftest elpaish-test-stable-stream-omission ()
  "Test omitting packages without clean semver tag from stable stream."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "untagged-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "untagged-pkg" "1.0.0" "Untagged Test")
     (let ((recipe (elpaish-register-package 'untagged-pkg pkg-dir)))
       ;; No clean semver git tag present
       (should-not (elpaish-derive-version recipe 'stable))
       (should-not (elpaish-build-package recipe 'stable))
       (let ((ac-file (elpaish-generate-archive-contents 'stable)))
         (with-temp-buffer
           (insert-file-contents ac-file)
           (let ((data (read (current-buffer))))
             (should-not (assoc 'untagged-pkg (cdr data))))))))))
(ert-deftest elpaish-test-pkg-descriptor-generation ()
  "Test multi-file file collection and <pkg>-pkg.el descriptor generation."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "desc-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (with-temp-file (expand-file-name "desc-pkg.el" pkg-dir) (insert ";;; desc-pkg.el -- Desc -*- lexical-binding: t -*-"))
     (with-temp-file (expand-file-name "desc-pkg-aux.el" pkg-dir) (insert ";;; desc-pkg-aux.el -- Aux -*- lexical-binding: t -*-"))
     (with-temp-file (expand-file-name "desc-pkg-tests.el" pkg-dir) (insert ";;; desc-pkg-tests.el -- Test -*- lexical-binding: t -*-"))

     ;; Verify file collection excludes test file
     (let ((files (elpaish--collect-files pkg-dir '("*.el") "desc-pkg")))
       (should (member "desc-pkg.el" files))
       (should (member "desc-pkg-aux.el" files))
       (should-not (member "desc-pkg-tests.el" files)))

     ;; Verify descriptor generation
     (let ((dest (expand-file-name "desc-pkg-pkg.el" temp-dir)))
       (elpaish--generate-pkg-file dest "desc-pkg"
                                   :version "1.2.3" :summary "Desc Summary"
                                   :reqs '((emacs "27.1")) :url "https://example.com" :keywords '("tools"))
       (should (file-exists-p dest))
       (with-temp-buffer
         (insert-file-contents dest)
         (should (search-forward "define-package" nil t))
         (should (search-forward "desc-pkg" nil t))
         (should (search-forward "1.2.3" nil t)))))))

(ert-deftest elpaish-test-packages-file-loading ()
  "Test external packages.el definitions loading into elpaish-registry."
  (elpaish-test-with-temp-env
   (let ((pkg-file (expand-file-name "packages.el" temp-dir)))
     (with-temp-file pkg-file
       (insert "(elpaish-register-package 'ext-pkg-1 \"/dummy/1\" :summary \"External 1\")\n")
       (insert "(elpaish-register-package 'ext-pkg-2 \"/dummy/2\" :summary \"External 2\")\n"))

     (let ((count (elpaish-load-packages pkg-file)))
       (should (>= count 2))
       (should (gethash "ext-pkg-1" elpaish-registry))
       (should (gethash "ext-pkg-2" elpaish-registry))
       (should (equal (elpaish-recipe-summary (gethash "ext-pkg-1" elpaish-registry)) "External 1"))))))

(ert-deftest elpaish-test-snapshot-rebuild-always-runs ()
  "Test that snapshot builds always regenerate the artifact, even with an
unchanged commit, while the derived version string stays pinned to the
source's last commit time rather than the time of the build."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "rebuild-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "rebuild-pkg" "1.0.0" "Rebuild Test")
     (elpaish-register-package 'rebuild-pkg pkg-dir)

     ;; Mock git commands to simulate a clean repository with commit hash "abc1234"
     (cl-letf (((symbol-function 'elpaish--git-string)
                (lambda (_dir cmd &rest args)
                  (cond
                   ((equal cmd "rev-parse") "abc1234567890abcdef")
                   ((and (equal cmd "log") (member "-1" args))
                    (if (member "--format=%ct" args)
                        "1700000000"
                      "abc1234567890abcdef"))
                   (t nil))))
               ((symbol-function 'file-directory-p)
                (lambda (path)
                  (or (string-suffix-p ".git" path)
                      (funcall #'file-exists-p path)))))

       ;; 1. First build
       (let* ((recipe (gethash "rebuild-pkg" elpaish-registry))
              (dest1 (elpaish-build-package recipe 'elpaish))
              (ver1 (elpaish-recipe-built-version-snapshot recipe)))
         (should (file-exists-p dest1))
         (should ver1)

         ;; Generate archive-contents
         (elpaish-generate-archive-contents 'elpaish)

         ;; Change the implementation's own packaging output without touching the
         ;; source commit: rebuilding must reflect the change instead of reusing
         ;; the prior artifact, while the version string (tied to commit time)
         ;; stays exactly the same.
         (with-temp-file (expand-file-name "rebuild-pkg.el" pkg-dir)
           (insert ";;; rebuild-pkg.el --- Rebuild Test -*- lexical-binding: t; -*-\n"
                   ";; Version: 1.0.0\n"
                   ";;; Commentary:\n;;; Code:\n"
                   "(defvar rebuild-pkg-marker t)\n"
                   ";;; rebuild-pkg.el ends here\n"))

         (let ((dest2 (elpaish-build-package recipe 'elpaish))
               (ver2 (elpaish-recipe-built-version-snapshot recipe)))
           (should (equal ver1 ver2))
           (with-temp-buffer
             (insert-file-contents dest2)
             (should (string-match-p "rebuild-pkg-marker" (buffer-string))))))))))

(ert-deftest elpaish-test-css-render ()
  "Test CSS sexp stylesheet rendering."
  (should (equal (elpaish-css-render '((body (color . "#fff") (margin . "0"))))
                 "body{color:#fff;margin:0;}"))
  (should (equal (elpaish-css-render '((".card" (border . "1px solid red"))))
                 ".card{border:1px solid red;}"))
  (should (equal (elpaish-css-render nil) ""))
  ;; Both generated stylesheets should render non-empty CSS referencing shared design tokens
  (let ((stream-css (elpaish-css-render (elpaish-css-stream-index-stylesheet)))
        (top-css (elpaish-css-render (elpaish-css-top-index-stylesheet))))
    (should (string-match-p (regexp-quote (elpaish-css-color 'fg-link)) stream-css))
    (should (string-match-p (regexp-quote (elpaish-css-color 'fg-link)) top-css))
    (should (string-match-p (regexp-quote elpaish-css-font-mono) stream-css))
    (should (string-match-p "\\.pkg-name-cell{" stream-css))
    (should (string-match-p "\\.btn{" top-css))))

(ert-deftest elpaish-test-website-stream-label-and-url ()
  "Test stream label and catalog URL formatting for generated pages."
  (should (equal (elpaish-website--stream-label 'snapshot) "snapshot"))
  (should (equal (elpaish-website--stream-label 'stable) "stable"))
  (should (equal (elpaish-website--stream-label 'staging) "staging"))
  (let ((elpaish-base-url "https://example.com/archive/"))
    (should (equal (elpaish-website--stream-url 'snapshot) "https://example.com/archive/snapshot/"))
    (should (equal (elpaish-website--stream-url 'stable) "https://example.com/archive/stable/"))
    (should (equal (elpaish-website--stream-url 'staging) "https://example.com/archive/staging/"))))

(ert-deftest elpaish-test-canonical-stream-aliases ()
  "Test stream alias normalization, including symbols not covered by the default cases."
  (should (eq (elpaish-canonical-stream 'snapshot) 'snapshot))
  (should (eq (elpaish-canonical-stream 'elpaish) 'snapshot))
  (should (eq (elpaish-canonical-stream 'unstable) 'snapshot))
  (should (eq (elpaish-canonical-stream 'stable) 'stable))
  (should (eq (elpaish-canonical-stream 'elpaish-stable) 'stable))
  (should (eq (elpaish-canonical-stream 'pre) 'staging))
  (should (eq (elpaish-canonical-stream 'staging) 'staging))
  (should (eq (elpaish-canonical-stream 'elpaish-staging) 'staging))
  (should (eq (elpaish-canonical-stream 'all) 'all))
  (should (eq (elpaish-canonical-stream 'some-unknown-symbol) 'snapshot)))

(ert-deftest elpaish-test-stream-dir-resolution ()
  "Test stream directory path resolution under a root directory."
  (let ((root "/tmp/elpaish-root/"))
    (should (equal (elpaish-stream-dir 'snapshot root) (expand-file-name "snapshot" root)))
    (should (equal (elpaish-stream-dir 'elpaish root) (expand-file-name "snapshot" root)))
    (should (equal (elpaish-stream-dir 'stable root) (expand-file-name "stable" root)))
    (should (equal (elpaish-stream-dir 'staging root) (expand-file-name "staging" root)))
    (should (equal (elpaish-stream-dir 'all root) (file-name-as-directory root)))))

(ert-deftest elpaish-test-css-color-error-handling ()
  "Test that `elpaish-css-color' signals an error when given an invalid color."
  (should (stringp (elpaish-css-color 'fg-main)))
  (should-error (elpaish-css-color 'nonexistent-palette-key)))
(ert-deftest elpaish-test-recipe-version-for-stream-setf ()
  "Test the generalized-variable setter for per-stream built versions."
  (elpaish-test-with-temp-env
   (let ((recipe (elpaish-register-package 'setf-pkg "/tmp/setf-pkg")))
     (setf (elpaish-recipe-version-for-stream recipe 'snapshot) "1.0.0")
     (setf (elpaish-recipe-version-for-stream recipe 'stable) "1.0.0")
     (setf (elpaish-recipe-version-for-stream recipe 'staging) "1.0.0.rc1")
     (should (equal (elpaish-recipe-built-version-snapshot recipe) "1.0.0"))
     (should (equal (elpaish-recipe-built-version-stable recipe) "1.0.0"))
     (should (equal (elpaish-recipe-built-version-staging recipe) "1.0.0.rc1"))
     (should (equal (elpaish-recipe-version-for-stream recipe 'snapshot) "1.0.0")))))
(ert-deftest elpaish-test-register-package-default-branch-customization ()
  "Test that omitting :branch honors `elpaish-default-branch' rather than a hardcoded literal."
  (elpaish-test-with-temp-env
   (let ((elpaish-default-branch "develop"))
     (let ((recipe (elpaish-register-package 'branch-pkg "/tmp/branch-pkg")))
       (should (equal (elpaish-recipe-branch recipe) "develop"))))))

(ert-deftest elpaish-test-monorepo-discovery ()
  "Test monorepo subpackage discovery and registration."
  (elpaish-test-with-temp-env
   (let ((root (expand-file-name "monorepo" temp-dir)))
     (elpaish-test-create-dummy-pkg (expand-file-name "pkg-one" root) "pkg-one" "1.0.0" "Pkg One")
     (elpaish-test-create-dummy-pkg (expand-file-name "pkg-two" root) "pkg-two" "1.0.0" "Pkg Two")
     ;; A subdirectory without a recognizable package header comment is ignored
     (make-directory (expand-file-name "not-a-package" root) t)
     (with-temp-file (expand-file-name "not-a-package.el" (expand-file-name "not-a-package" root))
       (insert ";; just a scratch file, no package header\n"))
     (let ((candidates (elpaish-discover-recipes root)))
       (should (= (length candidates) 2))
       (should (gethash "pkg-one" elpaish-registry))
       (should (gethash "pkg-two" elpaish-registry))
       (should-not (gethash "not-a-package" elpaish-registry))
       (should (equal (elpaish-recipe-source-directory-path (gethash "pkg-one" elpaish-registry)) "pkg-one"))))))

(ert-deftest elpaish-test-recipe-path-local-and-remote-fallback ()
  "Test local checkout search, remote URL fallback, and 1-argument URL derivation."
  (should (equal (elpaish-recipe-name-from-url "https://github.com/nailuoGG/ollama.el.git") "ollama"))
  (should (equal (elpaish-recipe-name-from-url "https://github.com/zackattackz/agent-shell-notifications.git") "agent-shell-notifications"))
  (should (equal (elpaish-recipe-name-from-url "https://github.com/tychoish/xtdlib.el") "xtdlib"))
  (elpaish-test-with-temp-env
   (let* ((search-root (expand-file-name "search-root" temp-dir))
          (elpaish-recipe-local-search-dirs (list search-root)))
     (make-directory (expand-file-name "found-pkg" search-root) t)
     (should (equal (elpaish-recipe-path "found-pkg" "https://example.com/found-pkg.git")
                    (expand-file-name "found-pkg" search-root)))
     (should (equal (elpaish-recipe-path "missing-pkg" "https://example.com/missing-pkg.git")
                    "https://example.com/missing-pkg.git"))
     (should (equal (elpaish-recipe-path "https://example.com/found-pkg.git")
                    (expand-file-name "found-pkg" search-root)))
     (should (equal (elpaish-recipe-path "https://example.com/missing-pkg.git")
                    "https://example.com/missing-pkg.git")))))

(ert-deftest elpaish-test-external-package-flag-and-website ()
  "Test :external and :mirror flag on package registration and website rendering."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "ext-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "ext-pkg" "1.0.0" "External Package Test")
     (elpaish-register-package 'ext-pkg pkg-dir :external t)
     (let ((recipe (gethash "ext-pkg" elpaish-registry)))
       (should (elpaish-recipe-external-p recipe))
       (should (equal (elpaish-recipe-origin-label recipe) "mirror"))
       (elpaish-build-package recipe 'snapshot)
       (elpaish-generate-stream-index 'snapshot)
       (let ((index-file (expand-file-name "snapshot/index.html" elpaish-output-dir)))
         (with-temp-buffer
           (insert-file-contents index-file)
           (goto-char (point-min))
           (should (search-forward "pkg-mirror-tag" nil t))
           (goto-char (point-min))
           (should (search-forward "pkg-external-tag" nil t))
           (goto-char (point-min))
           (should (search-forward "Mirror package" nil t))))))))

(ert-deftest elpaish-test-ollama-package-registration-and-build ()
  "Test that the registered ollama package recipe builds locally."
  (elpaish-test-with-temp-env
   (elpaish-load-packages)
   (let ((recipe (gethash "ollama" elpaish-registry)))
     (should recipe)
     (should (elpaish-recipe-fork-p recipe))
     (let ((built (elpaish-build-package recipe 'snapshot)))
       (should built)
       (should (file-exists-p built))
       (should (string-suffix-p ".tar" built))))))

(ert-deftest elpaish-test-dependency-fallback-without-archives ()
  "Recognize known external dependencies without local package data."
  (let ((elpaish-registry (make-hash-table :test 'equal))
        (package-archive-contents nil))
    (cl-letf (((symbol-function 'package-built-in-p) (lambda (&rest _) nil))
              ((symbol-function 'package-installed-p) (lambda (&rest _) nil))
              ((symbol-function 'locate-library) (lambda (&rest _) nil)))
      (should (elpaish-dependency-valid-p 'marginalia))
      (should-not (elpaish-dependency-valid-p 'unregistered-fake-dependency-xyz)))))

(ert-deftest elpaish-test-package-dependency-validation ()
  "Test validation of package dependencies (builtin, MELPA, ELPAish, or self-provided)."
  (elpaish-test-with-temp-env
   (elpaish-load-packages)
   ;; All packages in packages.el should pass dependency validation
   (should-not (elpaish-validate-all-dependencies))
   ;; Builtin dependency check
   (should (elpaish-dependency-valid-p 'emacs))
   (should (elpaish-dependency-valid-p 'json))
   ;; Self-provided internal feature check for multi-file package
   (let ((recipe (gethash "ollama" elpaish-registry)))
     (should (memq 'ollama-api (elpaish--recipe-provided-features recipe)))
     (should-not (elpaish-check-recipe-dependencies recipe)))
   ;; MELPA dependency check
   (should (elpaish-dependency-valid-p 'request))
   (should (elpaish-dependency-valid-p 'magit))
   ;; Invalid dependency check
   (should-not (elpaish-dependency-valid-p 'unregistered-fake-dependency-xyz))
   ;; Recipe with invalid dependency
   (elpaish-register-package 'bad-dep-pkg "/tmp" :requires '(unregistered-fake-dependency-xyz))
   (should (equal (elpaish-check-recipe-dependencies 'bad-dep-pkg)
                  '(unregistered-fake-dependency-xyz)))))

(ert-deftest elpaish-test-disabled-streams ()
  "Test suppressing/disabling package builds on specific release streams."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "suppressed-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "suppressed-pkg" "1.0.0" "Suppressed Stream Test")
     (elpaish-register-package 'suppressed-pkg pkg-dir :disabled-streams '(stable))
     (let ((recipe (gethash "suppressed-pkg" elpaish-registry)))
       (should (elpaish-recipe-disabled-for-stream-p recipe 'stable))
       (should-not (elpaish-recipe-disabled-for-stream-p recipe 'snapshot))
       (should-not (elpaish-recipe-disabled-for-stream-p recipe 'staging))
       ;; Build snapshot should succeed
       (should (elpaish-build-package recipe 'snapshot))
       ;; Build stable should be skipped
       (should-not (elpaish-build-package recipe 'stable))))))
(ert-deftest elpaish-test-bundle-readme-and-license ()
  "Test that README and LICENSE files are bundled into packages."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "bundled-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "bundled-pkg" "1.0.0" "Bundled Doc Test")
     ;; Write README.md and LICENSE
     (with-temp-file (expand-file-name "README.md" pkg-dir)
       (insert "# Bundled Package\nDocumentation here.\n"))
     (with-temp-file (expand-file-name "LICENSE" pkg-dir)
       (insert "GPL-3.0-or-later\n"))
     (elpaish-register-package 'bundled-pkg pkg-dir :doc "https://example.com/doc")
     (let* ((recipe (gethash "bundled-pkg" elpaish-registry))
            (files (elpaish--collect-files pkg-dir (elpaish-recipe-files recipe) "bundled-pkg" recipe)))
       (should (member "README.md" files))
       (should (member "LICENSE" files))
       (let ((artifact (elpaish-build-package recipe 'snapshot)))
         (should (string-suffix-p ".tar" artifact)))))))

(ert-deftest elpaish-test-bundle-org-files-default ()
  "Test that org documentation files in root and docs/ are bundled by default."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "org-doc-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "org-doc-pkg" "1.0.0" "Org Doc Bundle Test")
     (make-directory (expand-file-name "docs" pkg-dir) t)
     (make-directory (expand-file-name "tests" pkg-dir) t)
     (make-directory (expand-file-name ".hidden" pkg-dir) t)
     ;; Write org doc files
     (with-temp-file (expand-file-name "README.org" pkg-dir)
       (insert "* Org Doc Package\nDocumentation in root.\n"))
     (with-temp-file (expand-file-name "docs/guide.org" pkg-dir)
       (insert "* User Guide\nDetailed guide.\n"))
     (with-temp-file (expand-file-name "docs/api.org" pkg-dir)
       (insert "* API Reference\nGenerated API.\n"))
     ;; Files that must NOT be bundled
     (with-temp-file (expand-file-name "tests/test.org" pkg-dir)
       (insert "* Test notes\n"))
     (with-temp-file (expand-file-name ".hidden/internal.org" pkg-dir)
       (insert "* Hidden\n"))
     (elpaish-register-package 'org-doc-pkg pkg-dir)
     (let* ((recipe (gethash "org-doc-pkg" elpaish-registry))
            (files (elpaish--collect-files pkg-dir (elpaish-recipe-files recipe) "org-doc-pkg" recipe)))
       (should (member "README.org" files))
       (should (member "docs/guide.org" files))
       (should (member "docs/api.org" files))
       (should-not (member "tests/test.org" files))
       (should-not (member ".hidden/internal.org" files))
       (let ((artifact (elpaish-build-package recipe 'snapshot)))
         (should (string-suffix-p ".tar" artifact)))))))

(ert-deftest elpaish-test-bundle-org-files-filtering-and-exclusion ()
  "Test filtering and exclusion options for org documentation bundling."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "org-filter-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "org-filter-pkg" "1.0.0" "Org Filter Test")
     (make-directory (expand-file-name "docs" pkg-dir) t)
     (make-directory (expand-file-name "drafts" pkg-dir) t)
     (with-temp-file (expand-file-name "docs/public.org" pkg-dir)
       (insert "* Public\n"))
     (with-temp-file (expand-file-name "docs/internal.org" pkg-dir)
       (insert "* Internal\n"))
     (with-temp-file (expand-file-name "drafts/wip.org" pkg-dir)
       (insert "* Draft\n"))

     ;; 1. Test :org-files nil (opt-out)
     (elpaish-register-package 'org-no-docs pkg-dir :org-files nil)
     (let* ((rec (gethash "org-no-docs" elpaish-registry))
            (files (elpaish--collect-files pkg-dir (elpaish-recipe-files rec) "org-no-docs" rec)))
       (should-not (member "docs/public.org" files))
       (should-not (member "drafts/wip.org" files)))

     ;; 2. Test :org-files with prefix list
     (elpaish-register-package 'org-prefix-docs pkg-dir :org-files '("docs"))
     (let* ((rec (gethash "org-prefix-docs" elpaish-registry))
            (files (elpaish--collect-files pkg-dir (elpaish-recipe-files rec) "org-prefix-docs" rec)))
       (should (member "docs/public.org" files))
       (should-not (member "drafts/wip.org" files)))

     ;; 3. Test :exclude-files on org documentation
     (elpaish-register-package 'org-ex-docs pkg-dir :exclude-files '("docs/internal.org" "drafts/*"))
     (let* ((rec (gethash "org-ex-docs" elpaish-registry))
            (files (elpaish--collect-files pkg-dir (elpaish-recipe-files rec) "org-ex-docs" rec)))
       (should (member "docs/public.org" files))
       (should-not (member "docs/internal.org" files))
       (should-not (member "drafts/wip.org" files))))))

(ert-deftest elpaish-test-multi-glob-packages-files ()
  "Test loading package definitions from a list of globs."
  (elpaish-test-with-temp-env
   (let* ((recipes-dir (expand-file-name "recipes" temp-dir))
          (file1 (expand-file-name "pkg1.el" recipes-dir))
          (file2 (expand-file-name "pkg2.el" recipes-dir)))
     (make-directory recipes-dir t)
     (with-temp-file file1
       (insert "(elpaish-register-package 'glob-pkg-1 \"/tmp/p1\" :summary \"Glob 1\")\n"))
     (with-temp-file file2
       (insert "(elpaish-register-package 'glob-pkg-2 \"/tmp/p2\" :summary \"Glob 2\")\n"))
     (let ((elpaish-packages-files (list (expand-file-name "*.el" recipes-dir))))
       (elpaish-load-packages)
       (should (gethash "glob-pkg-1" elpaish-registry))
       (should (gethash "glob-pkg-2" elpaish-registry))))))

(ert-deftest elpaish-test-build-single-package ()
  "Test `elpaish-build-single' command."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "single-build-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "single-build-pkg" "1.0.0" "Single Build Test")
     (elpaish-register-package 'single-build-pkg pkg-dir)
     (elpaish-build-single "single-build-pkg" 'snapshot)
     (should (file-exists-p (expand-file-name "snapshot/archive-contents" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "snapshot/index.html" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "index.html" elpaish-output-dir))))))

(ert-deftest elpaish-test-transient-menu-defined ()
  "Test that transient menu `elpaish-menu' exists."
  (should (fboundp 'elpaish-menu)))

(ert-deftest elpaish-test-register-package-descriptive-parameter-names ()
  "Test registering a package with full descriptive path parameters."
  (elpaish-test-with-temp-env
   (let ((recipe (elpaish-register-package
                  'descriptive-pkg
                  "/tmp/descriptive-repo"
                  :source-directory-path "lisp"
                  :test-directory-path "test"
                  :disabled-streams '(stable))))
     (should (equal (elpaish-recipe-repository-path recipe) "/tmp/descriptive-repo"))
     (should (equal (elpaish-recipe-source-directory-path recipe) "lisp"))
     (should (equal (elpaish-recipe-test-directory-path recipe) "test"))
     (should (elpaish-recipe-disabled-for-stream-p recipe 'stable)))))

(ert-deftest elpaish-test-modus-theme-toggle ()
  "Test switching between `modus-operandi' and `modus-vivendi' themes."
  (let ((elpaish-modus-theme 'modus-operandi))
    (should (equal (elpaish-css-color 'bg-main) "#ffffff"))
    (should (equal (elpaish-css-color 'fg-main) "#000000")))
  (let ((elpaish-modus-theme 'modus-vivendi))
    (should (equal (elpaish-css-color 'bg-main) "#000000"))
    (should (equal (elpaish-css-color 'fg-main) "#ffffff"))))

(ert-deftest elpaish-test-website-format-reqs ()
  "Test formatting dependency requirements without lisp S-expressions."
  (should (equal (elpaish-website--format-reqs nil) "None"))
  (should (equal (elpaish-website--format-reqs '((emacs (27 1)) (magit (3 0 0))))
                 "emacs (>= 27.1), magit (>= 3.0.0)"))
  (should (equal (elpaish-website--format-reqs '((emacs "28.1") (seq "2.0")))
                 "emacs (>= 28.1), seq (>= 2.0)"))
  (should (equal (elpaish-website--format-reqs '(compat (json (0))))
                 "compat, json")))

(ert-deftest elpaish-test-website-file-sha256 ()
  "Test SHA256 checksum computation for package files."
  (elpaish-test-with-temp-env
   (let ((test-file (expand-file-name "test.txt" temp-dir)))
     (with-temp-file test-file
       (insert "hello elpaish"))
     (should (equal (elpaish-website--file-sha256 test-file)
                    (secure-hash 'sha256 "hello elpaish")))
     (should-not (elpaish-website--file-sha256 "/nonexistent/path")))))

(ert-deftest elpaish-test-top-navbar-positioning ()
  "Test that the navigation bar appears below the page title."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "nav-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "nav-pkg" "1.0.0" "Nav Test")
     (elpaish-register-package 'nav-pkg pkg-dir)
     (elpaish-build-package (gethash "nav-pkg" elpaish-registry) 'snapshot)
     (elpaish-generate-stream-index 'snapshot)
     (elpaish-generate-top-index)
     ;; In top index: h1 appears before navbar
     (with-temp-buffer
       (insert-file-contents (expand-file-name "index.html" elpaish-output-dir))
       (let ((h1-pos (search-forward "<h1>" nil t))
             (nav-pos (search-forward "<nav class=\"navbar\">" nil t)))
         (should (and h1-pos nav-pos (< h1-pos nav-pos)))))
     ;; In stream index: h1 appears before navbar
     (with-temp-buffer
       (insert-file-contents (expand-file-name "snapshot/index.html" elpaish-output-dir))
       (let ((h1-pos (search-forward "<h1>" nil t))
             (nav-pos (search-forward "<nav class=\"navbar\">" nil t)))
         (should (and h1-pos nav-pos (< h1-pos nav-pos))))))))

(ert-deftest elpaish-test-server-persistence-across-builds ()
  "Test that running preview server persists across `elpaish-build-all'."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "persist-pkg" temp-dir))
         (test-port (+ 18000 (random 2000))))
     (elpaish-test-create-dummy-pkg pkg-dir "persist-pkg" "1.0.0" "Server Persist Test")
     (elpaish-register-package 'persist-pkg pkg-dir)
     (elpaish-build-all 'snapshot)
     (elpaish-serve-local test-port elpaish-output-dir)
     (should (elpaish-server-running-p))
     ;; Run build-all while server is running
     (elpaish-build-all 'snapshot)
     (should (elpaish-server-running-p))
     ;; Run build-single while server is running
     (elpaish-build-single 'persist-pkg 'snapshot)
     (should (elpaish-server-running-p)))))
(ert-deftest elpaish-test-two-row-detail-layout ()
  "Test that package catalogs emit distinct rows for the main entry and detail view."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "row-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "row-pkg" "1.0.0" "Row Layout Test")
     (elpaish-register-package 'row-pkg pkg-dir)
     (elpaish-build-package (gethash "row-pkg" elpaish-registry) 'snapshot)
     (elpaish-generate-stream-index 'snapshot)
     (with-temp-buffer
       (insert-file-contents (expand-file-name "snapshot/index.html" elpaish-output-dir))
       (goto-char (point-min))
       (should (search-forward "class=\"pkg-row\"" nil t))
       (goto-char (point-min))
       (should (search-forward "class=\"pkg-detail-row\"" nil t))
       (goto-char (point-min))
       (should (search-forward "colspan=\"5\"" nil t))))))

(ert-deftest elpaish-test-build-recompile-command ()
  "Test that `recompile' or `g' in `*elpaish-build*' re-runs `elpaish-build-all'."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "recompile-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "recompile-pkg" "1.0.0" "Recompile Test")
     (elpaish-register-package 'recompile-pkg pkg-dir)
     (elpaish-build-all 'snapshot)
     (let ((buf (get-buffer elpaish-build-buffer-name)))
       (should buf)
       (with-current-buffer buf
         (should (eq major-mode 'elpaish-build-mode))
         (should (eq revert-buffer-function #'elpaish-build-recompile))
         (should (eq (key-binding (kbd "g")) #'elpaish-build-recompile))
         ;; Call recompile
         (elpaish-build-recompile)
         (should (file-exists-p (expand-file-name "snapshot/archive-contents" elpaish-output-dir))))))))

(ert-deftest elpaish-test-sha256-file-generation-and-icon-link ()
  "Test that SHA256 checksum files are written and linked in catalog pages."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "sha-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "sha-pkg" "1.0.0" "SHA256 Test")
     (elpaish-register-package 'sha-pkg pkg-dir)
     (let ((dest (elpaish-build-package (gethash "sha-pkg" elpaish-registry) 'snapshot)))
       (should dest)
       (let ((sha-file (concat dest ".sha256"))
             (index-file (expand-file-name "snapshot/index.html" elpaish-output-dir)))
         (elpaish-generate-stream-index 'snapshot)
         (should (file-exists-p sha-file))
         (with-temp-buffer
           (insert-file-contents sha-file)
           (should (search-forward "sha-pkg" nil t)))
         ;; Verify icon link in HTML
         (with-temp-buffer
           (insert-file-contents index-file)
           (goto-char (point-min))
           (should (search-forward "icon-checksum" nil t))
           (goto-char (point-min))
           (should (search-forward "SHA256 checksum file" nil t))))))))

(ert-deftest elpaish-test-build-website-command ()
  "Test `elpaish-build-website' command regenerates all HTML pages without rebuilding archives."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "site-only-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "site-only-pkg" "1.0.0" "Site Only Test")
     (elpaish-register-package 'site-only-pkg pkg-dir)
     (setf (elpaish-recipe-version-for-stream (gethash "site-only-pkg" elpaish-registry) 'snapshot) "1.0.0")
     ;; Build website only
     (elpaish-build-website elpaish-output-dir)
     (should (file-exists-p (expand-file-name "index.html" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "about.html" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "snapshot/index.html" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "stable/index.html" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "staging/index.html" elpaish-output-dir))))))
(ert-deftest elpaish-test-run-checks-compile-buffer-success ()
  "Test `elpaish-run-checks' pipes output to a compilation buffer on passing package."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "good-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "good-pkg" "1.0.0" "Good Test Package")
     (let ((res (elpaish-run-checks pkg-dir))
           (buf (get-buffer (elpaish-check--get-buffer-name pkg-dir))))
       (should res)
       (should buf)
       (with-current-buffer buf
         (should (derived-mode-p 'compilation-mode))
         (should (eq major-mode 'elpaish-check-mode))
         (goto-char (point-min))
         (should (search-forward "=== ELPAish Checks: good-pkg ===" nil t))
         (should (search-forward "1. Running check-parens" nil t))
         (should (search-forward "✓ check-parens passed" nil t))
         (should (search-forward "✓ byte-compilation passed" nil t))
         (should (search-forward "=== Checks PASSED" nil t))
         (should (search-forward "Compilation finished" nil t)))))))

(ert-deftest elpaish-test-run-checks-compile-buffer-failure ()
  "Test `elpaish-run-checks' pipes error output and parses compilation errors on broken package."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "broken-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (with-temp-file (expand-file-name "broken-pkg.el" pkg-dir)
       (insert ";;; broken-pkg.el --- Broken -*- lexical-binding: t; -*-\n")
       (insert "(defun broken (x (missing-close-paren)\n")
       (insert "(provide 'broken-pkg)\n"))
     (let ((res (elpaish-run-checks pkg-dir))
           (buf (get-buffer (elpaish-check--get-buffer-name pkg-dir))))
       (should-not res)
       (should buf)
       (with-current-buffer buf
         (should (derived-mode-p 'compilation-mode))
         (should (eq major-mode 'elpaish-check-mode))
         (goto-char (point-min))
         (should (search-forward "=== ELPAish Checks: broken-pkg ===" nil t))
         (should (search-forward "error: check-parens:" nil t))
         (should (search-forward "=== Checks FAILED" nil t))
         (should (search-forward "Compilation exited abnormally" nil t))
         (should (> compilation-num-errors-found 0)))))))

(ert-deftest elpaish-test-view-check-log ()
  "Test `elpaish-view-check-log' displays the check compilation buffer."
  (elpaish-test-with-temp-env
   (let ((elpaish-check-buffer-name "*elpaish-test-check-view*"))
     (elpaish-view-check-log)
     (let ((buf (get-buffer (elpaish-check--get-buffer-name))))
       (should buf)
       (with-current-buffer buf
         (should (derived-mode-p 'compilation-mode))
         (should (eq major-mode 'elpaish-check-mode)))))))
(ert-deftest elpaish-test-project-has-el-files-p ()
  "Test `elpaish-check-project-has-el-files-p' detection of .el files."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "has-el" temp-dir))
         (empty-dir (expand-file-name "no-el" temp-dir)))
     (make-directory pkg-dir t)
     (make-directory empty-dir t)
     (with-temp-file (expand-file-name "foo.el" pkg-dir)
       (insert ";;; foo.el\n"))
     (should (elpaish-check-project-has-el-files-p pkg-dir))
     (should-not (elpaish-check-project-has-el-files-p empty-dir)))))

(ert-deftest elpaish-test-setup-compile-command ()
  "Test `elpaish-check-setup-compile-command' sets `compile-command' buffer-locally."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "builder-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (with-temp-file (expand-file-name "builder-pkg.el" pkg-dir)
       (insert ";;; builder-pkg.el\n"))
     (with-temp-buffer
       (setq-local default-directory (file-name-as-directory pkg-dir))
       (emacs-lisp-mode)
       (elpaish-check-setup-compile-command pkg-dir)
       (should (string-prefix-p "emacsclient --eval" compile-command))
       (should (string-match-p "elpaish-run-checks" compile-command))
       (should (string-match-p "builder-pkg" compile-command))))))

(ert-deftest elpaish-test-maybe-setup-builder ()
  "Test `elpaish-check-maybe-setup-builder' hook sets `compile-command' on .el buffers."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "hook-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (let ((el-file (expand-file-name "hook-pkg.el" pkg-dir)))
       (with-temp-file el-file
         (insert ";;; hook-pkg.el\n"))
       (with-temp-buffer
         (setq-local buffer-file-name el-file)
         (setq-local default-directory (file-name-as-directory pkg-dir))
         (emacs-lisp-mode)
         (elpaish-check-maybe-setup-builder)
         (should (string-match-p "elpaish-run-checks" compile-command)))))))

(ert-deftest elpaish-test-check-buffer-name-formatting ()
  "Test that check compilation buffer name formats as *<project-name>-checks*."
  (elpaish-test-with-temp-env
   (let* ((elpaish-check-buffer-name nil)
          (name (elpaish-check-buffer-name "/path/to/my-package/")))
     (should (equal name "*my-package-checks*")))))

(ert-deftest elpaish-test-http-resolve-path-traversal-protection ()
  "Test `elpaish--http-resolve-path' prevents directory traversal outside doc-root."
  (elpaish-test-with-temp-env
   (let ((doc-root (expand-file-name "public/" temp-dir)))
     (make-directory doc-root t)
     (with-temp-file (expand-file-name "index.html" doc-root)
       (insert "<h1>Index</h1>"))
     (make-directory (expand-file-name "snapshot" doc-root) t)
     (with-temp-file (expand-file-name "snapshot/index.html" doc-root)
       (insert "<h1>Snapshot</h1>"))

     ;; Valid paths
     (should (equal (elpaish--http-resolve-path "/" doc-root)
                    (expand-file-name "index.html" doc-root)))
     (should (equal (elpaish--http-resolve-path "/snapshot/" doc-root)
                    (expand-file-name "snapshot/index.html" doc-root)))
     (should (equal (elpaish--http-resolve-path "index.html" doc-root)
                    (expand-file-name "index.html" doc-root)))

     ;; Path traversal attempts should be rejected (return nil)
     (should-not (elpaish--http-resolve-path "../secret.txt" doc-root))
     (should-not (elpaish--http-resolve-path "../../etc/passwd" doc-root))
     (should-not (elpaish--http-resolve-path "/../secret.txt" doc-root)))))

(ert-deftest elpaish-test-keyring-setup-streams ()
  "Test `elpaish-keyring-setup' configures `package-archives' entries."
  (elpaish-test-with-temp-env
   (let ((package-archives nil))
     (elpaish-keyring-setup 'snapshot)
     (should (assoc "snapshot" package-archives))
     (should (string-match-p "/snapshot/" (cdr (assoc "snapshot" package-archives)))))
   (let ((package-archives nil))
     (elpaish-keyring-setup 'stable)
     (should (assoc "stable" package-archives))
     (should (string-match-p "/stable/" (cdr (assoc "stable" package-archives)))))
   (let ((package-archives nil))
     (elpaish-keyring-setup 'staging)
     (should (assoc "staging" package-archives))
     (should (string-match-p "/staging/" (cdr (assoc "staging" package-archives)))))))

(ert-deftest elpaish-test-package-header-p-validation ()
  "Test `elpaish--package-header-p' detects valid and invalid package files."
  (elpaish-test-with-temp-env
   (let ((valid-file (expand-file-name "valid.el" temp-dir))
         (invalid-file (expand-file-name "invalid.el" temp-dir))
         (non-existent (expand-file-name "missing.el" temp-dir)))
     (with-temp-file valid-file
       (insert ";;; valid.el --- Valid package summary -*- lexical-binding: t; -*-\n"))
     (with-temp-file invalid-file
       (insert ";; Not a valid package header\n(defun foo ())\n"))
     (should (elpaish--package-header-p valid-file))
     (should-not (elpaish--package-header-p invalid-file))
     (should-not (elpaish--package-header-p non-existent)))))

(ert-deftest elpaish-test-signing-key-expiration-helper ()
  "Test `elpaish-verify-signing-key--subkey-expired-p' subkey expiration check."
  (let* ((now (float-time))
         (future-key (epg-make-sub-key nil nil nil nil nil nil nil (+ now 10000)))
         (never-key (epg-make-sub-key nil nil nil nil nil nil nil 0))
         (past-key (epg-make-sub-key nil nil nil nil nil nil nil (- now 10000))))
    (should-not (elpaish-verify-signing-key--subkey-expired-p future-key))
    (should-not (elpaish-verify-signing-key--subkey-expired-p never-key))
    (should (elpaish-verify-signing-key--subkey-expired-p past-key))))

(ert-deftest elpaish-test-install-packages-fallback-and-list ()
  "Test `elpaish-install-packages' fallback and argument parsing."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list '(boot-a boot-b))
         (installed-log nil))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-install) (lambda (pkg) (push pkg installed-log))))
       ;; Default fallback (synchronous)
       (elpaish-install-packages :sync t)
       (should (equal installed-log '(boot-b boot-a)))
       (setq installed-log nil)

       ;; List of symbols
       (elpaish-install-packages '(pkg-x pkg-y) :sync t)
       (should (equal installed-log '(pkg-y pkg-x)))))))

(ert-deftest elpaish-test-install-packages-continue-on-error ()
  "Test `elpaish-install-packages' installs packages and continues on error."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-bootstrap-packages-list '(dummy-pkg-a dummy-pkg-b))
         (installed-log nil)
         (refresh-called nil))
     (cl-letf (((symbol-function 'package-refresh-contents)
                (lambda () (setq refresh-called t)))
               ((symbol-function 'package-installed-p)
                (lambda (pkg) (eq pkg 'already-installed)))
               ((symbol-function 'package-install)
                (lambda (pkg)
                  (if (eq pkg 'fail-pkg)
                      (error "Simulated install error for %s" pkg)
                    (push pkg installed-log)))))
       ;; Test with refresh option and error handling
       (elpaish-install-packages 'good-pkg 'fail-pkg 'already-installed :refresh t :sync t)
       (should refresh-called)
       (should (equal installed-log '(good-pkg)))))))

(ert-deftest elpaish-test-upgrade-packages-continue-on-error ()
  "Test `elpaish-install-upgrade-packages' refreshes contents and upgrades packages without error."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-bootstrap-packages-list '(boot-pkg))
         (refresh-called nil)
         (upgraded-log nil))
     (cl-letf (((symbol-function 'package-refresh-contents)
                (lambda () (setq refresh-called t)))
               ((symbol-function 'package-installed-p)
                (lambda (_pkg) t))
               ((symbol-function 'package-upgrade)
                (lambda (pkg)
                  (if (eq pkg 'up-to-date-pkg)
                      (signal 'user-error '("Package is up to date"))
                    (push pkg upgraded-log)))))
       ;; Runs refresh contents first and does not fail on up-to-date or error
       (elpaish-install-upgrade-packages 'upgrade-pkg 'up-to-date-pkg :sync t)
       (should refresh-called)
       (should (equal upgraded-log '(upgrade-pkg)))))))

(ert-deftest elpaish-test-install-packages-noop-when-installed ()
  "Test `elpaish-install-packages' is a silent no-op when all packages are already installed."
  (elpaish-test-with-temp-env
   (let ((installed-log nil)
         (refresh-called nil))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) t))
               ((symbol-function 'package-install) (lambda (pkg) (push pkg installed-log)))
               ((symbol-function 'package-refresh-contents) (lambda () (setq refresh-called t))))
       (let ((res (elpaish-install-packages 'pkg-a 'pkg-b :action 'install :sync t)))
         (should-not res)
         (should-not installed-log)
         (should-not refresh-called))))))

(ert-deftest elpaish-test-install-packages-nested-lists ()
  "Test `elpaish-install-packages' flattens nested package lists and deduplicates."
  (elpaish-test-with-temp-env
   (let ((installed-log nil))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-install) (lambda (pkg) (push pkg installed-log))))
       (elpaish-install-packages 'pkg-a '((pkg-b (pkg-c))) 'pkg-a :sync t)
       (should (equal installed-log '(pkg-c pkg-b pkg-a)))))))

(ert-deftest elpaish-test-add-bootstrap-packages ()
  "Test `elpaish-install-register-package' appends unique symbols."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list '(pkg-a pkg-b)))
     ;; Append single symbols and list of symbols
     (elpaish-install-register-package 'pkg-c 'pkg-b '(pkg-d pkg-a))
     (should (equal elpaish-install-packages-list '(pkg-a pkg-b pkg-c pkg-d)))

     ;; Append additional symbols
     (elpaish-install-register-package '(pkg-e pkg-c) 'pkg-f)
     (should (equal elpaish-install-packages-list '(pkg-a pkg-b pkg-c pkg-d pkg-e pkg-f))))))

(ert-deftest elpaish-test-install-packages-async-sprite ()
  "Test `elpaish-install-packages' async path using sprite pool."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-bootstrap-packages-list '(pkg-async-a pkg-async-b))
         (reloaded nil)
         (callback-called nil)
         (original-require (symbol-function 'require)))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-read-all-archive-contents) (lambda () (setq reloaded t)))
               ((symbol-function 'require)
                (lambda (feature &optional filename noerror)
                  (if (memq feature '(sprite sprite-fleet))
                      t
                    (funcall original-require feature filename noerror))))
               ((symbol-function 'sprite-pool-mapcar)
                (cl-function
                 (lambda (_fn items &key async _pool-size &allow-other-keys)
                   (let ((res (mapcar (lambda (item) (list :status :installed :pkg item)) items)))
                     (if async
                         (let ((f (sprite-future--make :state :pending)))
                           (sprite-future--settle f :resolved res)
                           f)
                       res)))))
               ;; Stub the sprite-future protocol itself: `sprite' is not a
               ;; published package installable in CI, so the test cannot rely
               ;; on the real implementation being loaded.
               ((symbol-function 'sprite-future--make)
                (cl-function (lambda (&key state &allow-other-keys) (cons state nil))))
               ((symbol-function 'sprite-future--settle)
                (lambda (future state value)
                  (if (consp future)
                      (progn (setcar future state) (setcdr future value))
                    (aset future 2 state)
                    (aset future 3 value))))
               ((symbol-function 'sprite-future-then)
                (lambda (future on-resolve &optional _on-reject)
                  (let ((state (if (consp future) (car future) (aref future 2)))
                        (val (if (consp future) (cdr future) (aref future 3))))
                    (when (eq state :resolved)
                      (funcall on-resolve val)))
                  future)))
       (elpaish-install-packages-async '(pkg-async-a pkg-async-b) :callback (lambda (_) (setq callback-called t)))
       (should (eq (require 'cl-lib) 'cl-lib))
       (should reloaded)
       (should callback-called)))))

(ert-deftest elpaish-test-install-packages-async-noop-when-installed ()
  "Test `elpaish-install-packages' async is a no-op when all packages are installed."
  (elpaish-test-with-temp-env
   (let ((callback-called nil)
         (callback-result 'unset))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) t)))
       (elpaish-install-packages 'pkg-a 'pkg-b
                                 :action 'install
                                 :async t
                                 :callback (lambda (res)
                                             (setq callback-called t)
                                             (setq callback-result res)))
       (should callback-called)
       (should-not callback-result)))))

(ert-deftest elpaish-test-install-packages-async-fallback-timer ()
  "Test `elpaish-install-packages' async path falls back to `run-at-time' when gen and sprite unavailable."
  (elpaish-test-with-temp-env
   (let ((timer-called nil)
         (callback-called nil)
         (orig-fboundp (symbol-function 'fboundp))
         (original-require (symbol-function 'require)))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'fboundp)
                (lambda (sym)
                  (if (memq sym '(gen-run-async sprite-pool-mapcar))
                      nil
                    (funcall orig-fboundp sym))))
               ((symbol-function 'require)
                (lambda (feature &optional filename noerror)
                  (if (memq feature '(sprite sprite-fleet gen))
                      nil
                    (funcall original-require feature filename noerror))))
               ((symbol-function 'run-at-time)
                (lambda (_secs _repeat fn)
                  (setq timer-called t)
                  (funcall fn)))
               ((symbol-function 'elpaish-install--do-packages)
                (lambda (pkgs &rest _)
                  (mapcar (lambda (p) (list :status :installed :pkg p)) pkgs))))
       (elpaish-install-packages 'pkg-a :action 'install :async t :callback (lambda (_) (setq callback-called t)))
       (should (eq (require 'cl-lib) 'cl-lib))
       (should timer-called)
       (should callback-called)))))

(ert-deftest elpaish-test-install-local-dir ()
  "Build and install a local repository into the current package session."
  (elpaish-test-with-temp-env
   (let* ((repo (expand-file-name "example-repo" temp-dir))
          (package-user-dir (expand-file-name "installed" temp-dir))
          (package-alist nil)
          (package-selected-packages nil)
          (package-quickstart nil)
          (package-quickstart-file (expand-file-name "quickstart.el" temp-dir))
          (load-path (copy-sequence load-path)))
     (elpaish-test-create-dummy-pkg repo "example-pkg" "1.0" "Example package")
     (cl-letf (((symbol-function 'package--save-selected-packages) #'ignore))
       (let ((desc (elpaish-install-local repo :name 'example-pkg)))
         (should (eq (package-desc-name desc) 'example-pkg))
         (should (package-installed-p 'example-pkg))
         (should (file-exists-p (expand-file-name "example-pkg.el" (package-desc-dir desc))))
         (should (member (package-desc-dir desc) load-path))
         (should-not (gethash "example-pkg" elpaish-registry)))))))

(ert-deftest elpaish-test-install-local-recipe-tar ()
  "Install a multi-file package from a recipe without modifying it."
  (elpaish-test-with-temp-env
   (let* ((repo (expand-file-name "repo" temp-dir))
          (source (expand-file-name "src" repo))
          (package-user-dir (expand-file-name "installed" temp-dir))
          (package-alist nil)
          (package-selected-packages nil)
          (package-quickstart nil)
          (package-quickstart-file (expand-file-name "quickstart.el" temp-dir))
          (load-path (copy-sequence load-path)))
     (elpaish-test-create-dummy-pkg source "recipe-pkg" "1.0" "Recipe package")
     (with-temp-file (expand-file-name "extra.el" source)
       (insert ";;; extra.el --- Extra file -*- lexical-binding: t; -*-\n"))
     (let ((recipe (elpaish-register-package 'recipe-pkg repo :source-dir "src")))
       (cl-letf (((symbol-function 'package--save-selected-packages) #'ignore))
         (let ((desc (elpaish-install-local recipe)))
           (should (eq (package-desc-name desc) 'recipe-pkg))
           (should (file-exists-p (expand-file-name "extra.el" (package-desc-dir desc))))
           (should-not (elpaish-recipe-built-version-snapshot recipe))))))))

(ert-deftest elpaish-test-install-local-build-failure ()
  "A failed build never invokes package installation."
  (elpaish-test-with-temp-env
   (let ((installed nil))
     (cl-letf (((symbol-function 'elpaish-build-package) (lambda (&rest _) nil))
               ((symbol-function 'package-install-file) (lambda (_) (setq installed t))))
       (should-error (elpaish-install-local temp-dir :name 'missing))
       (should-not installed)))))

(ert-deftest elpaish-test-install-local-packages-scopes ()
  "Test `elpaish-install-local-packages' across installed, upgrade, and all scopes."
  (elpaish-test-with-temp-env
   (let* ((repo1 (expand-file-name "repo1" temp-dir))
          (repo2 (expand-file-name "repo2" temp-dir))
          (repo3 (expand-file-name "repo3" temp-dir))
          (package-user-dir (expand-file-name "installed" temp-dir))
          (package-alist nil)
          (package-selected-packages nil)
          (package-quickstart nil)
          (package-quickstart-file (expand-file-name "quickstart.el" temp-dir))
          (load-path (copy-sequence load-path))
          (built-packages nil))
     (elpaish-test-create-dummy-pkg repo1 "pkg-one" "1.0" "Package One")
     (elpaish-test-create-dummy-pkg repo2 "pkg-two" "1.0" "Package Two")
     (elpaish-test-create-dummy-pkg repo3 "pkg-three" "1.0" "Package Three")
     (let ((rec1 (elpaish-register-package 'pkg-one repo1))
           (rec2 (elpaish-register-package 'pkg-two repo2))
           (rec3 (elpaish-register-package 'pkg-three repo3))
           (elpaish-install-packages-list '(pkg-one pkg-two))
           (elpaish-install-upgrade-packages-list '(pkg-one)))
       (cl-letf (((symbol-function 'package--save-selected-packages) #'ignore)
                 ((symbol-function 'native-compile-async) #'ignore))
         ;; 1. Test scope 'upgrade installs only pkg-one
         (let ((descs (elpaish-install-local-packages 'upgrade)))
           (should (= (length descs) 1))
           (should (eq (package-desc-name (car descs)) 'pkg-one)))
         ;; 2. Test scope 'installed installs pkg-one and pkg-two
         (let ((descs (elpaish-install-local-packages 'installed)))
           (should (= (length descs) 2))
           (should (member (package-desc-name (nth 0 descs)) '(pkg-one pkg-two)))
           (should (member (package-desc-name (nth 1 descs)) '(pkg-one pkg-two))))
         ;; 3. Test explicit list installs specified package
         (let ((descs (elpaish-install-local-packages '(pkg-three))))
           (should (= (length descs) 1))
           (should (eq (package-desc-name (car descs)) 'pkg-three)))
         ;; 4. Test scope 'all installs all 3 packages
         (let ((descs (elpaish-install-local-packages 'all)))
           (should (= (length descs) 3))))))))

(ert-deftest elpaish-test-install-local-delegation ()
  "Test that `elpaish-install-local' delegates to `elpaish-install-local-packages'."
  (elpaish-test-with-temp-env
   (let ((invoked-scope nil))
     (cl-letf (((symbol-function 'elpaish-install-local-packages)
                (lambda (scope &rest _) (setq invoked-scope scope) '(:mock-desc))))
       ;; Nil repository defaults to 'installed
       (should (equal (elpaish-install-local nil) '(:mock-desc)))
       (should (eq invoked-scope 'installed))
       ;; Empty string defaults to 'installed
       (setq invoked-scope nil)
       (should (equal (elpaish-install-local "   ") '(:mock-desc)))
       (should (eq invoked-scope 'installed))
       ;; Scope symbols and keywords delegate appropriately
       (setq invoked-scope nil)
       (should (equal (elpaish-install-local 'upgrade) '(:mock-desc)))
       (should (eq invoked-scope 'upgrade))
       (setq invoked-scope nil)
       (should (equal (elpaish-install-local :all) '(:mock-desc)))
       (should (eq invoked-scope 'all))))))

(ert-deftest elpaish-test-install-local-packages-acr-menu ()
  "Test interactive ACR selection for `elpaish-install-local-packages'."
  (elpaish-test-with-temp-env
   (let* ((repo (expand-file-name "repo" temp-dir))
          (package-user-dir (expand-file-name "installed" temp-dir))
          (package-alist nil)
          (package-selected-packages nil)
          (package-quickstart nil)
          (package-quickstart-file (expand-file-name "quickstart.el" temp-dir))
          (load-path (copy-sequence load-path)))
     (elpaish-test-create-dummy-pkg repo "pkg-one" "1.0" "Package One")
     (elpaish-register-package 'pkg-one repo)
     (let ((elpaish-install-packages-list '(pkg-one))
           (elpaish-install-upgrade-packages-list '(pkg-one)))
       (cl-letf (((symbol-function 'annotated-completing-read)
                  (lambda (table &rest args)
                    (should (assoc "installed" table))
                    (should (assoc "upgrade" table))
                    (should (assoc "all" table))
                    (should (equal (plist-get args :default) "installed"))
                    (should (equal (plist-get args :prompt) "Install local packages: "))
                    "upgrade"))
                 ((symbol-function 'package--save-selected-packages) #'ignore)
                 ((symbol-function 'native-compile-async) #'ignore))
         ;; Interactive call with prefix arg exercises synchronous ACR execution
         (let* ((current-prefix-arg '(4))
                (descs (call-interactively #'elpaish-install-local-packages)))
           (should (= (length descs) 1))
           (should (eq (package-desc-name (car descs)) 'pkg-one)))
         ;; Interactive call without prefix arg runs asynchronously and returns cancel handle
         (let* ((current-prefix-arg nil)
                (handle (call-interactively #'elpaish-install-local-packages)))
           (should (functionp handle))))))))

(ert-deftest elpaish-test-install-local-packages-async-toggle ()
  "Test prefix argument and async default resolution for `elpaish-install-local-packages'."
  (let ((captured-sync nil)
        (elpaish-install--active-cancel-handle nil))
    (cl-letf (((symbol-function 'elpaish-install--upgrade-run)
               (lambda (_pkgs _refresh sync &rest _args)
                 (setq captured-sync sync)
                 (if sync '(:sync-res) (lambda (&optional _) :cancel-handle))))
              ((symbol-function 'annotated-completing-read) (lambda (&rest _) "installed")))
      ;; Programmatic call with no sync/async: in interactive session (noninteractive=nil) defaults to async
      (let ((noninteractive nil))
        (setq elpaish-install--active-cancel-handle nil)
        (let ((res (elpaish-install-local-packages 'installed)))
          (should (null captured-sync))
          (should (functionp res))))
      ;; Programmatic call with noninteractive=t: defaults to sync
      (let ((noninteractive t))
        (setq elpaish-install--active-cancel-handle nil)
        (let ((res (elpaish-install-local-packages 'installed)))
          (should (eq captured-sync t))))
      ;; Programmatic call with :sync t: forces sync even if noninteractive is nil
      (let ((noninteractive nil))
        (setq elpaish-install--active-cancel-handle nil)
        (let ((res (elpaish-install-local-packages 'installed :sync t)))
          (should (eq captured-sync t))))
      ;; Interactive call with prefix arg: forces sync
      (let ((noninteractive nil)
            (current-prefix-arg '(4)))
        (setq elpaish-install--active-cancel-handle nil)
        (call-interactively #'elpaish-install-local-packages)
        (should (eq captured-sync t)))
      ;; Interactive call without prefix arg: defaults to async
      (let ((noninteractive nil)
            (current-prefix-arg nil))
        (setq elpaish-install--active-cancel-handle nil)
        (call-interactively #'elpaish-install-local-packages)
        (should (null captured-sync))))))

(ert-deftest elpaish-test-upgrade-packages-alias-and-execution ()
  "Test `elpaish-install-upgrade-packages' calls upgrade sequence properly."
  (elpaish-test-with-temp-env
   (let ((upgraded nil)
         (refresh-called nil))
     (cl-letf (((symbol-function 'package-refresh-contents) (lambda () (setq refresh-called t)))
               ((symbol-function 'package-installed-p) (lambda (_) t))
               ((symbol-function 'package-upgrade) (lambda (p) (push p upgraded))))
       (elpaish-install-upgrade-packages 'pkg-a 'pkg-b :sync t)
       (should refresh-called)
       (should (equal upgraded '(pkg-b pkg-a)))))))

(ert-deftest elpaish-test-add-bootstrap-packages-nested-and-dedup ()
  "Test `elpaish-install-register-package' flattens arbitrary nesting and dedups."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list '(pkg-a pkg-b)))
     (elpaish-install-register-package '((pkg-c (pkg-d pkg-a)) "non-symbol" nil (pkg-e (pkg-b pkg-f))))
     (should (equal elpaish-install-packages-list '(pkg-a pkg-b pkg-c pkg-d pkg-e pkg-f))))))

(ert-deftest elpaish-test-bootstrap-packages-async-yielding-by-default ()
  "Test `elpaish-install-bootstrap-packages' is async and yielding by default via `gen-run-async'."
  (elpaish-test-with-temp-env
   (let ((gen-run-called nil)
         (gen-items nil)
         (gen-concurrency nil)
         (callback-called nil))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) t))
               ((symbol-function 'package-upgrade) #'ignore)
               ((symbol-function 'package-refresh-contents) #'ignore)
               ((symbol-function 'gen-run-async)
                (lambda (worker items &rest plist)
                  (setq gen-run-called t
                        gen-items items
                        gen-concurrency (plist-get plist :concurrency))
                  (dolist (item items)
                    (funcall worker item #'ignore))
                  (when-let* ((done (plist-get plist :on-done)))
                    (funcall done))
                  (lambda (&optional _) nil))))
       (let ((handle (elpaish-install-bootstrap-packages 'pkg-1 'pkg-2
                                                        :callback (lambda (_) (setq callback-called t)))))
         (should gen-run-called)
         (should (equal gen-items '(pkg-1 pkg-2)))
         (should (= gen-concurrency 1))
         (should callback-called)
         (should (functionp handle)))))))

(ert-deftest elpaish-test-bootstrap-packages-prefix-override-sync ()
  "Test interactive prefix argument forces synchronous execution."
  (elpaish-test-with-temp-env
   (let ((gen-run-called nil)
         (installed-log nil))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-install) (lambda (p) (push p installed-log)))
               ((symbol-function 'package-refresh-contents) #'ignore)
               ((symbol-function 'gen-run-async)
                (lambda (&rest _) (setq gen-run-called t) (lambda (&optional _) nil))))
       ;; Calling interactively with prefix arg '(4) forces synchronous execution
       (let ((current-prefix-arg '(4))
             (elpaish-install-packages-list '(pkg-sync-a pkg-sync-b)))
         (call-interactively #'elpaish-install-bootstrap-packages)
         (should-not gen-run-called)
         (should (equal installed-log '(pkg-sync-b pkg-sync-a))))))))

(ert-deftest elpaish-test-bootstrap-packages-combined-install-and-upgrade ()
  "Test `elpaish-install-bootstrap-packages' installs uninstalled and upgrades installed packages."
  (elpaish-test-with-temp-env
   (let ((installed nil)
         (upgraded nil))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (p) (eq p 'pkg-installed)))
               ((symbol-function 'package-install) (lambda (p) (push p installed)))
               ((symbol-function 'package-upgrade) (lambda (p) (push p upgraded)))
               ((symbol-function 'package-refresh-contents) #'ignore))
       (let ((res (elpaish-install-bootstrap-packages 'pkg-new 'pkg-installed :sync t)))
         (should (equal installed '(pkg-new)))
         (should (equal upgraded '(pkg-installed)))
         (should (equal (plist-get (car res) :status) :installed))
         (should (equal (plist-get (cadr res) :status) :upgraded)))))))

(ert-deftest elpaish-test-bootstrap-packages-reload-in-session ()
  "Test `elpaish-install-bootstrap-packages' reloads packages in the current session."
  (elpaish-test-with-temp-env
   (let* ((desc (package-desc-create :name 'pkg-reload :version '(2 0 0)))
          (package-alist `((pkg-reload ,desc)))
          (reloaded-desc nil))
     (cl-letf (((symbol-function 'package-initialize) #'ignore)
               ((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-install) #'ignore)
               ((symbol-function 'package-activate) #'ignore)
               ((symbol-function 'package--reload-previously-loaded) (lambda (d &optional _) (setq reloaded-desc d)))
               ((symbol-function 'package-refresh-contents) #'ignore))
       (elpaish-install-bootstrap-packages 'pkg-reload :sync t)
       (should (equal reloaded-desc desc))))))

(ert-deftest elpaish-test-bootstrap-packages-cancel ()
  "Test `elpaish-install-cancel' cancels active async installation."
  (elpaish-test-with-temp-env
   (let ((cancel-called nil))
     (setq elpaish-install--active-cancel-handle
           (lambda (action) (when (eq action :cancel) (setq cancel-called t))))
     (elpaish-install-cancel)
     (should cancel-called)
     (should-not elpaish-install--active-cancel-handle))))

(ert-deftest elpaish-test-process-package-legacy-upgrade ()
  "Test `elpaish-install--process-package' upgrade path when `package-upgrade' is unbound."
  (elpaish-test-with-temp-env
   (let* ((desc-old-inst (package-desc-create :name 'pkg-old :version '(1 0 0)))
          (desc-old-arch (package-desc-create :name 'pkg-old :version '(2 0 0)))
          (desc-cur-inst (package-desc-create :name 'pkg-cur :version '(1 0 0)))
          (desc-cur-arch (package-desc-create :name 'pkg-cur :version '(1 0 0)))
          (package-alist `((pkg-old ,desc-old-inst) (pkg-cur ,desc-cur-inst)))
          (package-archive-contents `((pkg-old ,desc-old-arch) (pkg-cur ,desc-cur-arch)))
          (installed-log nil)
          (orig-fboundp (symbol-function 'fboundp)))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (p) (memq p '(pkg-old pkg-cur))))
               ((symbol-function 'fboundp)
                (lambda (fn) (if (eq fn 'package-upgrade) nil (funcall orig-fboundp fn))))
               ((symbol-function 'package-install) (lambda (p) (push p installed-log))))
       ;; pkg-old has newer version in archive -> should install
       (let ((res (elpaish-install--process-package 'pkg-old 'upgrade)))
         (should (equal (plist-get res :status) :upgraded))
         (should (equal installed-log '(pkg-old))))
       ;; pkg-cur has same version -> up to date
       (let ((res (elpaish-install--process-package 'pkg-cur 'upgrade)))
         (should (equal (plist-get res :status) :up-to-date)))))))

(ert-deftest elpaish-test-ensure-package-dependencies ()
  "Test `elpaish-install-ensure-package-dependencies' implicitly installs missing header dependencies."
  (elpaish-test-with-temp-env
   (let ((installed-log nil)
         (recipe (elpaish-recipe-create :name "demo-pkg"
                                       :repository-path "demo-pkg"
                                       :requires '((emacs "28.1") (dep-a "1.0") (dep-b "2.0")))))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (p) (eq p 'dep-b)))
               ((symbol-function 'package-install) (lambda (p) (push p installed-log)))
               ((symbol-function 'package-refresh-contents) #'ignore)
               ((symbol-function 'package-initialize) #'ignore))
       (elpaish-install-ensure-package-dependencies recipe)
       (should (equal installed-log '(dep-a)))))))

(ert-deftest elpaish-test-ensure-package-dependencies-user-error ()
  "Test `elpaish-install-ensure-package-dependencies' signals user-error when package-archives is nil."
  (elpaish-test-with-temp-env
   (let ((package-archives nil)
         (recipe (elpaish-recipe-create :name "demo-pkg"
                                       :repository-path "demo-pkg"
                                       :requires '((emacs "28.1") (missing-dep "1.0")))))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil)))
       (should-error (elpaish-install-ensure-package-dependencies recipe)
                     :type 'user-error)))))

(ert-deftest elpaish-test-ensure-package-dependencies-skips-registry-packages ()
  "Test `elpaish-install-ensure-package-dependencies' skips packages in `elpaish-registry'."
  (elpaish-test-with-temp-env
   (let ((installed-log nil)
         (elpaish-registry (make-hash-table :test 'equal))
         (recipe (elpaish-recipe-create :name "demo-pkg"
                                       :repository-path "demo-pkg"
                                       :requires '((emacs "28.1") (local-pkg "1.0") (remote-dep "2.0")))))
     (puthash "local-pkg" (elpaish-recipe-create :name "local-pkg" :repository-path "local-pkg") elpaish-registry)
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-install) (lambda (p) (push p installed-log)))
               ((symbol-function 'package-refresh-contents) #'ignore)
               ((symbol-function 'package-initialize) #'ignore))
       (elpaish-install-ensure-package-dependencies recipe)
       (should (equal installed-log '(remote-dep)))))))

(ert-deftest elpaish-test-ensure-package-dependencies-continues-on-install-error ()
  "Test `elpaish-install-ensure-package-dependencies' continues when package-install errors."
  (elpaish-test-with-temp-env
   (let ((attempted nil)
         (recipe (elpaish-recipe-create :name "demo-pkg"
                                       :repository-path "demo-pkg"
                                       :requires '((emacs "28.1") (broken-dep "1.0") (good-dep "2.0")))))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-install)
                (lambda (p)
                  (push p attempted)
                  (when (eq p 'broken-dep)
                    (error "Package `broken-dep' is unavailable"))))
               ((symbol-function 'package-refresh-contents) #'ignore)
               ((symbol-function 'package-initialize) #'ignore))
       (elpaish-install-ensure-package-dependencies recipe)
       (should (equal (reverse attempted) '(good-dep broken-dep)))))))

(ert-deftest elpaish-test-ensure-package-dependencies-bare-symbol-requires ()
  "Test `elpaish-install-ensure-package-dependencies' handles bare symbol requirements."
  (elpaish-test-with-temp-env
   (let ((installed-log nil)
         (recipe (elpaish-recipe-create :name "demo-pkg"
                                       :repository-path "demo-pkg"
                                       :requires '(bare-dep (dep-spec "1.0")))))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-install) (lambda (p) (push p installed-log)))
               ((symbol-function 'package-refresh-contents) #'ignore)
               ((symbol-function 'package-initialize) #'ignore))
       (elpaish-install-ensure-package-dependencies recipe)
       (should (equal installed-log '(bare-dep dep-spec)))))))

(ert-deftest elpaish-test-ensure-package-dependencies-direct-file-path ()
  "Test `elpaish-install-ensure-package-dependencies' accepts a direct file path."
  (elpaish-test-with-temp-env
   (let* ((installed-log nil)
          (pkg-file (expand-file-name "demo.el" temp-dir)))
     (with-temp-file pkg-file
       (insert ";;; demo.el --- Demo -*- lexical-binding: t; -*-\n"
               ";; Version: 0.1\n"
               ";; Package-Requires: ((file-dep \"1.0\"))\n"
               "(provide 'demo)\n"))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-install) (lambda (p) (push p installed-log)))
               ((symbol-function 'package-refresh-contents) #'ignore)
               ((symbol-function 'package-initialize) #'ignore))
       (elpaish-install-ensure-package-dependencies pkg-file)
       (should (equal installed-log '(file-dep)))))))

(ert-deftest elpaish-test-install-feedback-hooks ()
  "Test `elpaish-install-package-updated-functions' and finished hooks."
  (elpaish-test-with-temp-env
   (let ((updated-events nil)
         (finished-events nil))
     (let ((elpaish-install-package-updated-functions
            (list (lambda (pkg status res)
                    (push (list pkg status (plist-get res :status)) updated-events))))
           (elpaish-install-bootstrap-finished-functions
            (list (lambda (res-list action)
                    (push (list (length res-list) action) finished-events)))))
       (cl-letf (((symbol-function 'package-installed-p)
                  (lambda (p) (eq p 'pkg-installed)))
                 ((symbol-function 'package-install) #'ignore)
                 ((symbol-function 'package-upgrade) #'ignore)
                 ((symbol-function 'package-refresh-contents) #'ignore))
         (elpaish-install-bootstrap-packages 'pkg-new 'pkg-installed :sync t)
         (should (equal (reverse updated-events)
                        '((pkg-new installed :installed)
                          (pkg-installed upgraded :upgraded))))
         (should (equal finished-events '((2 upgrade)))))))))

(ert-deftest elpaish-test-install-after-package-install-hook ()
  "Run the after-install hook once per installed or upgraded package.
`elpaish-install-after-package-install-hook' is a normal hook, so functions
take no arguments, and it runs alongside
`elpaish-install-package-updated-functions'."
  (elpaish-test-with-temp-env
   (let ((runs 0)
         (updated nil))
     (let ((elpaish-install-after-package-install-hook
            (list (lambda () (cl-incf runs))))
           (elpaish-install-package-updated-functions
            (list (lambda (pkg status _res) (push (list pkg status) updated)))))
       (cl-letf (((symbol-function 'package-installed-p)
                  (lambda (p) (eq p 'pkg-installed)))
                 ((symbol-function 'package-install) #'ignore)
                 ((symbol-function 'package-upgrade) #'ignore)
                 ((symbol-function 'package-refresh-contents) #'ignore))
         (elpaish-install-bootstrap-packages 'pkg-new 'pkg-installed :sync t)
         (should (= runs 2))
         (should (equal (reverse updated)
                        '((pkg-new installed) (pkg-installed upgraded)))))))))

(ert-deftest elpaish-test-report-summary-formatting ()
  "Test `elpaish-install--report-summary' formats named package progress and results."
  (let* ((last-msg nil)
         (hook-fn (lambda (ev) (setq last-msg (plist-get ev :msg)))))
    (add-hook 'logx-event-hook hook-fn)
    (unwind-protect
        (progn
          ;; Case 1: all up to date
          (elpaish-install--report-summary
           '((:status :up-to-date :pkg pkg-1) (:status :up-to-date :pkg pkg-2)) 'upgrade)
          (should (string-match-p "Bootstrap complete: all 2 package(s) up to date" last-msg))

          ;; Case 2: upgraded and installed
          (elpaish-install--report-summary
           '((:status :upgraded :pkg trans)
             (:status :installed :pkg gen)
             (:status :up-to-date :pkg compat)) 'upgrade)
          (should (string-match-p "1 upgraded (trans)" last-msg))
          (should (string-match-p "1 installed (gen)" last-msg))
          (should (string-match-p "1 up to date" last-msg))

          ;; Case 3: failed package
          (elpaish-install--report-summary
           '((:status :failed :pkg broken :error "not found")) 'upgrade)
          (should (string-match-p "1 failed (broken)" last-msg)))
      (remove-hook 'logx-event-hook hook-fn))))
(ert-deftest elpaish-test-recipe-exclude-files ()
  "Test `:exclude-files' filters matching files in `elpaish--collect-files'."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "ex-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (with-temp-file (expand-file-name "ex-pkg.el" pkg-dir)
       (insert ";;; ex-pkg.el --- Test -*- lexical-binding: t; -*-\n(provide 'ex-pkg)\n"))
     (with-temp-file (expand-file-name "other-pkg.el" pkg-dir)
       (insert ";;; other-pkg.el --- Other -*- lexical-binding: t; -*-\n(provide 'other-pkg)\n"))
     (with-temp-file (expand-file-name "sibling-pkg-pkg.el" pkg-dir)
       (insert "(define-package \"sibling-pkg\" \"1.0.0\" \"Sibling\")\n"))
     (let* ((recipe (elpaish-register-package
                     'ex-pkg pkg-dir
                     :files '("*.el")
                     :exclude-files '("other-pkg.el")))
            (collected (elpaish--collect-files pkg-dir '("*.el") "ex-pkg" recipe)))
       (should (member "ex-pkg.el" collected))
       (should-not (member "other-pkg.el" collected))
       (should-not (member "sibling-pkg-pkg.el" collected))))))

(ert-deftest elpaish-test-tar-package-from-pkg-descriptor ()
  "Test packaging from <name>-pkg.el and asset files without <name>.el."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "asset-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (with-temp-file (expand-file-name "asset-pkg-pkg.el" pkg-dir)
       (insert "(define-package \"asset-pkg\" \"1.0.0\"\n  \"Asset Package Summary\"\n  '()\n  :url \"https://example.com/asset\"\n  :keywords '(\"assets\"))\n"))
     (with-temp-file (expand-file-name "key.asc" pkg-dir)
       (insert "-----BEGIN PGP PUBLIC KEY BLOCK-----\ntest\n-----END PGP PUBLIC KEY BLOCK-----\n"))
     (let ((recipe (elpaish-register-package
                    'asset-pkg pkg-dir
                    :files '("key.asc" "asset-pkg-pkg.el")
                    :preflight-skip t)))
       (let ((dest (elpaish-build-package recipe 'snapshot (expand-file-name "snapshot" elpaish-output-dir))))
         (should dest)
         (should (string-suffix-p ".tar" dest))
         (should (file-exists-p dest))
         (should (equal (elpaish-recipe-summary recipe) "Asset Package Summary"))
         (should (equal (elpaish-recipe-url recipe) "https://example.com/asset"))
         (should (equal (elpaish-recipe-keywords recipe) '("assets"))))))))


(ert-deftest elpaish-test-package-origin-labels-and-column ()
  "Test origin classification and rendering for fork, mirror, and tychoish."
  (elpaish-test-with-temp-env
   (let ((f-dir (expand-file-name "fork-pkg" temp-dir))
         (m-dir (expand-file-name "m-pkg" temp-dir))
         (tp-dir (expand-file-name "tp-pkg" temp-dir))
         (ty-dir (expand-file-name "tycho-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg f-dir "fork-pkg" "1.0.0" "Fork Test")
     (elpaish-test-create-dummy-pkg m-dir "m-pkg" "1.0.0" "Mirror Test")
     (elpaish-test-create-dummy-pkg tp-dir "tp-pkg" "1.0.0" "Third Party Test")
     (elpaish-test-create-dummy-pkg ty-dir "tycho-pkg" "1.0.0" "Tychoish Test")
     (elpaish-register-package 'fork-pkg f-dir :fork t)
     (elpaish-register-package 'm-pkg m-dir :mirror t)
     (elpaish-register-package 'tp-pkg tp-dir :external t)
     (elpaish-register-package 'tycho-pkg ty-dir)
     (let ((r-fork (gethash "fork-pkg" elpaish-registry))
           (r-m (gethash "m-pkg" elpaish-registry))
           (r-tp (gethash "tp-pkg" elpaish-registry))
           (r-ty (gethash "tycho-pkg" elpaish-registry)))
       (should (elpaish-recipe-fork-p r-fork))
       (should (equal (elpaish-recipe-origin-label r-fork) "fork"))
       (should (elpaish-recipe-external-p r-m))
       (should (equal (elpaish-recipe-origin-label r-m) "mirror"))
       (should (elpaish-recipe-external-p r-tp))
       (should (equal (elpaish-recipe-origin-label r-tp) "mirror"))
       (should-not (elpaish-recipe-fork-p r-ty))
       (should-not (elpaish-recipe-external-p r-ty))
       (should (equal (elpaish-recipe-origin-label r-ty) "tychoish"))
       (elpaish-build-all 'snapshot)
       (elpaish-generate-stream-index 'snapshot)
       (let ((index-file (expand-file-name "snapshot/index.html" elpaish-output-dir)))
         (with-temp-buffer
           (insert-file-contents index-file)
           (goto-char (point-min))
           (should (search-forward "<th class=\"pkg-origin-cell\">Source</th>" nil t))
           (goto-char (point-min))
           (should (search-forward "pkg-fork-tag" nil t))
           (goto-char (point-min))
           (should (search-forward ">fork</span>" nil t))
           (goto-char (point-min))
           (should (search-forward "pkg-mirror-tag" nil t))
           (goto-char (point-min))
           (should (search-forward ">mirror</span>" nil t))
           (goto-char (point-min))
           (should (search-forward "pkg-tychoish-tag" nil t))
           (goto-char (point-min))
           (should (search-forward ">tychoish</span>" nil t))))))))

(ert-deftest elpaish-test-tar-package-info-default-opt-in ()
  "Test that by default (without :info-files), documentation files are not converted."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "defaultdocpkg" temp-dir))
         (out-dir (expand-file-name "snapshot" elpaish-output-dir)))
     (make-directory (expand-file-name "docs" pkg-dir) t)
     (with-temp-file (expand-file-name "defaultdocpkg.el" pkg-dir)
       (insert ";;; defaultdocpkg.el --- Default Doc Pkg -*- lexical-binding: t; -*-\n")
       (insert ";;; Version: 1.0.0\n")
       (insert ";;; URL: https://example.com/defaultdoc\n")
       (insert ";;; Commentary:\n;; Default doc commentary.\n")
       (insert ";;; Code:\n(defun defaultdocpkg-fn () t)\n(provide 'defaultdocpkg)\n;;; defaultdocpkg.el ends here\n"))
     (with-temp-file (expand-file-name "docs/manual.org" pkg-dir)
       (insert "#+title: Default Doc Manual\n* Overview\nRaw documentation.\n"))
     (let* ((elpaish-registry (make-hash-table :test 'equal))
            (recipe (elpaish-register-package
                     'defaultdocpkg pkg-dir
                     :files '("defaultdocpkg.el" "docs/manual.org")
                     :preflight-skip t)))
       (cl-letf (((symbol-function 'elpaish-derive-version) (lambda (&rest _) "1.0.0")))
         (let ((dest (elpaish-build-package recipe 'snapshot out-dir)))
           (should dest)
           (should (string-suffix-p ".tar" dest))
           (let ((members (process-lines "tar" "-tf" dest)))
             ;; Raw source org file should be preserved
             (should (seq-some (lambda (m) (string-suffix-p "manual.org" m)) members))
             ;; No Info manual or dir index should be generated
             (should-not (seq-some (lambda (m) (string-suffix-p "manual.info" m)) members))
             (should-not (seq-some (lambda (m) (string-suffix-p "/dir" m)) members)))))))))

(ert-deftest elpaish-test-tar-package-info-generation-opt-in-file ()
  "Test opt-in Info manual generation for a specific file, preserving other Org files."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "infopkg" temp-dir))
         (out-dir (expand-file-name "snapshot" elpaish-output-dir)))
     (make-directory (expand-file-name "docs" pkg-dir) t)
     (with-temp-file (expand-file-name "infopkg.el" pkg-dir)
       (insert ";;; infopkg.el --- Info Pkg -*- lexical-binding: t; -*-\n")
       (insert ";;; Version: 1.0.0\n")
       (insert ";;; URL: https://example.com/infopkg\n")
       (insert ";;; Commentary:\n;; Info package commentary.\n")
       (insert ";;; Code:\n(defun infopkg-fn () t)\n(provide 'infopkg)\n;;; infopkg.el ends here\n"))
     (with-temp-file (expand-file-name "README.org" pkg-dir)
       (insert "#+title: Infopkg Readme\n* Readme\nProject readme.\n"))
     (with-temp-file (expand-file-name "docs/manual.org" pkg-dir)
       (insert "#+title: Infopkg Manual\n* Overview\nThis is the Infopkg user documentation.\n"))
     (let* ((elpaish-registry (make-hash-table :test 'equal))
            (recipe (elpaish-register-package
                     'infopkg pkg-dir
                     :files '("infopkg.el" "README.org" "docs/manual.org")
                     :info-files "docs/manual.org"
                     :preflight-skip t)))
       (cl-letf (((symbol-function 'elpaish-derive-version) (lambda (&rest _) "1.0.0")))
         (let ((dest (elpaish-build-package recipe 'snapshot out-dir)))
           (should dest)
           (should (string-suffix-p ".tar" dest))
           (should (file-exists-p dest))
           (let ((members (process-lines "tar" "-tf" dest)))
             ;; Should contain generated Info manual and merged dir file
             (should (seq-some (lambda (m) (string-suffix-p "manual.info" m)) members))
             (should (seq-some (lambda (m) (string-suffix-p "/dir" m)) members))
             ;; Should NOT bundle converted source manual.org
             (should-not (seq-some (lambda (m) (string-suffix-p "manual.org" m)) members))
             ;; README.org was not targeted and should remain as-is in the tarball
             (should (seq-some (lambda (m) (string-suffix-p "README.org" m)) members)))
           ;; Upstream repo directory should remain completely untouched
           (should (file-exists-p (expand-file-name "docs/manual.org" pkg-dir)))
           (should-not (file-exists-p (expand-file-name "docs/manual.texi" pkg-dir)))
           (should-not (file-exists-p (expand-file-name "docs/manual.info" pkg-dir)))))))))

(ert-deftest elpaish-test-tar-package-info-generation-opt-in-list ()
  "Test opt-in Info manual generation when a list of files is specified."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "infolistpkg" temp-dir))
         (out-dir (expand-file-name "snapshot" elpaish-output-dir)))
     (make-directory (expand-file-name "docs" pkg-dir) t)
     (with-temp-file (expand-file-name "infolistpkg.el" pkg-dir)
       (insert ";;; infolistpkg.el --- Info List Pkg -*- lexical-binding: t; -*-\n")
       (insert ";;; Version: 1.0.0\n")
       (insert ";;; URL: https://example.com/infolist\n")
       (insert ";;; Commentary:\n;; Commentary.\n")
       (insert ";;; Code:\n(defun infolistpkg-fn () t)\n(provide 'infolistpkg)\n;;; infolistpkg.el ends here\n"))
     (with-temp-file (expand-file-name "docs/manual.org" pkg-dir)
       (insert "#+title: Infolist Manual\n* Overview\nManual.\n"))
     (let* ((elpaish-registry (make-hash-table :test 'equal))
            (recipe (elpaish-register-package
                     'infolistpkg pkg-dir
                     :files '("infolistpkg.el" "docs/manual.org")
                     :info-files '("docs/manual.org")
                     :preflight-skip t)))
       (cl-letf (((symbol-function 'elpaish-derive-version) (lambda (&rest _) "1.0.0")))
         (let ((dest (elpaish-build-package recipe 'snapshot out-dir)))
           (should dest)
           (should (string-suffix-p ".tar" dest))
           (let ((members (process-lines "tar" "-tf" dest)))
             (should (seq-some (lambda (m) (string-suffix-p "manual.info" m)) members))
             (should (seq-some (lambda (m) (string-suffix-p "/dir" m)) members))
             (should-not (seq-some (lambda (m) (string-suffix-p "manual.org" m)) members)))))))))

(ert-deftest elpaish-test-tar-package-info-skip ()
  "Test :info-skip t suppresses Info manual generation even if :info-files is set."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "skipinfopkg" temp-dir))
         (out-dir (expand-file-name "snapshot" elpaish-output-dir)))
     (make-directory (expand-file-name "docs" pkg-dir) t)
     (with-temp-file (expand-file-name "skipinfopkg.el" pkg-dir)
       (insert ";;; skipinfopkg.el --- Skip Info Pkg -*- lexical-binding: t; -*-\n")
       (insert ";;; Version: 1.0.0\n")
       (insert ";;; URL: https://example.com/skipinfo\n")
       (insert ";;; Commentary:\n;; Skip Info commentary.\n")
       (insert ";;; Code:\n(defun skipinfopkg-fn () t)\n(provide 'skipinfopkg)\n;;; skipinfopkg.el ends here\n"))
     (with-temp-file (expand-file-name "docs/manual.org" pkg-dir)
       (insert "#+title: Skip Infopkg Manual\n* Overview\nRaw Org manual.\n"))
     (let* ((elpaish-registry (make-hash-table :test 'equal))
            (recipe (elpaish-register-package
                     'skipinfopkg pkg-dir
                     :files '("skipinfopkg.el" "docs/manual.org")
                     :info-files "docs/manual.org"
                     :info-skip t
                     :preflight-skip t)))
       (cl-letf (((symbol-function 'elpaish-derive-version) (lambda (&rest _) "1.0.0")))
         (let ((dest (elpaish-build-package recipe 'snapshot out-dir)))
           (should dest)
           (should (string-suffix-p ".tar" dest))
           (let ((members (process-lines "tar" "-tf" dest)))
             ;; Raw source org file should be preserved
             (should (seq-some (lambda (m) (string-suffix-p "manual.org" m)) members))
             ;; Neither manual.info nor dir should be generated
             (should-not (seq-some (lambda (m) (string-suffix-p "manual.info" m)) members))
             (should-not (seq-some (lambda (m) (string-suffix-p "/dir" m)) members)))))))))

(ert-deftest elpaish-test-tar-package-info-global-disable ()
  "Test elpaish-generate-info-manuals nil globally suppresses Info manual generation."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "globaldispkg" temp-dir))
         (out-dir (expand-file-name "snapshot" elpaish-output-dir))
         (elpaish-generate-info-manuals nil))
     (make-directory (expand-file-name "docs" pkg-dir) t)
     (with-temp-file (expand-file-name "globaldispkg.el" pkg-dir)
       (insert ";;; globaldispkg.el --- Global Dis Pkg -*- lexical-binding: t; -*-\n")
       (insert ";;; Version: 1.0.0\n")
       (insert ";;; URL: https://example.com/globaldis\n")
       (insert ";;; Commentary:\n;; Commentary.\n")
       (insert ";;; Code:\n(defun globaldispkg-fn () t)\n(provide 'globaldispkg)\n;;; globaldispkg.el ends here\n"))
     (with-temp-file (expand-file-name "docs/manual.org" pkg-dir)
       (insert "#+title: Global Disable Manual\n* Overview\nOrg manual.\n"))
     (let* ((elpaish-registry (make-hash-table :test 'equal))
            (recipe (elpaish-register-package
                     'globaldispkg pkg-dir
                     :files '("globaldispkg.el" "docs/manual.org")
                     :info-files "docs/manual.org"
                     :preflight-skip t)))
       (cl-letf (((symbol-function 'elpaish-derive-version) (lambda (&rest _) "1.0.0")))
         (let ((dest (elpaish-build-package recipe 'snapshot out-dir)))
           (should dest)
           (should (string-suffix-p ".tar" dest))
           (let ((members (process-lines "tar" "-tf" dest)))
             (should (seq-some (lambda (m) (string-suffix-p "manual.org" m)) members))
             (should-not (seq-some (lambda (m) (string-suffix-p "manual.info" m)) members))
             (should-not (seq-some (lambda (m) (string-suffix-p "/dir" m)) members)))))))))

(ert-deftest elpaish-test-tar-package-texinfo-missing-fallback ()
  "Test graceful degradation when makeinfo/install-info executables are missing."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "missingtexipkg" temp-dir))
         (out-dir (expand-file-name "snapshot" elpaish-output-dir))
         (elpaish--texinfo-warned nil))
     (make-directory (expand-file-name "docs" pkg-dir) t)
     (with-temp-file (expand-file-name "missingtexipkg.el" pkg-dir)
       (insert ";;; missingtexipkg.el --- Missing Texi -*- lexical-binding: t; -*-\n")
       (insert ";;; Version: 1.0.0\n")
       (insert ";;; URL: https://example.com/missingtexi\n")
       (insert ";;; Commentary:\n;; Commentary.\n")
       (insert ";;; Code:\n(defun missingtexipkg-fn () t)\n(provide 'missingtexipkg)\n;;; missingtexipkg.el ends here\n"))
     (with-temp-file (expand-file-name "docs/manual.org" pkg-dir)
       (insert "#+title: Missing Texi Manual\n* Overview\nOrg manual.\n"))
     (let* ((elpaish-registry (make-hash-table :test 'equal))
            (recipe (elpaish-register-package
                     'missingtexipkg pkg-dir
                     :files '("missingtexipkg.el" "docs/manual.org")
                     :info-files "docs/manual.org"
                     :preflight-skip t)))
       (cl-letf (((symbol-function 'elpaish-derive-version) (lambda (&rest _) "1.0.0"))
                 ((symbol-function 'elpaish--texinfo-available-p) (lambda () nil)))
         (let ((dest (elpaish-build-package recipe 'snapshot out-dir)))
           (should dest)
           (should (string-suffix-p ".tar" dest))
           (should elpaish--texinfo-warned)
           (let ((members (process-lines "tar" "-tf" dest)))
             (should (seq-some (lambda (m) (string-suffix-p "manual.org" m)) members))
             (should-not (seq-some (lambda (m) (string-suffix-p "manual.info" m)) members))
             (should-not (seq-some (lambda (m) (string-suffix-p "/dir" m)) members)))))))))

(ert-deftest elpaish-test-async-error-p ()
  "Test `elpaish-async-error-p' predicate."
  (should (elpaish-async-error-p '(:error (error "failure") :target local)))
  (should (elpaish-async-error-p '(:error "raw-error")))
  (should-not (elpaish-async-error-p nil))
  (should-not (elpaish-async-error-p '(:ok "success")))
  (should-not (elpaish-async-error-p "/path/to/artifact.tar")))

(ert-deftest elpaish-test-async-mapcar-synchronous-fallback ()
  "Test `elpaish-async-mapcar' collecting items and wrapping signaled errors."
  (let ((items '(1 2 "bad" 4)))
    (let ((results (elpaish-async-mapcar
                    (lambda (x)
                      (if (stringp x)
                          (error "not a number: %s" x)
                        (* x 10)))
                    items)))
      (should (equal (nth 0 results) 10))
      (should (equal (nth 1 results) 20))
      (should (elpaish-async-error-p (nth 2 results)))
      (should (equal (nth 3 results) 40)))))

(ert-deftest elpaish-test-warm-gpg-agent ()
  "Test `elpaish--warm-gpg-agent' execution safety."
  (let ((elpaish-sign-packages nil))
    (should-not (elpaish--warm-gpg-agent)))
  (let ((elpaish-sign-packages t))
    (should (member (elpaish--warm-gpg-agent) '(0 nil 1)))))

(ert-deftest elpaish-test-archive-contents-deterministic-sorting ()
  "Test that `archive-contents' generates alphabetically sorted entries."
  (elpaish-test-with-temp-env
   (let* ((out-dir (expand-file-name "snapshot" elpaish-output-dir))
          (pkg-z-dir (expand-file-name "z-pkg" temp-dir))
          (pkg-a-dir (expand-file-name "a-pkg" temp-dir))
          (pkg-m-dir (expand-file-name "m-pkg" temp-dir)))
     (make-directory out-dir t)
     (make-directory pkg-z-dir t)
     (make-directory pkg-a-dir t)
     (make-directory pkg-m-dir t)
     (with-temp-file (expand-file-name "z-pkg.el" pkg-z-dir)
       (insert ";;; z-pkg.el --- Z -*- lexical-binding: t; -*-\n;; Version: 1.0.0\n(provide 'z-pkg)\n"))
     (with-temp-file (expand-file-name "a-pkg.el" pkg-a-dir)
       (insert ";;; a-pkg.el --- A -*- lexical-binding: t; -*-\n;; Version: 1.0.0\n(provide 'a-pkg)\n"))
     (with-temp-file (expand-file-name "m-pkg.el" pkg-m-dir)
       (insert ";;; m-pkg.el --- M -*- lexical-binding: t; -*-\n;; Version: 1.0.0\n(provide 'm-pkg)\n"))
     (let ((elpaish-registry (make-hash-table :test 'equal))
           (elpaish-sign-packages nil))
       (elpaish-register-package 'z-pkg pkg-z-dir :preflight-skip t)
       (elpaish-register-package 'a-pkg pkg-a-dir :preflight-skip t)
       (elpaish-register-package 'm-pkg pkg-m-dir :preflight-skip t)
       (setf (elpaish-recipe-version-for-stream (gethash "z-pkg" elpaish-registry) 'snapshot) "1.0.0")
       (setf (elpaish-recipe-version-for-stream (gethash "a-pkg" elpaish-registry) 'snapshot) "1.0.0")
       (setf (elpaish-recipe-version-for-stream (gethash "m-pkg" elpaish-registry) 'snapshot) "1.0.0")
       (elpaish-generate-archive-contents 'snapshot out-dir)
       (let* ((archive-file (expand-file-name "archive-contents" out-dir))
              (contents (with-temp-buffer
                          (insert-file-contents archive-file)
                          (read (current-buffer))))
              (entries (cdr contents))
              (names (mapcar #'car entries)))
         (should (equal names '(a-pkg m-pkg z-pkg))))))))

(ert-deftest elpaish-test-gnupg-dir-resolution ()
  "Test that `elpaish--gnupg-dir' respects GNUPGHOME without appending .gnupg."
  (let ((process-environment (cons "GNUPGHOME=/custom/gpg/home" process-environment)))
    (should (equal (elpaish--gnupg-dir) (expand-file-name "/custom/gpg/home"))))
  (let ((process-environment (cons "GNUPGHOME=" (cons "HOME=/tmp/fakehome" process-environment))))
    (should (equal (elpaish--gnupg-dir) (expand-file-name ".gnupg" "/tmp/fakehome")))))

(ert-deftest elpaish-test-build-all-async-failure-quarantine ()
  "Test that `elpaish-build-all' quarantines packages that fail in async worker."
  (elpaish-test-with-temp-env
   (let* ((pkg1-dir (expand-file-name "pkg1" temp-dir))
          (pkg2-dir (expand-file-name "pkg2" temp-dir)))
     (make-directory pkg1-dir t)
     (make-directory pkg2-dir t)
     (with-temp-file (expand-file-name "pkg1.el" pkg1-dir)
       (insert ";;; pkg1.el --- Pkg 1 -*- lexical-binding: t; -*-\n;; Version: 1.0.0\n(provide 'pkg1)\n"))
     (with-temp-file (expand-file-name "pkg2.el" pkg2-dir)
       (insert ";;; pkg2.el --- Pkg 2 -*- lexical-binding: t; -*-\n;; Version: 1.0.0\n(provide 'pkg2)\n"))
     (let ((elpaish-registry (make-hash-table :test 'equal))
           (elpaish-sign-packages nil)
           (elpaish-run-preflight nil))
       (elpaish-register-package 'pkg1 pkg1-dir :preflight-skip t)
       (elpaish-register-package 'pkg2 pkg2-dir :preflight-skip t)
       (cl-letf (((symbol-function 'elpaish-build-package)
                  (lambda (rec stream dir)
                    (if (equal (elpaish-recipe-name rec) "pkg2")
                        (error "Simulated async worker build failure for %s" (elpaish-recipe-name rec))
                      (expand-file-name "pkg1-1.0.0.tar" dir)))))
         (should-error (elpaish-build-all 'snapshot elpaish-output-dir)))))))

(ert-deftest elpaish-test-two-lists-install-and-upgrade-subset ()
  "Test two package lists and strict subset maintenance for upgrades."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list '(pkg-base-1 pkg-base-2))
         (elpaish-install-upgrade-packages-list nil))
     ;; Registering an upgrade package automatically ensures it is in List 1
     (elpaish-install-register-upgrade-package 'pkg-base-1 'pkg-new-up)
     (should (member 'pkg-new-up elpaish-install-packages-list))
     (should (member 'pkg-base-1 elpaish-install-upgrade-packages-list))
     (should (member 'pkg-new-up elpaish-install-upgrade-packages-list))

     ;; If List 2 is manually manipulated with an item not in List 1,
     ;; `elpaish-install--get-upgrade-packages' filters it out to enforce strict subset
     (setq elpaish-install-upgrade-packages-list '(pkg-base-1 pkg-not-in-list1))
     (should (equal (elpaish-install--get-upgrade-packages) '(pkg-base-1))))))

(ert-deftest elpaish-test-register-group-upgrade-and-install-policies ()
  "Test group registration maintains policy, group inventory, and flat lists."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list nil)
         (elpaish-install-upgrade-packages-list nil)
         (elpaish-install-package-groups nil))
     (elpaish-install-register-group 'tools '(tool-a tool-b) 'upgrade)
     (elpaish-install-register-group 'extras '(extra-1 extra-2) 'install)
     (should (equal (elpaish-install-get-group-packages 'tools) '(tool-a tool-b)))
     (should (equal (elpaish-install-get-group-packages 'extras) '(extra-1 extra-2)))
     (should (member 'tool-a elpaish-install-upgrade-packages-list))
     (should (member 'tool-b elpaish-install-upgrade-packages-list))
     (should-not (member 'extra-1 elpaish-install-upgrade-packages-list))
     (should-not (member 'extra-2 elpaish-install-upgrade-packages-list))
     (should (member 'tool-a elpaish-install-packages-list))
     (should (member 'extra-1 elpaish-install-packages-list)))))

(ert-deftest elpaish-test-register-groups-alist ()
  "Test batch registering multiple groups from alist."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list nil)
         (elpaish-install-upgrade-packages-list nil)
         (elpaish-install-package-groups nil)
         (groups '((core . (core-a core-b))
                   (install . (aux-1 aux-2))
                   (install-extra . (aux-3 aux-4))
                   (install-special :packages (aux-5)))))
     (elpaish-install-register-groups groups)
     (should (equal (elpaish-install-get-group-packages 'core) '(core-a core-b)))
     (should (equal (elpaish-install-get-group-packages 'install) '(aux-1 aux-2)))
     (should (equal (elpaish-install-get-group-packages 'install-extra) '(aux-3 aux-4)))
     (should (equal (elpaish-install-get-group-packages 'install-special) '(aux-5)))
     (should (equal elpaish-install-upgrade-packages-list '(core-a core-b)))
     (should (equal elpaish-install-packages-list '(core-a core-b aux-1 aux-2 aux-3 aux-4 aux-5))))))

(ert-deftest elpaish-test-upgrade-packages-by-group ()
  "Test upgrading targeted group only upgrades packages in that group."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list nil)
         (elpaish-install-upgrade-packages-list nil)
         (elpaish-install-package-groups nil)
         (upgraded nil))
     (elpaish-install-register-groups '((group-1 . (g1-a g1-b))
                                       (group-2 . (g2-a g2-b))))
     (cl-letf (((symbol-function 'elpaish-install--upgrade-run)
                (lambda (pkgs &rest _) (setq upgraded pkgs))))
       (elpaish-install-upgrade-packages :group 'group-2 :sync t)
       (should (equal upgraded '(g2-a g2-b)))))))

(ert-deftest elpaish-test-install-packages-blocking-no-refresh-when-installed ()
  "Test `elpaish-install-packages' is blocking and skips refresh when all installed."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list '(installed-a installed-b))
         (refresh-called nil)
         (install-called nil))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) t))
               ((symbol-function 'package-refresh-contents)
                (lambda () (setq refresh-called t)))
               ((symbol-function 'package-install)
                (lambda (_) (setq install-called t))))
       ;; Blocking call without missing packages should not refresh or install
       (let ((res (elpaish-install-packages)))
         (should-not res)
         (should-not refresh-called)
         (should-not install-called))))))

(ert-deftest elpaish-test-install-packages-blocking-refreshes-when-missing ()
  "Test `elpaish-install-packages' refreshes and installs blocking when packages missing."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list '(inst-pkg missing-pkg))
         (refresh-called nil)
         (installed-log nil))
     (cl-letf (((symbol-function 'package-installed-p)
                (lambda (p) (eq p 'inst-pkg)))
               ((symbol-function 'package-refresh-contents)
                (lambda () (setq refresh-called t)))
               ((symbol-function 'package-install)
                (lambda (p) (push p installed-log))))
       ;; Missing packages trigger blocking refresh and install
       (let ((res (elpaish-install-packages)))
         (should refresh-called)
         (should (equal installed-log '(missing-pkg)))
         (should (= (length res) 1))
         (should (eq (plist-get (car res) :status) :installed)))))))

(ert-deftest elpaish-test-upgrade-packages-lazy-background-command ()
  "Upgrade stages yield and update progress as each package finishes."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list '(pkg-u1 pkg-u2))
         (elpaish-install-upgrade-packages-list '(pkg-u1 pkg-u2))
         (elpaish-install--active-cancel-handle nil)
         (package-archive-contents '((dummy)))
         (queue nil) (events nil) (result nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_delay repeat fn &rest _)
                  (unless repeat
                    (setq queue (append queue (list fn))))))
               ((symbol-function 'package-refresh-contents) (lambda () (push 'refresh events)))
               ((symbol-function 'elpaish-install--process-package)
                (lambda (pkg &rest _) (push pkg events)
                  (list :pkg pkg :status :upgraded :to "2.0")))
               ((symbol-function 'display-buffer) #'ignore))
       (let ((handle (elpaish-install-upgrade-packages :callback (lambda (res) (setq result res)))))
         (should (functionp handle))
         (should-not events)
         (with-current-buffer "*ELPAish Upgrade Progress*"
           (should (string-match-p "pkg-u1: not installed" (buffer-string))))
         (funcall (pop queue))
         (should (equal events '(refresh)))
         (funcall (pop queue))
         (with-current-buffer "*ELPAish Upgrade Progress*"
           (should (string-match-p (regexp-quote "[x] pkg-u1: 2.0") (buffer-string)))
           (should (string-match-p (regexp-quote "[ ] pkg-u2") (buffer-string))))
         (while queue (funcall (pop queue)))
         (should (equal (mapcar (lambda (r) (plist-get r :pkg)) result) '(pkg-u1 pkg-u2)))
         (should-not elpaish-install--active-cancel-handle))))))

(ert-deftest elpaish-test-upgrade-progress-quickstart-and-failure ()
  "Progress records package failures and a completed quickstart rebuild."
  (elpaish-test-with-temp-env
   (let ((package-archive-contents '((dummy)))
         (elpaish-install--active-cancel-handle nil)
         (elpaish-install-bootstrap-finished-functions '(elpaish-install--on-bootstrap-finished))
         (refreshes 0) (finished nil))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'elpaish-install-flush-package-quickstart)
                (lambda (&optional _) (cl-incf refreshes)))
               ((symbol-function 'elpaish-install--process-package)
                (lambda (pkg &rest _)
                  (if (eq pkg 'good)
                      (list :pkg pkg :status :upgraded :to "3.0")
                    (list :pkg pkg :status :failed :error "broken")))))
       (elpaish-install-upgrade-packages 'good 'bad :sync t :refresh nil
                                         :callback (lambda (res) (setq finished res)))
       (should (= refreshes 1))
       (should (= (length finished) 2))
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (should (string-match-p (regexp-quote "[!] bad: not installed — broken") (buffer-string)))
         (should (string-match-p (regexp-quote "[x] Rebuild package quickstart") (buffer-string))))))))

(ert-deftest elpaish-test-hidden-package-website ()
  "Hidden recipes remain registered but do not appear in the catalog."
  (elpaish-test-with-temp-env
   (let ((hidden (elpaish-register-package 'hidden-pkg temp-dir :hidden t))
         (visible (elpaish-register-package 'visible-pkg temp-dir)))
     (should (elpaish-recipe-hidden hidden))
     (cl-letf (((symbol-function 'elpaish-website--package-rows)
                (lambda (recipe &rest _) `((tr nil (td nil ,(elpaish-recipe-name recipe)))))))
       (elpaish-generate-stream-index 'snapshot)
       (with-temp-buffer
         (insert-file-contents (expand-file-name "snapshot/index.html" elpaish-output-dir))
         (should (search-forward "visible-pkg" nil t))
         (goto-char (point-min))
         (should-not (search-forward "hidden-pkg" nil t))))
     (should (eq hidden (gethash "hidden-pkg" elpaish-registry)))
     (should (eq visible (gethash "visible-pkg" elpaish-registry))))))

(ert-deftest elpaish-test-bootstrap-finished-schedules-quickstart ()
  "Schedule a quickstart refresh only when packages changed."
  (let ((refreshes 0))
    (cl-letf (((symbol-function 'elpaish-install-schedule-quickstart-refresh)
               (lambda () (cl-incf refreshes))))
      (elpaish-install--on-bootstrap-finished '((:status :unchanged)) 'install)
      (should (= refreshes 0))
      (elpaish-install--on-bootstrap-finished '((:status :installed)) 'install)
      (elpaish-install--on-bootstrap-finished '((:status :upgraded)) 'upgrade)
      (should (= refreshes 2)))))

(ert-deftest elpaish-test-flush-package-quickstart-lifecycle ()
  "Test `elpaish-install-flush-package-quickstart' only refreshes when pending or forced."
  (let ((refreshes 0)
        (elpaish-install--quickstart-refresh-timer nil))
    (cl-letf (((symbol-function 'elpaish-install-refresh-package-quickstart)
               (lambda (&rest _) (cl-incf refreshes))))
      ;; When no timer is pending and force is nil, does nothing.
      (elpaish-install-flush-package-quickstart)
      (should (= refreshes 0))
      ;; When forced, refreshes immediately.
      (elpaish-install-flush-package-quickstart t)
      (should (= refreshes 1))
      ;; When a timer is active, cancels timer and refreshes.
      (let ((timer (run-at-time 100 nil #'ignore)))
        (setq elpaish-install--quickstart-refresh-timer timer)
        (elpaish-install-flush-package-quickstart)
        (should (= refreshes 2))
        (should-not elpaish-install--quickstart-refresh-timer)))))

(ert-deftest elpaish-test-quickstart-optimization-streamlines-content ()
  "Test `elpaish-install-optimize-quickstart-file' streamlines quickstart contents."
  (let ((tmp-file (make-temp-file "test-elpaish-qs-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp-file
            (insert ";;; quickstart.el -*- lexical-binding: t; -*-\n\n\n"
                    "(let* ((load-file-name \"/tmp/elpa/pkg-1.0/pkg-autoloads.el\")\n"
                    "       (load-true-file-name load-file-name))\n"
                    "  (add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))\n"
                    "  (register-definition-prefixes \"pkg\" '(\"pkg-\"))\n"
                    "  (autoload 'pkg-func \"pkg\" \"This is a long docstring.\" t nil)\n"
                    "  (provide 'pkg-autoloads))\n"))
          (elpaish-install-optimize-quickstart-file tmp-file)
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "(setq load-path" content))
              (should (string-match-p "/tmp/elpa/pkg-1.0" content))
              (should-not (string-match-p "register-definition-prefixes" content))
              (should-not (string-match-p "This is a long docstring" content))
              (should (string-match-p "(autoload 'pkg-func \"pkg\" nil t nil)" content))
              (should-not (string-match-p "provide 'pkg-autoloads" content)))))
      (when (file-exists-p tmp-file)
        (delete-file tmp-file)))))

(ert-deftest elpaish-test-quickstart-staleness-detection ()
  "Test `elpaish-install-package-quickstart-stale-p' detects missing .elc."
  (let* ((tmp-el (make-temp-file "test-elpaish-stale-" nil ".el"))
         (package-quickstart-file tmp-el))
    (unwind-protect
        (should (elpaish-install-package-quickstart-stale-p))
      (when (file-exists-p tmp-el) (delete-file tmp-el)))))

(ert-deftest elpaish-test-delete-package-artifacts ()
  "Test `elpaish-install-delete-package-artifacts' removes all matching artifacts."
  (let* ((temp-dir (make-temp-file "test-elpaish-artifacts-" t))
         (package-user-dir temp-dir)
         (pkg-dir (expand-file-name "testpkg-1.2.3" temp-dir))
         (tar-file (expand-file-name "testpkg-1.2.3.tar" temp-dir))
         (sig-file (expand-file-name "testpkg-1.2.3.tar.sig" temp-dir))
         (other-dir (expand-file-name "otherpkg-1.0.0" temp-dir))
         (package-alist '((testpkg (1 2 3)) (otherpkg (1 0 0)))))
    (unwind-protect
        (progn
          (make-directory pkg-dir t)
          (make-directory other-dir t)
          (with-temp-file tar-file (insert "dummy tar"))
          (with-temp-file sig-file (insert "dummy sig"))
          (let ((deleted (elpaish-install-delete-package-artifacts 'testpkg)))
            (should (member pkg-dir deleted))
            (should (member tar-file deleted))
            (should (member sig-file deleted))
            (should-not (file-exists-p pkg-dir))
            (should-not (file-exists-p tar-file))
            (should-not (file-exists-p sig-file))
            (should (file-directory-p other-dir))
            (should-not (assq 'testpkg package-alist))
            (should (assq 'otherpkg package-alist))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest elpaish-test-rebuild-local-package-git-checks ()
  "Test `elpaish-install-rebuild-local-package' validates git repository state."
  (elpaish-test-with-temp-env
   (let* ((recipe (elpaish-register-package 'test-pkg temp-dir))
          (package-user-dir temp-dir)
          (package-alist nil)
          (git-dir (expand-file-name ".git" temp-dir)))
     (make-directory git-dir t)
     ;; 1. Refuse if dirty
     (cl-letf (((symbol-function 'elpaish--git-string)
                (lambda (dir &rest args)
                  (if (member "status" args) " M dirty.el" "origin/main")))
               ((symbol-function 'elpaish--git-lines) (lambda (&rest _) nil)))
       (should-error (elpaish-install-rebuild-local-package 'test-pkg) :type 'user-error))
     ;; 2. Refuse if no upstream remote
     (cl-letf (((symbol-function 'elpaish--git-string)
                (lambda (dir &rest args)
                  (if (member "status" args) "" nil)))
               ((symbol-function 'elpaish--git-lines) (lambda (&rest _) nil)))
       (should-error (elpaish-install-rebuild-local-package 'test-pkg) :type 'user-error))
     ;; 3. Refuse if unpushed commits exist
     (cl-letf (((symbol-function 'elpaish--git-string)
                (lambda (dir &rest args)
                  (if (member "status" args) "" "origin/main")))
               ((symbol-function 'elpaish--git-lines)
                (lambda (&rest _) '("commit-hash-ahead"))))
       (should-error (elpaish-install-rebuild-local-package 'test-pkg) :type 'user-error)))))

(ert-deftest elpaish-test-clean-stale-packages ()
  "Test `elpaish-install-clean-stale-packages' deletes older versions."
  (let* ((temp-dir (make-temp-file "test-elpaish-stale-clean-" t))
         (package-user-dir temp-dir)
         (old-dir (expand-file-name "multipkg-1.0.0" temp-dir))
         (new-dir (expand-file-name "multipkg-2.0.0" temp-dir))
         (old-tar (expand-file-name "multipkg-1.0.0.tar" temp-dir))
         (single-dir (expand-file-name "singlepkg-1.0" temp-dir)))
    (unwind-protect
        (progn
          (make-directory old-dir t)
          (make-directory new-dir t)
          (make-directory single-dir t)
          (with-temp-file old-tar (insert "tar"))
          (cl-letf (((symbol-function 'elpaish-install-schedule-quickstart-refresh) #'ignore))
            (let ((count (elpaish-install-clean-stale-packages t)))
              (should (= count 1))
              (should-not (file-directory-p old-dir))
              (should-not (file-exists-p old-tar))
              (should (file-directory-p new-dir))
              (should (file-directory-p single-dir)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest elpaish-test-quickstart-native-compile ()
  "Test quickstart refresh triggers native-compile when supported."
  (let* ((tmp-el (make-temp-file "test-elpaish-nc-" nil ".el"))
         (package-quickstart-file tmp-el)
         (compiled-file nil))
    (unwind-protect
        (progn
          (with-temp-file tmp-el (insert ";;; quickstart\n"))
          (cl-letf (((symbol-function 'native-comp-available-p) (lambda () t))
                    ((symbol-function 'native-compile-async)
                     (lambda (file) (setq compiled-file file)))
                    ((symbol-function 'package-quickstart-refresh) #'ignore)
                    ((symbol-function 'elpaish-install-sync-package-load-paths) #'ignore))
            (elpaish-install-refresh-package-quickstart t)
            (should (equal compiled-file tmp-el))))
      (when (file-exists-p tmp-el) (delete-file tmp-el)))))

(ert-deftest elpaish-test-progress-mode-and-controls ()
  "Test `elpaish-install-progress-mode' keybindings and restart mechanism."
  (let* ((buf (get-buffer-create "*ELPAish Progress Test*"))
         (restarted nil))
    (unwind-protect
        (with-current-buffer buf
          (elpaish-install-progress-mode)
          (should (eq major-mode 'elpaish-install-progress-mode))
          (should (keymapp (current-local-map)))
          (setq-local elpaish-install--progress-context
                      (list :pkgs '(test-a) :refresh nil :sync t :running nil))
          (cl-letf (((symbol-function 'elpaish-install--upgrade-run)
                     (lambda (pkgs &rest _) (setq restarted pkgs))))
            (elpaish-install-progress-restart)
            (should (equal restarted '(test-a)))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest elpaish-test-resolve-signing-policy ()
  "Test `elpaish--resolve-signing-p' for explicit and auto policies."
  (should-not (elpaish--resolve-signing-p nil))
  (should (eq (elpaish--resolve-signing-p t) t))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/gpg"))
            ((symbol-function 'elpaish--get-signing-key) (lambda () "ABCD1234")))
    (should (eq (elpaish--resolve-signing-p 'auto) t))
    (should (eq (elpaish--resolve-signing-p 'if-possible) t)))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
            ((symbol-function 'elpaish--get-signing-key) (lambda () "ABCD1234")))
    (should-not (elpaish--resolve-signing-p 'auto)))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/gpg"))
            ((symbol-function 'elpaish--get-signing-key) (lambda () nil)))
    (should-not (elpaish--resolve-signing-p 'auto))))

(ert-deftest elpaish-test-server-sync-build-and-serve ()
  "Test `elpaish-test-server' builds packages and starts `web-server'."
  (let* ((tmp (make-temp-file "elpaish-test-srv-" t))
         (repo-dir (expand-file-name "repo" tmp))
         (out-dir (expand-file-name "public" tmp))
         (pkg-file (expand-file-name "packages.el" tmp))
         (elpaish-registry (make-hash-table :test 'equal))
         (elpaish-server-process nil)
         (callback-res nil))
    (unwind-protect
        (progn
          (make-directory repo-dir t)
          (with-temp-file (expand-file-name "srvpkg.el" repo-dir)
            (insert ";;; srvpkg.el --- Test server package -*- lexical-binding: t; -*-\n"
                    ";; Version: 0.1.0\n"
                    "(provide 'srvpkg)\n"
                    ";;; srvpkg.el ends here\n"))
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init" "-b" "main")
            (call-process "git" nil nil nil "config" "user.email" "test@example.com")
            (call-process "git" nil nil nil "config" "user.name" "Test User")
            (call-process "git" nil nil nil "add" "srvpkg.el")
            (call-process "git" nil nil nil "commit" "-m" "Initial commit"))
          (with-temp-file pkg-file
            (insert (format "(elpaish-register-package 'srvpkg %S :branch \"main\" :preflight-skip t)\n"
                            repo-dir)))
          (let ((res (elpaish-test-server
                      :port 0
                      :output-dir out-dir
                      :mode 'snapshot
                      :sign nil
                      :sync t
                      :packages-files (list pkg-file)
                      :callback (lambda (r) (setq callback-res r)))))
            (should (elpaish-server-running-p))
            (should (equal res callback-res))
            (should (eq (plist-get res :status) 'ok))
            (should (= (plist-get res :packages) 1))
            (should (string-prefix-p "http://127.0.0.1:" (elpaish-test-server-url 'snapshot)))
            (should (string-suffix-p "/snapshot/" (elpaish-test-server-url 'snapshot)))
            (should (file-exists-p (expand-file-name "snapshot/archive-contents" out-dir)))))
      (elpaish-stop-server)
      (when (file-directory-p tmp)
        (delete-directory tmp t)))))

(ert-deftest elpaish-test-server-sprite-delegation ()
  "Test `elpaish-test-server' delegates rebuild to `sprite-fleet' when available."
  (let* ((tmp (make-temp-file "elpaish-test-sprite-" t))
         (out-dir (expand-file-name "public" tmp))
         (elpaish-server-process nil)
         (captured-form nil)
         (captured-args nil)
         (fake-future (list :fake-future t))
         (callback-res nil))
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional _filename _noerror)
                     (memq feature '(sprite sprite-fleet sprite-future elpaish-recipes))))
                  ((symbol-function 'sprite-fleet-mapcar)
                   (lambda (form items &rest args)
                     (setq captured-form form
                           captured-args (list :items items :args args))
                     fake-future))
                  ((symbol-function 'sprite-future-then)
                   (lambda (future cb)
                     (should (eq future fake-future))
                     (funcall cb (list (list :status 'ok :packages 3)))))
                  ((symbol-function 'elpaish--resolve-signing-p) (lambda (_) nil)))
          (let ((res (elpaish-test-server
                      :port 0
                      :output-dir out-dir
                      :mode 'snapshot
                      :sign 'auto
                      :sprite t
                      :callback (lambda (r) (setq callback-res r)))))
            (should (eq res fake-future))
            (should (elpaish-server-running-p))
            (should (consp captured-form))
            (should (equal (plist-get captured-args :items) '(t)))
            (should (eq (plist-get (plist-get captured-args :args) :async) t))
            (should (= (plist-get (plist-get captured-args :args) :fleet-size) 1))
            (should (equal callback-res '(:status ok :packages 3)))))
      (elpaish-stop-server)
      (when (file-directory-p tmp)
        (delete-directory tmp t)))))

(ert-deftest elpaish-test-async-build-package-in-sprite-delegation ()
  "Test `elpaish-async-build-package-in-sprite' delegates to `sprite-fleet-mapcar'."
  (let* ((captured-form nil)
         (captured-args nil)
         (fake-future (list :fake-fleet-future t))
         (settled-res nil)
         (dummy-result (list :status :ok :pkg 'pkg-one :artifact "/tmp/pkg-one-1.0.tar")))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (memq feature '(sprite sprite-fleet sprite-future map elpaish elpaish-recipes elpaish-async))))
              ((symbol-function 'sprite-fleet-mapcar)
               (lambda (form items &rest args)
                 (setq captured-form form
                       captured-args (list :items items :args args))
                 fake-future))
              ((symbol-function 'sprite-future-then)
               (lambda (future on-resolve &optional _on-reject)
                 (should (eq future fake-future))
                 (setq settled-res (funcall on-resolve (list dummy-result)))
                 (list :chained-future settled-res))))
      (let ((res (elpaish-async-build-package-in-sprite 'pkg-one :stream 'snapshot :staging-dir "/tmp/staging")))
        (should (consp res))
        (should (consp captured-form))
        (should (equal (plist-get captured-args :items) '(t)))
        (should (eq (plist-get (plist-get captured-args :args) :async) t))
        (should (= (plist-get (plist-get captured-args :args) :fleet-size) 1))
        (should (equal settled-res dummy-result))))))

(ert-deftest elpaish-test-async-build-package-in-sprite-error-handling ()
  "Test `elpaish-async-build-package-in-sprite' handles preflight and sprite errors."
  (let* ((fake-future (list :fake-fleet-future t))
         (settled-res nil)
         (worker-error (list :error '(error "Worker timeout") :target 'fleet)))
    ;; Test worker sprite crash/timeout translation
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (memq feature '(sprite sprite-fleet sprite-future map elpaish elpaish-recipes elpaish-async))))
              ((symbol-function 'sprite-fleet-mapcar)
               (lambda (&rest _) fake-future))
              ((symbol-function 'sprite-future-then)
               (lambda (future on-resolve &optional _on-reject)
                 (setq settled-res (funcall on-resolve (list worker-error)))
                 (list :chained-future settled-res))))
      (elpaish-async-build-package-in-sprite 'pkg-err)
      (should (eq (plist-get settled-res :status) :failed))
      (should (eq (plist-get settled-res :stage) :sprite-dispatch))
      (should (equal (plist-get settled-res :pkg) 'pkg-err))
      (should (string-search "Worker timeout" (plist-get settled-res :error))))))

(defun elpaish-test--reject-fleet (form-value fallback &optional choice)
  "Run a build whose fleet rejects; return (RESULT PROMPTED FALLBACK-AFTER).
FORM-VALUE is the build form, FALLBACK the policy and CHOICE the key pressed."
  (let ((fleet-future (list :fake-fleet-future t))
        (elpaish-async-local-fallback fallback)
        (noninteractive (null choice))
        (prompted nil)
        (settled nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (memq feature '(sprite sprite-fleet sprite-future map elpaish elpaish-recipes elpaish-async))))
              ((symbol-function 'elpaish-async--build-package-form)
               (lambda (&rest _) form-value))
              ((symbol-function 'sprite-fleet-mapcar) (lambda (&rest _) fleet-future))
              ((symbol-function 'sprite-future-then)
               (lambda (_future _on-resolve &optional on-reject)
                 (setq settled (funcall on-reject "No available sprites"))
                 (list :chained settled)))
              ((symbol-function 'read-multiple-choice)
               (lambda (&rest _) (setq prompted t) (list choice)))
              ((symbol-function 'message) #'ignore))
      (elpaish-async-build-package-in-sprite 'pkg-one)
      (list settled prompted elpaish-async-local-fallback))))

(ert-deftest elpaish-test-async-fallback-asks-and-builds-on-yes ()
  (let ((res (elpaish-test--reject-fleet '(quote (:status :ok :pkg pkg-one)) 'ask ?y)))
    (should (equal (car res) '(:status :ok :pkg pkg-one)))
    (should (nth 1 res))
    (should (eq (nth 2 res) 'ask))))

(ert-deftest elpaish-test-async-fallback-declined-fails-without-building ()
  (let ((res (elpaish-test--reject-fleet '(error "must not run") 'ask ?n)))
    (should (eq (plist-get (car res) :status) :failed))
    (should (eq (plist-get (car res) :stage) :sprite-dispatch))
    (should (string-search "not permitted" (plist-get (car res) :error)))
    (should (nth 1 res))))

(ert-deftest elpaish-test-async-fallback-always-choice-sticks ()
  (let ((res (elpaish-test--reject-fleet '(quote (:status :ok :pkg pkg-one)) 'ask ?a)))
    (should (equal (car res) '(:status :ok :pkg pkg-one)))
    (should (eq (nth 2 res) 'always))))

(ert-deftest elpaish-test-async-fallback-policy-skips-prompt ()
  (let ((always (elpaish-test--reject-fleet '(quote (:status :ok :pkg pkg-one)) 'always ?n))
        (never (elpaish-test--reject-fleet '(error "must not run") 'never ?y)))
    (should (equal (car always) '(:status :ok :pkg pkg-one)))
    (should-not (nth 1 always))
    (should (eq (plist-get (car never) :status) :failed))
    (should-not (nth 1 never))))

(ert-deftest elpaish-test-async-fallback-noninteractive-ask-fails ()
  "Without a user to consent, `ask' fails the build instead of building."
  (let ((res (elpaish-test--reject-fleet '(error "must not run") 'ask nil)))
    (should (eq (plist-get (car res) :status) :failed))
    (should (eq (plist-get (car res) :stage) :sprite-dispatch))
    (should-not (nth 1 res))))

(ert-deftest elpaish-test-async-local-fallback-reports-build-errors ()
  "A failing local fallback build yields a failed result, not a signal."
  (let ((res (elpaish-test--reject-fleet '(error "compile exploded") 'always)))
    (should (eq (plist-get (car res) :status) :failed))
    (should (eq (plist-get (car res) :stage) :local-fallback))
    (should (equal (plist-get (car res) :pkg) 'pkg-one))
    (should (string-search "compile exploded" (plist-get (car res) :error)))))

(ert-deftest elpaish-test-rejection-message-never-signals ()
  "Sprite rejections come as strings, error objects, lists, or other values."
  (should (equal (elpaish-install--rejection-message "No available sprites")
                 "No available sprites"))
  (should (string-search "boom" (elpaish-install--rejection-message '(error "boom"))))
  (should (string-search "worker" (elpaish-install--rejection-message '(:worker-failed "w" "worker down"))))
  (should (stringp (elpaish-install--rejection-message nil)))
  (should (stringp (elpaish-install--rejection-message 42))))

(ert-deftest elpaish-test-async-build-package-form-execution ()
  "Test execution of the form generated by `elpaish-async--build-package-form'."
  (elpaish-test-with-temp-env
   (let* ((repo (expand-file-name "repo" temp-dir))
          (staging-dir (expand-file-name "staging" temp-dir))
          (elpaish-registry (make-hash-table :test 'equal)))
     (elpaish-test-create-dummy-pkg repo "pkg-form" "1.0" "Form Test Package")
     (elpaish-register-package 'pkg-form repo :preflight-skip t)
     (let* ((form (elpaish-async--build-package-form 'pkg-form 'snapshot staging-dir load-path nil))
            (res (eval form t)))
       (should (eq (plist-get res :status) :ok))
       (should (eq (plist-get res :pkg) 'pkg-form))
       (should (file-exists-p (plist-get res :artifact)))))))

(ert-deftest elpaish-test-server-upgrade-overrides-archives ()
  "Test `elpaish-test-server-upgrade' prepends local test server archive."
  (let ((elpaish-server-process nil))
    (should-error (elpaish-test-server-upgrade 'pkg-a) :type 'user-error))
  (let* ((elpaish-server-process nil)
         (package-archives '(("melpa" . "https://melpa.org/packages/")))
         (package-archive-priorities '(("melpa" . 100)))
         (seen-archives nil)
         (seen-priorities nil)
         (seen-args nil))
    (cl-letf (((symbol-function 'elpaish-server-running-p) (lambda (&optional _) t))
              ((symbol-function 'elpaish--server-port) (lambda (&optional _) 19001))
              ((symbol-function 'elpaish-install-upgrade-packages)
               (lambda (&rest args)
                 (setq seen-archives package-archives
                       seen-priorities package-archive-priorities
                       seen-args args))))
      (elpaish-test-server-upgrade 'pkg-a :sync t)
      (should (equal (car seen-archives)
                     '("elpaish-local" . "http://127.0.0.1:19001/snapshot/")))
      (should (equal (cadr seen-archives)
                     '("melpa" . "https://melpa.org/packages/")))
      (should (= (cdr (assoc "elpaish-local" seen-priorities)) 1000))
      (should (equal seen-args '(pkg-a :sync t))))))

(ert-deftest elpaish-test-upgrade-run-preserves-let-bound-archives ()
  "Test async `elpaish-install--upgrade-run' preserves `let'-bound archives."
  (let ((seen-archives nil)
        (elpaish-install--active-cancel-handle nil)
        (elpaish-install-bootstrap-finished-functions nil)
        (package-archives '(("global" . "https://example.com/packages/")))
        (done nil))
    (cl-letf (((symbol-function 'package-initialize) #'ignore)
              ((symbol-function 'package-refresh-contents)
               (lambda () (setq seen-archives package-archives)))
              ((symbol-function 'elpaish-install--installed-version) (lambda (_) "0.1.0"))
              ((symbol-function 'elpaish-install--process-package)
               (lambda (pkg _act _refresh) (list :package pkg :status :up-to-date)))
              ((symbol-function 'elpaish-install--report-summary) #'ignore)
              ((symbol-function 'display-buffer) #'ignore))
      (let ((package-archives '(("elpaish-local" . "http://127.0.0.1:9999/snapshot/"))))
        (elpaish-install--upgrade-run
         '(test-pkg) t nil 0.001
         (lambda (_res) (setq done t))
         'upgrade))
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should done)
      (should (equal seen-archives
                     '(("elpaish-local" . "http://127.0.0.1:9999/snapshot/")))))))

(ert-deftest elpaish-test-sync-package-load-paths-prunes-stale-and-meta-dirs ()
  "Test `elpaish-install-sync-package-load-paths' keeps only newest package versions and skips archives/gnupg."
  (let* ((tmp (make-temp-file "elpaish-test-sync-lp-" t))
         (user-emacs-directory tmp)
         (elpa-dir (expand-file-name "elpa" tmp))
         (archives-dir (expand-file-name "archives" elpa-dir))
         (gnupg-dir (expand-file-name "gnupg" elpa-dir))
         (old-pkg (expand-file-name "foo-1.0.0" elpa-dir))
         (new-pkg (expand-file-name "foo-2.0.0" elpa-dir))
         (load-path (list archives-dir gnupg-dir old-pkg)))
    (unwind-protect
        (progn
          (make-directory archives-dir t)
          (make-directory gnupg-dir t)
          (make-directory old-pkg t)
          (make-directory new-pkg t)
          (elpaish-install-sync-package-load-paths)
          (should (member new-pkg load-path))
          (should-not (member old-pkg load-path))
          (should-not (member archives-dir load-path))
          (should-not (member gnupg-dir load-path)))
      (when (file-directory-p tmp)
        (delete-directory tmp t)))))

(ert-deftest elpaish-test-install-local-package-non-interactive ()
  "Test `elpaish-install-local-package' installs single and multiple packages."
  (let ((installed nil)
        (noninteractive t)
        (elpaish-install--active-cancel-handle nil))
    (cl-letf (((symbol-function 'elpaish-install-local-packages)
               (lambda (targets &rest _args)
                 (setq installed (append installed targets))
                 (mapcar (lambda (p) (package-desc-create :name (if (symbolp p) p (intern p))))
                         targets))))
      ;; Single symbol
      (let ((res (elpaish-install-local-package 'foo)))
        (should (equal installed '(foo)))
        (should (eq (package-desc-name res) 'foo)))

      ;; String or list
      (setq installed nil)
      (let ((res (elpaish-install-local-package '("bar" baz))))
        (should (equal installed '("bar" baz)))
        (should (equal (mapcar #'package-desc-name res) '(bar baz)))))))

(ert-deftest elpaish-test-install-local-package-interactive ()
  "Test `elpaish-install-local-package' interactive ACR selection."
  (elpaish-test-with-temp-env
   (let ((installed nil))
     (puthash "alpha" (elpaish-recipe-create :name 'alpha :summary "Alpha pkg") elpaish-registry)
     (puthash "beta" (elpaish-recipe-create :name 'beta :summary "Beta pkg") elpaish-registry)
     (cl-letf (((symbol-function 'elpaish-install-local-packages)
                (lambda (targets &rest _args)
                  (setq installed (append installed targets))
                  (mapcar (lambda (p) (package-desc-create :name (if (symbolp p) p (intern p))))
                          targets)))
               ((symbol-function 'annotated-completing-read)
                (lambda (_table &rest plist)
                  (if (plist-get plist :multiple)
                      '("alpha" "beta")
                    "alpha"))))
       ;; Single interactive selection
       (let ((current-prefix-arg nil))
         (call-interactively #'elpaish-install-local-package)
         (should (equal installed '(alpha))))

       ;; Multiple interactive selection with prefix arg
       (setq installed nil)
       (let ((current-prefix-arg '(4)))
         (call-interactively #'elpaish-install-local-package)
         (should (equal installed '(alpha beta))))))))

(ert-deftest elpaish-test-build-all-non-blocking-interactive ()
  "Test `elpaish-build-all' runs asynchronously in interactive mode and with :async t."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "async-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "async-pkg" "1.0.0" "Async Test")
     (elpaish-register-package 'async-pkg pkg-dir :preflight-skip t)
     ;; Interactive call returns a running thread
     (let* ((noninteractive nil)
            (thread (call-interactively #'elpaish-build-all)))
       (should (threadp thread))
       (thread-join thread)
       (should (file-exists-p (expand-file-name "snapshot/archive-contents" elpaish-output-dir))))
     ;; Alias elpaish-build-repository with :async t returns thread
     (let ((thread (elpaish-build-repository :async t)))
       (should (threadp thread))
       (thread-join thread))
     ;; Synchronous build with :async nil returns target-root and not a thread
     (let ((res (elpaish-build-all :async nil)))
       (should-not (threadp res))
       (should (stringp res))))))

(ert-deftest elpaish-test-upgrade-triage-strikes-through-and-moves-to-end ()
  "Test upgrade first marks up-to-date packages as current and moves upgrades to end."
  (elpaish-test-with-temp-env
   (let* ((desc-w1-inst (package-desc-create :name 'pkg-work-1 :version '(1 0)))
          (desc-w1-arch (package-desc-create :name 'pkg-work-1 :version '(2 0)))
          (desc-u1-inst (package-desc-create :name 'pkg-utd-1 :version '(1 5)))
          (desc-u1-arch (package-desc-create :name 'pkg-utd-1 :version '(1 5)))
          (desc-w2-inst (package-desc-create :name 'pkg-work-2 :version '(1 0)))
          (desc-w2-arch (package-desc-create :name 'pkg-work-2 :version '(3 0)))
          (desc-u2-inst (package-desc-create :name 'pkg-utd-2 :version '(2 0)))
          (desc-u2-arch (package-desc-create :name 'pkg-utd-2 :version '(2 0)))
          (package-alist `((pkg-work-1 ,desc-w1-inst)
                           (pkg-utd-1 ,desc-u1-inst)
                           (pkg-work-2 ,desc-w2-inst)
                           (pkg-utd-2 ,desc-u2-inst)))
          (package-archive-contents `((pkg-work-1 ,desc-w1-arch)
                                      (pkg-utd-1 ,desc-u1-arch)
                                      (pkg-work-2 ,desc-w2-arch)
                                      (pkg-utd-2 ,desc-u2-arch)))
          (elpaish-install-packages-list '(pkg-work-1 pkg-utd-1 pkg-work-2 pkg-utd-2))
          (elpaish-install-upgrade-packages-list '(pkg-work-1 pkg-utd-1 pkg-work-2 pkg-utd-2))
          (elpaish-install--active-cancel-handle nil)
          (processed nil)
          (queue nil))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'package-installed-p) (lambda (_) t))
               ((symbol-function 'package-refresh-contents) #'ignore)
               ((symbol-function 'run-at-time)
                (lambda (_delay repeat fn &rest _)
                  (unless repeat
                    (setq queue (append queue (list fn))))))
               ((symbol-function 'elpaish-install--process-package)
                (lambda (pkg &rest _)
                  (push pkg processed)
                  (list :pkg pkg :status :upgraded :from "1.0" :to "2.0"))))
       (elpaish-install-upgrade-packages :refresh t)
       ;; Step 1: refresh and initial triage pass
       (funcall (pop queue))
       ;; No work items should have been processed yet during triage
       (should-not processed)
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (let ((text (buffer-string))
               (pos-u1 nil)
               (pos-u2 nil)
               (pos-w1 nil)
               (pos-w2 nil))
           (should (string-match-p "2 current, 2 work items: 0 done, 2 pending" text))
           (save-excursion
             (goto-char (point-min))
             (setq pos-u1 (search-forward "pkg-utd-1: 1.5" nil t))
             (goto-char (point-min))
             (setq pos-u2 (search-forward "pkg-utd-2: 2.0" nil t))
             (goto-char (point-min))
             (setq pos-w1 (search-forward "pkg-work-1: 1.0" nil t))
             (goto-char (point-min))
             (setq pos-w2 (search-forward "pkg-work-2: 1.0" nil t)))
           ;; Up-to-date packages appear first; work items are moved to the end
           (should (and pos-u1 pos-u2 pos-w1 pos-w2))
           (should (< pos-u1 pos-w1))
           (should (< pos-u2 pos-w1))
           (should (< pos-w1 pos-w2))
           ;; Up-to-date package rows are grayed out without strike-through
           (should (get-text-property (1- pos-u1) 'elpaish-up-to-date))
           (should (get-text-property (1- pos-u2) 'elpaish-up-to-date))
           (should-not (get-text-property (1- pos-w1) 'elpaish-up-to-date))
           (should (memq (face-attribute 'elpaish-install-up-to-date-face :strike-through nil t) '(nil unspecified)))))
       ;; Drain remaining steps: only the 2 upgrade work items are processed
       (while queue (funcall (pop queue)))
       (should (equal (nreverse processed) '(pkg-work-1 pkg-work-2)))))))

(ert-deftest elpaish-test-upgrade-timing-statistics-wall-actual-wait ()
  "Test timing statistics track wall clock, actual work, and wait durations."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list '(pkg-time-1))
          (elpaish-install-upgrade-packages-list '(pkg-time-1))
          (elpaish-install--active-cancel-handle nil)
          (package-archive-contents '((dummy)))
          (summary-msg nil))
     (let ((hook-fn (lambda (ev) (setq summary-msg (plist-get ev :msg)))))
       (add-hook 'logx-event-hook hook-fn)
       (unwind-protect
           (cl-letf (((symbol-function 'display-buffer) #'ignore)
                     ((symbol-function 'elpaish-install--process-package)
                      (lambda (pkg &rest _)
                        (list :pkg pkg :status :upgraded :from "1.0" :to "2.0"))))
             (let ((res (elpaish-install-upgrade-packages 'pkg-time-1 :sync t :refresh nil)))
               (should (= (length res) 1))
               (should (numberp (plist-get (car res) :duration)))
               (with-current-buffer "*ELPAish Upgrade Progress*"
                 (let ((ctx elpaish-install--progress-context)
                       (text (buffer-string)))
                   (should (numberp (plist-get ctx :wall-time)))
                   (should (numberp (plist-get ctx :actual-time)))
                   (should (numberp (plist-get ctx :wait-time)))
                   (should (string-match-p "Duration: [0-9.]+s wall ([0-9.]+s actual, [0-9.]+s waiting)" text))
                   (should (string-match-p "time: [0-9.]+s" text))
                   (should-not header-line-format)))
               (should (string-match-p "\\[wall: [0-9.]+s, actual: [0-9.]+s, wait: [0-9.]+s\\]" summary-msg))))
         (remove-hook 'logx-event-hook hook-fn))))))

(ert-deftest elpaish-test-upgrade-previous-generation-and-local-update-times ()
  "Test tracking of previous generation times and local update timestamps."
  (elpaish-test-with-temp-env
   ;; 1. Version generation timestamp decoding
   (should (equal (elpaish-install--version-generation-time "20261118.134022")
                  "2026-11-18 13:40"))
   (should (equal (elpaish-install--version-generation-time "20261115.0930")
                  "2026-11-15 09:30"))
   (should (equal (elpaish-install--version-generation-time '(20261118 83045))
                  "2026-11-18 08:30"))
   (should-not (elpaish-install--version-generation-time "1.2.3"))
   ;; 2. Full work-item timestamp tracking from installed package directory
   (let* ((package-user-dir (expand-file-name "elpa" temp-dir))
          (inst-dir (expand-file-name "pkg-ts-20261115.142000" package-user-dir))
          (autoloads (expand-file-name "pkg-ts-autoloads.el" inst-dir))
          (inst-desc (package-desc-create :name 'pkg-ts
                                          :version '(20261115 142000)
                                          :dir inst-dir))
          (arch-desc (package-desc-create :name 'pkg-ts
                                          :version '(20261120 100000)))
          (package-alist `((pkg-ts ,inst-desc)))
          (package-archive-contents `((pkg-ts ,arch-desc)))
          (elpaish-install-packages-list '(pkg-ts))
          (elpaish-install-upgrade-packages-list '(pkg-ts))
          (elpaish-install--active-cancel-handle nil))
     (make-directory inst-dir t)
     (with-temp-file autoloads (insert ";; autoloads\n"))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'package-installed-p) (lambda (_) t))
               ((symbol-function 'elpaish-install--process-package)
                (lambda (pkg &rest _)
                  (list :pkg pkg :status :upgraded
                        :from "20261115.142000" :to "20261120.100000"))))
       (let* ((res (elpaish-install-upgrade-packages 'pkg-ts :sync t :refresh nil))
              (item (car res)))
         (should (equal (plist-get item :prev-version) "20261115.142000"))
         (should (equal (plist-get item :prev-generated) "2026-11-15 14:20"))
         (should (stringp (plist-get item :prev-local-update)))
         (with-current-buffer "*ELPAish Upgrade Progress*"
           (let ((text (buffer-string)))
             (should (string-match-p "prev: 20261115\\.142000" text))
             (should (string-match-p "to: 20261120\\.100000" text)))))))))

(ert-deftest elpaish-test-upgrade-controls-pause-resume-slow-down ()
  "Test pause, resume, slow-down, speed-up, and set-delay controls in progress mode."
  (elpaish-test-with-temp-env
   (let ((elpaish-install-packages-list '(pkg-c1 pkg-c2))
         (elpaish-install-upgrade-packages-list '(pkg-c1 pkg-c2))
         (elpaish-install--active-cancel-handle nil)
         (package-archive-contents '((dummy)))
         (queue nil))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'run-at-time)
                (lambda (_delay repeat fn &rest _)
                  (unless repeat
                    (setq queue (append queue (list fn))))
                  'fake-timer))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'elpaish-install--process-package)
                (lambda (pkg &rest _)
                  (list :pkg pkg :status :upgraded :to "2.0"))))
       (elpaish-install-upgrade-packages :refresh nil :delay 0.01)
       (with-current-buffer "*ELPAish Upgrade Progress*"
         ;; Test slow-down, speed-up, and set-delay controls
         (elpaish-install-progress-slow-down)
         (should (>= (plist-get elpaish-install--progress-context :delay) 0.1))
         (elpaish-install-progress-set-delay 0.5)
         (should (equal (plist-get elpaish-install--progress-context :delay) 0.5))
         (should (string-match-p "delay 0.5s" (buffer-string)))
         (elpaish-install-progress-speed-up)
         (should (equal (plist-get elpaish-install--progress-context :delay) 0.25))
         ;; Test pause and toggle-pause
         (elpaish-install-progress-pause)
         (should (plist-get elpaish-install--progress-context :paused))
         (should (string-match-p "\\[Paused\\]" (buffer-string)))
         (elpaish-install-progress-toggle-pause)
         (should-not (plist-get elpaish-install--progress-context :paused)))
       (while queue (funcall (pop queue)))
       (should-not elpaish-install--active-cancel-handle)))))

(ert-deftest elpaish-test-upgrade-intermediate-confirmation ()
  "Test intermediate confirmation option (default off) and decline-to-pause flow."
  (elpaish-test-with-temp-env
   (should-not elpaish-install-confirm-intermediate)
   (let ((elpaish-install-packages-list '(pkg-conf-1 pkg-conf-2))
         (elpaish-install-upgrade-packages-list '(pkg-conf-1 pkg-conf-2))
         (elpaish-install--active-cancel-handle nil)
         (package-archive-contents '((dummy)))
         (prompts nil)
         (answers '(t nil t))
         (upgraded nil))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'y-or-n-p)
                (lambda (prompt)
                  (push prompt prompts)
                  (pop answers)))
               ((symbol-function 'elpaish-install--process-package)
                (lambda (pkg &rest _)
                  (push pkg upgraded)
                  (list :pkg pkg :status :upgraded :to "2.0"))))
       ;; Run with :confirm t; 1st work item confirmed (t), 2nd declined (nil -> pauses)
       (elpaish-install-upgrade-packages 'pkg-conf-1 'pkg-conf-2
                                         :sync t :refresh nil :confirm t)
       (should (= (length prompts) 2))
       (should (equal upgraded '(pkg-conf-1)))
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (should (plist-get elpaish-install--progress-context :paused))
         (should (string-match-p "\\[Paused\\]" (buffer-string)))
         ;; Toggle confirm off and continue to finish pkg-conf-2
         (elpaish-install-progress-toggle-confirm)
         (should-not (plist-get elpaish-install--progress-context :confirm))
         (elpaish-install-progress-continue)
         (while (plist-get elpaish-install--progress-context :running)
           (elpaish-install-progress-continue)))
       (should (equal (nreverse upgraded) '(pkg-conf-1 pkg-conf-2)))))))

(ert-deftest elpaish-test-install-local-packages-uses-progress-interface ()
  "Test `elpaish-install-local-packages' uses the `*ELPAish Upgrade Progress*' interface."
  (elpaish-test-with-temp-env
   (let* ((repo1 (expand-file-name "repo-ui-1" temp-dir))
          (package-user-dir (expand-file-name "installed" temp-dir))
          (package-alist nil)
          (package-selected-packages nil)
          (package-quickstart nil)
          (package-quickstart-file (expand-file-name "quickstart.el" temp-dir))
          (load-path (copy-sequence load-path)))
     (elpaish-test-create-dummy-pkg repo1 "pkg-ui-one" "1.2" "Package UI One")
     (elpaish-register-package 'pkg-ui-one repo1)
     (let ((elpaish-install-packages-list '(pkg-ui-one)))
       (cl-letf (((symbol-function 'display-buffer) #'ignore)
                 ((symbol-function 'package--save-selected-packages) #'ignore)
                 ((symbol-function 'native-compile-async) #'ignore))
         (let ((descs (elpaish-install-local-packages 'installed)))
           (should (= (length descs) 1))
           (should (eq (package-desc-name (car descs)) 'pkg-ui-one))
           (with-current-buffer "*ELPAish Upgrade Progress*"
             (let ((text (buffer-string)))
               (should (string-match-p "ELPAish package repository installation \\[Completed\\]" text))
               (should (string-match-p "\\[x\\] pkg-ui-one: " text))
               (should (string-match-p "Duration: [0-9.]+s wall" text))))))))))

(ert-deftest elpaish-test-progress-archive-and-new-version ()
  "Show the active archive and link the newly installed version."
  (elpaish-test-with-temp-env
   (let ((package-user-dir (expand-file-name "elpa" temp-dir))
         (package-archive-contents '((dummy)))
         (package-archives '(("example" . "https://example.invalid/")))
         (seen nil))
     (make-directory (expand-file-name "good-3.0" package-user-dir) t)
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'package-refresh-contents)
                (lambda (&optional _)
                  (package--download-one-archive (car package-archives) "archive-contents")))
               ((symbol-function 'package--download-one-archive)
                (lambda (&rest _)
                  (with-current-buffer "*ELPAish Upgrade Progress*"
                    (setq seen (string-match-p "Downloading archive: example" (buffer-string))))))
               ((symbol-function 'elpaish-install--process-package)
                (lambda (&rest _) '(:pkg good :status :upgraded :to "3.0"))))
       (elpaish-install--upgrade-run '(good) t t 0 nil 'upgrade)
       (should seen)
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (goto-char (point-min))
         (should (search-forward "good: 3.0" nil t))
         (should (button-at (1- (point))))
         (should (eq (lookup-key elpaish-install-progress-mode-map (kbd "C-g"))
                     'elpaish-install-progress-soft-quit)))))))

(ert-deftest elpaish-test-upgrade-run-async-sprite-building ()
  "Test async repository building in progress runner with sprite future transitions."
  (elpaish-test-with-temp-env
   (let* ((repo (expand-file-name "repo-async" temp-dir))
          (package-user-dir (expand-file-name "installed" temp-dir))
          (package-alist nil)
          (package-selected-packages nil)
          (package-quickstart nil)
          (queue nil)
          (captured-states nil)
          (fake-future (list :state :pending))
          (callback-results nil)
          (elpaish-install--active-cancel-handle nil)
          (orig-render (symbol-function 'elpaish-install--upgrade-progress))
          (dummy-artifact (expand-file-name "pkg-async-1.0.tar" temp-dir)))
     (elpaish-test-create-dummy-pkg repo "pkg-async" "1.0" "Async Package")
     (elpaish-register-package 'pkg-async repo)
     (with-temp-file dummy-artifact (insert "dummy artifact"))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'run-at-time)
                (lambda (_delay repeat fn &rest _)
                  (unless repeat (setq queue (append queue (list fn))))
                  'fake-timer))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'sprite-future-p) (lambda (f) (and (consp f) (plist-get f :state))))
               ((symbol-function 'elpaish-async-build-package-in-sprite)
                (lambda (&rest _) fake-future))
               ((symbol-function 'sprite-future-then)
                (lambda (future on-resolve &optional on-reject)
                  (setf (plist-get (cdr future) :on-resolve) on-resolve)
                  (setf (plist-get (cdr future) :on-reject) on-reject)
                  future))
               ((symbol-function 'package-install-file)
                (lambda (_file)
                  (package-desc-create :name 'pkg-async
                                       :version '(1 0)
                                       :dir temp-dir
                                       :summary "Async Package"
                                       :kind 'single
                                       :archive "elpaish")))
               ((symbol-function 'elpaish-install--upgrade-progress)
                (lambda (buf entries ref qs global cur act stats &rest rest)
                  (push global captured-states)
                  (apply orig-render buf entries ref qs global cur act stats rest))))
       (elpaish-install--upgrade-run
        '(pkg-async) nil nil 0
        (lambda (res) (setq callback-results res))
        'repo-install)
       ;; 1. Initial queue has the first step scheduled
       (should queue)
       (funcall (pop queue)) ;; Run first step (refresh stage transitions to packages stage)
       (should queue)
       (funcall (pop queue)) ;; Run package step: starts building pkg-async in sprite
       ;; Global state was "Compiling in sprite"
       (should (member "Compiling in sprite" captured-states))
       ;; Future is still pending, so queue should be empty (waiting for future)
       (should-not queue)
       ;; 2. Settle the future with :ok status
       (let ((resolve-fn (plist-get (cdr fake-future) :on-resolve)))
         (should (functionp resolve-fn))
         (funcall resolve-fn (list :status :ok :artifact dummy-artifact :pkg 'pkg-async)))
       ;; Settle triggered "Installing in host" and scheduled next step
       (should (member "Installing in host" captured-states))
       (should queue)
       ;; 3. Run remaining steps (quickstart / done)
       (while queue (funcall (pop queue)))
       (should callback-results)
       (should (eq (plist-get (car callback-results) :status) :installed))
       (should (eq (plist-get (car callback-results) :pkg) 'pkg-async))
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (let ((text (buffer-string)))
           (should (string-match-p "\\[x\\] pkg-async: 1.0" text))
           (should (string-match-p "\\[Completed\\]" text))))))))

(ert-deftest elpaish-test-upgrade-run-async-sprite-failure-isolation ()
  "Test sprite build failures report [!] with diagnostic and continue without halting."
  (elpaish-test-with-temp-env
   (let* ((repo1 (expand-file-name "repo-fail" temp-dir))
          (repo2 (expand-file-name "repo-succ" temp-dir))
          (package-user-dir (expand-file-name "installed" temp-dir))
          (package-alist nil)
          (package-selected-packages nil)
          (package-quickstart nil)
          (queue nil)
          (callback-results nil)
          (elpaish-install--active-cancel-handle nil)
          (active-future nil)
          (dummy-artifact (expand-file-name "pkg-succ-1.0.tar" temp-dir)))
     (elpaish-test-create-dummy-pkg repo1 "pkg-fail" "1.0" "Failing Package")
     (elpaish-test-create-dummy-pkg repo2 "pkg-succ" "1.0" "Succeeding Package")
     (elpaish-register-package 'pkg-fail repo1)
     (elpaish-register-package 'pkg-succ repo2)
     (with-temp-file dummy-artifact (insert "dummy artifact"))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'run-at-time)
                (lambda (_delay repeat fn &rest _)
                  (unless repeat (setq queue (append queue (list fn))))
                  'fake-timer))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'sprite-future-p) (lambda (f) (and (consp f) (plist-get f :state))))
               ((symbol-function 'elpaish-async-build-package-in-sprite)
                (lambda (recipe &rest _)
                  (setq active-future (list :state :pending :pkg (if (symbolp recipe) recipe (elpaish-recipe-name recipe))))
                  active-future))
               ((symbol-function 'sprite-future-then)
                (lambda (future on-resolve &optional on-reject)
                  (setf (plist-get (cdr future) :on-resolve) on-resolve)
                  (setf (plist-get (cdr future) :on-reject) on-reject)
                  future))
               ((symbol-function 'package-install-file)
                (lambda (_file)
                  (package-desc-create :name 'pkg-succ
                                       :version '(1 0)
                                       :dir temp-dir
                                       :summary "Succeeding Package"
                                       :kind 'single
                                       :archive "elpaish"))))
       (elpaish-install--upgrade-run
        '(pkg-fail pkg-succ) nil nil 0
        (lambda (res) (setq callback-results res))
        'repo-install)
       ;; Advance past refresh
       (funcall (pop queue))
       ;; Advance into pkg-fail
       (funcall (pop queue))
       (should (equal (format "%s" (plist-get active-future :pkg)) "pkg-fail"))
       ;; Fail pkg-fail build with diagnostic
       (let ((resolve-fn (plist-get (cdr active-future) :on-resolve)))
         (funcall resolve-fn (list :status :failed :stage :build :error "Syntax error in pkg-fail.el" :pkg 'pkg-fail)))
       ;; Next step scheduled for pkg-succ
       (should queue)
       (funcall (pop queue))
       (should (equal (format "%s" (plist-get active-future :pkg)) "pkg-succ"))
       ;; Resolve pkg-succ build
       (let ((resolve-fn (plist-get (cdr active-future) :on-resolve)))
         (funcall resolve-fn (list :status :ok :artifact dummy-artifact :pkg 'pkg-succ)))
       ;; Finish remaining steps
       (while queue (funcall (pop queue)))
       (should (= (length callback-results) 2))
       (let ((fail-res (seq-find (lambda (r) (eq (plist-get r :pkg) 'pkg-fail)) callback-results))
             (succ-res (seq-find (lambda (r) (eq (plist-get r :pkg) 'pkg-succ)) callback-results)))
         (should (eq (plist-get fail-res :status) :failed))
         (should (string-search "Syntax error" (plist-get fail-res :error)))
         (should (eq (plist-get fail-res :stage) :build))
         (should (eq (plist-get succ-res :status) :installed)))
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (let ((text (buffer-string)))
           (should (string-match-p "\\[!\\] pkg-fail" text))
           (should (string-match-p "\\[x\\] pkg-succ: 1.0" text))))))))

(ert-deftest elpaish-test-upgrade-run-async-sprite-cancel-and-rejection ()
  "Test cancelling in-flight sprite future and handling sprite future rejection."
  (elpaish-test-with-temp-env
   (let* ((repo (expand-file-name "repo-cancel" temp-dir))
          (package-user-dir (expand-file-name "installed" temp-dir))
          (package-alist nil)
          (package-selected-packages nil)
          (package-quickstart nil)
          (queue nil)
          (cancelled-future nil)
          (fake-future (list :state :pending))
          (elpaish-install--active-cancel-handle nil)
          (handle nil))
     (elpaish-test-create-dummy-pkg repo "pkg-cancel" "1.0" "Cancel Package")
     (elpaish-register-package 'pkg-cancel repo)
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'run-at-time)
                (lambda (_delay repeat fn &rest _)
                  (unless repeat (setq queue (append queue (list fn))))
                  'fake-timer))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'sprite-future-p) (lambda (f) (and (consp f) (plist-get f :state))))
               ((symbol-function 'sprite-future-cancel)
                (lambda (f &optional _reason) (setq cancelled-future f)))
               ((symbol-function 'elpaish-async-build-package-in-sprite)
                (lambda (&rest _) fake-future))
               ((symbol-function 'sprite-future-then)
                (lambda (future on-resolve &optional on-reject)
                  (setf (plist-get (cdr future) :on-resolve) on-resolve)
                  (setf (plist-get (cdr future) :on-reject) on-reject)
                  future)))
       (setq handle
             (elpaish-install--upgrade-run
              '(pkg-cancel) nil nil 0 nil 'repo-install))
       ;; Advance refresh
       (funcall (pop queue))
       ;; Start package build
       (funcall (pop queue))
       ;; Invoke cancel while future is in-flight
       (funcall handle)
       (should (eq cancelled-future fake-future))
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (should (string-match-p "\\[Canceled\\]" (buffer-string))))
       ;; Settle future after cancellation; should be ignored without altering canceled state
       (let ((resolve-fn (plist-get (cdr fake-future) :on-resolve)))
         (funcall resolve-fn (list :status :ok :artifact "/tmp/none" :pkg 'pkg-cancel)))
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (should (string-match-p "\\[Canceled\\]" (buffer-string))))))))

(ert-deftest elpaish-test-install-local-package-async-and-yielding ()
  "Test `elpaish-install-local-package' runs asynchronously with yielding."
  (elpaish-test-with-temp-env
   (let* ((captured-args nil)
          (fake-handle (lambda (&rest _) :cancelled)))
     (cl-letf (((symbol-function 'elpaish-install-local-packages)
                (lambda (targets &rest args)
                  (setq captured-args (cons targets args))
                  (if (plist-get args :sync)
                      (list (package-desc-create :name (car targets)))
                    fake-handle))))
       ;; Programmatic with :async t returns handle and passes :sync nil :async t
       (let ((handle (elpaish-install-local-package 'test-pkg :async t :delay 0.01)))
         (should (eq handle fake-handle))
         (should (equal (car captured-args) '(test-pkg)))
         (should (null (plist-get (cdr captured-args) :sync)))
         (should (eq (plist-get (cdr captured-args) :async) t))
         (should (= (plist-get (cdr captured-args) :delay) 0.01)))
       ;; Programmatic with :sync t passes :sync t :async nil
       (setq captured-args nil)
       (let ((res (elpaish-install-local-package 'test-pkg :sync t)))
         (should (eq (package-desc-name res) 'test-pkg))
         (should (null (plist-get (cdr captured-args) :async)))
         (should (eq (plist-get (cdr captured-args) :sync) t)))))))

(ert-deftest elpaish-test-upgrade-cleanup-legacy-packages-option-and-toggle ()
  "Test legacy package cleanup option and interactive toggle in progress buffer."
  (elpaish-test-with-temp-env
   (let* ((clean-called 0)
          (package-user-dir (expand-file-name "elpa" temp-dir))
          (package-alist nil)
          (queue nil))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'run-at-time)
                (lambda (_delay repeat fn &rest _)
                  (unless repeat (setq queue (append queue (list fn))))
                  'fake-timer))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'elpaish-install--process-package)
                (lambda (pkg &rest _) (list :pkg pkg :status :upgraded :from "1.0" :to "2.0")))
               ((symbol-function 'elpaish-install-clean-stale-packages)
                (lambda (&optional _no-prompt)
                  (cl-incf clean-called)
                  3)))
       ;; Run with :cleanup t
       (elpaish-install--upgrade-run
        '(pkg-cl1) nil nil 0 nil 'upgrade :cleanup t)
       ;; Drain queue to complete all stages
       (while queue (funcall (pop queue)))
       (should (= clean-called 1))
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (let ((text (buffer-string)))
           (should (string-match-p "Clean up legacy packages (3 legacy packages cleaned)" text))
           (should (string-match-p "Controls:.*cleanup on" text))))
       ;; Test interactive toggle command
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (elpaish-install-progress-toggle-cleanup)
         (should (null (plist-get elpaish-install--progress-context :cleanup)))
         (should (string-match-p "Controls:.*cleanup off" (buffer-string)))
         (elpaish-install-progress-toggle-cleanup)
         (should (plist-get elpaish-install--progress-context :cleanup))
         (should (string-match-p "Controls:.*cleanup on" (buffer-string))))))))

(ert-deftest elpaish-test-upgrade-progress-mode-line-and-archives-summary ()
  "Test shortened mode-line and archive old -> new summary formatting."
  (elpaish-test-with-temp-env
   (let* ((package-user-dir (expand-file-name "elpa" temp-dir))
          (arch-dir (expand-file-name "archives/gnu" package-user-dir))
          (arch-file (expand-file-name "archive-contents" arch-dir))
          (package-archives '(("gnu" . "https://elpa.gnu.org/packages/"))))
     (make-directory arch-dir t)
     (with-temp-file arch-file (insert "dummy"))
     (set-file-times arch-file (encode-time 0 0 12 1 1 2026))
     ;; Test elpaish-install--archives-summary old -> new
     (let ((summary (elpaish-install--archives-summary
                     '(("gnu" . "2026-01-01 10:00"))
                     "[x]" nil)))
       (should (string-match-p "gnu +2026-01-01 10:00 ->" summary)))
     ;; Test repo-install omits archives summary entirely
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'elpaish-install--process-package)
                (lambda (pkg &rest _) (list :pkg pkg :status :installed :to "1.0"))))
       (elpaish-install--upgrade-run '(pkg-no-arch) nil t 0 nil 'repo-install)
       (with-current-buffer "*ELPAish Upgrade Progress*"
         (let ((text (buffer-string)))
           (should-not (string-match-p "Package Archives:" text))
           (should-not (string-match-p "gnu:" text))
           ;; Check mode name and mode line
           (should (equal mode-name "ELPAish"))
           (should (member 'mode-line-buffer-identification mode-line-format))))))))

(ert-deftest elpaish-test-install-progress-menu ()
  "Test `elpaish-install-progress-menu' dispatches chosen action command."
  (let ((invoked nil))
    (cl-letf (((symbol-function 'elpaish-install-progress-pause)
               (lambda () (interactive) (setq invoked 'paused)))
              ((symbol-function 'annotated-completing-read)
               (lambda (actions &rest _)
                 (should (consp actions))
                 ;; Target is elpaish-install-progress-pause
                 'elpaish-install-progress-pause)))
      (elpaish-install-progress-menu)
      (should (eq invoked 'paused)))
    ;; Test fallback when completing-read returns candidate label string
    (setq invoked nil)
    (cl-letf (((symbol-function 'elpaish-install-progress-pause)
               (lambda () (interactive) (setq invoked 'paused-fallback)))
              ((symbol-function 'annotated-completing-read) nil)
              ((symbol-function 'completing-read)
               (lambda (_prompt actions &rest _)
                 (should (consp actions))
                 "Pause operation")))
      (elpaish-install-progress-menu)
      (should (eq invoked 'paused-fallback)))))

(provide 'test-elpaish)
;;; test-elpaish.el ends here
