;;; test-elpaish.el --- ERT Tests for ELPAish package repository toolkit -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: tychoish
;; Keywords: test, elpa, package

;;; Code:

(require 'package)
(let ((ci-elpa (expand-file-name "elpa-ci" default-directory))
      (local-elpa (expand-file-name "elpa" default-directory)))
  (cond
   ((file-directory-p ci-elpa)
    (setq package-user-dir ci-elpa)
    (package-initialize))
   ((file-directory-p local-elpa)
    (setq package-user-dir local-elpa)
    (package-initialize))
   (t
    (package-initialize))))
(let ((pkg-dir (expand-file-name "pkg" default-directory)))
  (when (file-directory-p pkg-dir)
    (push pkg-dir load-path)))
(require 'ert)
(require 'elpaish)
(require 'elpaish-recipes)
(require 'elpaish-check)
(require 'elpaish-website)
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
          (package-archives (copy-sequence package-archives))
          (package-check-signature nil))
     (unwind-protect
         (progn ,@body)
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
     (should (equal (elpaish-recipe-test-dir recipe-b) "test"))
     (should (equal (elpaish-recipe-preflight-skip recipe-b) '(checkdoc))))))

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
     (let ((res (elpaish-check-package pkg-dir :skip-checks t)))
       (should (plist-get res :passed))))))

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
       (should (equal (elpaish-recipe-source-dir (gethash "pkg-one" elpaish-registry)) "pkg-one"))))))

(ert-deftest elpaish-test-recipe-path-local-and-remote-fallback ()
  "Test local checkout search and remote URL fallback for `elpaish-recipe-path'."
  (elpaish-test-with-temp-env
   (let* ((search-root (expand-file-name "search-root" temp-dir))
          (elpaish-recipe-local-search-dirs (list search-root)))
     (make-directory (expand-file-name "found-pkg" search-root) t)
     (should (equal (elpaish-recipe-path "found-pkg" "https://example.com/found-pkg.git")
                    (expand-file-name "found-pkg" search-root)))
     (should (equal (elpaish-recipe-path "missing-pkg" "https://example.com/missing-pkg.git")
                    "https://example.com/missing-pkg.git")))))

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
  "Test that transient menu `elpaish-menu' and alias `elpaish-dispatch' exist."
  (should (fboundp 'elpaish-menu))
  (should (fboundp 'elpaish-dispatch)))

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
       (should (search-forward "colspan=\"4\"" nil t))))))

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
  "Test `elpaish-project-has-el-files-p' detection of .el files."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "has-el" temp-dir))
         (empty-dir (expand-file-name "no-el" temp-dir)))
     (make-directory pkg-dir t)
     (make-directory empty-dir t)
     (with-temp-file (expand-file-name "foo.el" pkg-dir)
       (insert ";;; foo.el\n"))
     (should (elpaish-project-has-el-files-p pkg-dir))
     (should-not (elpaish-project-has-el-files-p empty-dir)))))

(ert-deftest elpaish-test-setup-compile-command ()
  "Test `elpaish-setup-compile-command' sets `compile-command' buffer-locally."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "builder-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (with-temp-file (expand-file-name "builder-pkg.el" pkg-dir)
       (insert ";;; builder-pkg.el\n"))
     (with-temp-buffer
       (setq-local default-directory (file-name-as-directory pkg-dir))
       (emacs-lisp-mode)
       (elpaish-setup-compile-command pkg-dir)
       (should (string-prefix-p "emacsclient --eval" compile-command))
       (should (string-match-p "elpaish-run-checks" compile-command))
       (should (string-match-p "builder-pkg" compile-command))))))

(ert-deftest elpaish-test-maybe-setup-builder ()
  "Test `elpaish-maybe-setup-builder' hook sets `compile-command' on .el buffers."
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
         (elpaish-maybe-setup-builder)
         (should (string-match-p "elpaish-run-checks" compile-command)))))))

(ert-deftest elpaish-test-check-buffer-name-formatting ()
  "Test that check compilation buffer name formats as *<project-name>-checks*."
  (elpaish-test-with-temp-env
   (let* ((elpaish-check-buffer-name nil)
          (name (elpaish-check-buffer-name "/path/to/my-package/")))
     (should (equal name "*my-package-checks*")))))
(ert-deftest elpaish-test-run-check-alias ()
  "Test `elpaish-run-check' alias."
  (should (fboundp 'elpaish-run-check))
  (should (equal (indirect-function 'elpaish-run-check)
                 (indirect-function 'elpaish-run-checks))))

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
   (let ((elpaish-bootstrap-packages '(boot-a boot-b))
         (installed-log nil))
     (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
               ((symbol-function 'package-install) (lambda (pkg) (push pkg installed-log))))
       ;; Default fallback
       (elpaish-install-packages)
       (should (equal installed-log '(boot-b boot-a)))
       (setq installed-log nil)

       ;; List of symbols
       (elpaish-install-packages '(pkg-x pkg-y))
       (should (equal installed-log '(pkg-y pkg-x)))))))

(ert-deftest elpaish-test-install-packages-continue-on-error ()
  "Test `elpaish-install-packages' installs packages and continues on error."
  (elpaish-test-with-temp-env
   (let ((elpaish-bootstrap-packages '(dummy-pkg-a dummy-pkg-b))
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
       (elpaish-install-packages 'good-pkg 'fail-pkg 'already-installed :refresh t)
       (should refresh-called)
       (should (equal installed-log '(good-pkg)))))))

(ert-deftest elpaish-test-upgrade-packages-continue-on-error ()
  "Test `elpaish-upgrade-packages' refreshes contents and upgrades packages without error."
  (elpaish-test-with-temp-env
   (let ((elpaish-bootstrap-packages '(boot-pkg))
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
       (elpaish-upgrade-packages 'upgrade-pkg 'up-to-date-pkg)
       (should refresh-called)
       (should (equal upgraded-log '(upgrade-pkg)))))))

(provide 'test-elpaish)
;;; test-elpaish.el ends here
