;;; test-elpaish.el --- ERT Tests for ELPAish multi-track builder -*- lexical-binding: t; no-byte-compile: t; -*-

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

(require 'ert)
(require 'elpaish)
(require 'elpaish-recipes)
(require 'elpaish-check)
(require 'url)

(defmacro elpaish-test-with-temp-env (&rest body)
  "Execute BODY within an isolated temporary registry, work, and output directory."
  `(let* ((temp-dir (make-temp-file "elpaish-test-" t))
          (elpaish-output-dir (expand-file-name "public/" temp-dir))
          (elpaish-work-dir (expand-file-name "repos/" temp-dir))
          (elpaish-registry (make-hash-table :test 'equal))
          (elpaish-sign-packages nil)
          (elpaish-force-rebuild nil)
          (elpaish-run-preflight nil))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p elpaish-server-process)
         (elpaish-stop-server))
       (delete-directory temp-dir t))))

(defun elpaish-test-create-dummy-pkg (dir name version summary &optional reqs)
  "Create a dummy package file in DIR."
  (make-directory dir t)
  (let ((file (expand-file-name (concat name ".el") dir)))
    (with-temp-file file
      (insert (format ";;; %s.el --- %s -*- lexical-binding: t; -*-\n" name summary))
      (when version
        (insert ";; Version: " version "\n"))
      (when reqs
        (insert ";; Package-Requires: " (format "%S" reqs) "\n"))
      (insert ";; Keywords: test, tools\n")
      (insert ";; URL: https://github.com/tychoish/" name "\n\n")
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
       (should (elpaish-recipe-built-version-elpaish recipe))
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
  "Test multi-track `archive-contents' generation."
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

     (elpaish-generate-github-index 'elpaish)
     (elpaish-generate-top-index)

     (let ((track-index (expand-file-name "elpaish/index.html" elpaish-output-dir))
           (top-index (expand-file-name "index.html" elpaish-output-dir)))
       (should (file-exists-p track-index))
       (should (file-exists-p top-index))
       (with-temp-buffer
         (insert-file-contents track-index)
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
         (should (search-forward "font-size:18px" nil t))
         (goto-char (point-min))
         (should (search-forward "pkg-name" nil t))
         (goto-char (point-min))
         (should (search-forward "table-wrapper" nil t)))
       (with-temp-buffer
         (insert-file-contents top-index)
         (goto-char (point-min))
         (should (search-forward "elpaish (Snapshots)" nil t))
         (goto-char (point-min))
         (should (search-forward "elpaish-stable (Releases)" nil t))
         (goto-char (point-min))
         (should (search-forward "elpaish-staging (Pre-release)" nil t))
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
     (elpaish-generate-github-index 'elpaish)
     (elpaish-generate-top-index)

     (let ((track-index (expand-file-name "elpaish/index.html" elpaish-output-dir))
           (top-index (expand-file-name "index.html" elpaish-output-dir)))
       (with-temp-buffer
         (insert-file-contents track-index)
         (goto-char (point-min))
         (should (search-forward "color:#000000;background:#ffffff" nil t))
         (goto-char (point-min))
         (should (search-forward "color:#0000aa" nil t))
         (goto-char (point-min))
         (should (search-forward "color:#721045" nil t))
         (goto-char (point-min))
         (should (search-forward "min-width:320px" nil t))
         (goto-char (point-min))
         (should (search-forward "white-space:nowrap!important" nil t))
         (goto-char (point-min))
         (should (search-forward "annotated-completing-read" nil t)))
       (with-temp-buffer
         (insert-file-contents top-index)
         (goto-char (point-min))
         (should (search-forward "color:#000000;background:#ffffff" nil t))
         (goto-char (point-min))
         (should (search-forward "background:#00538b" nil t)))))))
(ert-deftest elpaish-test-build-all-multi-track ()
  "Test building all tracks with `elpaish-build-all`."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "all-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "all-pkg" "1.0.0" "All Track Test")
     (elpaish-register-package 'all-pkg pkg-dir)

     (elpaish-build-all 'all)

     ;; Snapshot track should exist
     (should (file-exists-p (expand-file-name "elpaish/archive-contents" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "elpaish/index.html" elpaish-output-dir)))
     ;; Staging track should exist
     (should (file-exists-p (expand-file-name "elpaish-staging/archive-contents" elpaish-output-dir)))
     (should (file-exists-p (expand-file-name "elpaish-staging/index.html" elpaish-output-dir)))
     ;; Top index should exist
     (should (file-exists-p (expand-file-name "index.html" elpaish-output-dir))))))

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
       (should-not (elpaish-recipe-built-version-elpaish recipe))))))

(ert-deftest elpaish-test-timer-controls ()
  "Test starting and stopping auto-build background timer."
  (unwind-protect
      (progn
        (elpaish-start-auto-build "3600")
        (should (timerp elpaish-timer))
        (elpaish-stop-auto-build)
        (should-not elpaish-timer)
        (elpaish-start-auto-build "5 mins" t)
        (should (timerp elpaish-timer))
        (elpaish-stop-auto-build))
    (elpaish-stop-auto-build)))

(ert-deftest elpaish-test-status-ui ()
  "Test status buffer creation and multi-track column population."
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
           (should (process-live-p elpaish-server-process))

           ;; Verify HTTP request to preview server
           (let ((url-buf (url-retrieve-synchronously (format "http://127.0.0.1:%d/elpaish/archive-contents" test-port) t t 5)))
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
   (let ((doc-root temp-dir))
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
       (should (string-prefix-p "HTTP/1.1 400 Bad Request" (car res)))))))

(ert-deftest elpaish-test-stable-track-omission ()
  "Test omitting packages without clean semver tag from stable track."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "untagged-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "untagged-pkg" "1.0.0" "Untagged Test")
     (let ((recipe (elpaish-register-package 'untagged-pkg pkg-dir)))
       ;; No clean semver git tag present
       (should-not (elpaish-derive-version recipe 'elpaish-stable))
       (should-not (elpaish-build-package recipe 'elpaish-stable))
       (let ((ac-file (elpaish-generate-archive-contents 'elpaish-stable)))
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
       (elpaish--generate-pkg-file dest "desc-pkg" "1.2.3" "Desc Summary" '((emacs "27.1")) "https://example.com" '("tools"))
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

(ert-deftest elpaish-test-snapshot-skip-unchanged-rebuild ()
  "Test that snapshot builds are skipped if commit has not changed since last build."
  (elpaish-test-with-temp-env
   (let ((pkg-dir (expand-file-name "skip-rebuild-pkg" temp-dir)))
     (elpaish-test-create-dummy-pkg pkg-dir "skip-rebuild-pkg" "1.0.0" "Skip Rebuild Test")
     (elpaish-register-package 'skip-rebuild-pkg pkg-dir)

     ;; Mock git commands to simulate a clean repository with commit hash "abc1234"
     (cl-letf (((symbol-function 'magit-git-string)
                (lambda (cmd &rest args)
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
       (let* ((recipe (gethash "skip-rebuild-pkg" elpaish-registry))
              (dest1 (elpaish-build-package recipe 'elpaish))
              (ver1 (elpaish-recipe-built-version-elpaish recipe)))
         (should dest1)
         (should (file-exists-p dest1))
         (should ver1)

         ;; Generate archive-contents
         (elpaish-generate-archive-contents 'elpaish)

         ;; 2. Second build without commit change: should return existing dest and keep exact same version
         (let ((dest2 (elpaish-build-package recipe 'elpaish))
               (ver2 (elpaish-recipe-built-version-elpaish recipe)))
           (should (equal dest1 dest2))
           (should (equal ver1 ver2))))))))

(provide 'test-elpaish)
;;; test-elpaish.el ends here
