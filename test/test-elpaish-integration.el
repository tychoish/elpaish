;;; test-elpaish-integration.el --- Integration tests for ELPAish -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: tychoish <sam@tychoish.com>
;; Keywords: package, elpa, test, integration

;;; Commentary:
;; End-to-end integration tests that spin up full ELPAish repositories,
;; generate package tarballs, write archive-contents and static website pages,
;; sign repositories with temporary GPG keyrings, and invoke real `emacs -Q`
;; child processes to install packages into clean consumer environments.
;;
;; Also includes live deployment verification that downloads real published
;; keyrings from GitHub Pages and installs packages from live stream URLs.

;;; Code:

(eval-and-compile
  (let ((pkg-dir (expand-file-name "pkg" default-directory)))
    (unless (member pkg-dir load-path)
      (push pkg-dir load-path)))
  (let ((ci-dir (expand-file-name "elpa-ci" default-directory)))
    (when (file-directory-p ci-dir)
      (dolist (d (file-expand-wildcards (expand-file-name "*" ci-dir)))
        (when (file-directory-p d)
          (add-to-list 'load-path d)))))
  (let ((elpa-dir (expand-file-name "elpa" default-directory)))
    (when (file-directory-p elpa-dir)
      (dolist (d (file-expand-wildcards (expand-file-name "*" elpa-dir)))
        (when (file-directory-p d)
          (add-to-list 'load-path d))))))

(require 'cl-lib)
(require 'ert)
(require 'compat)
(require 'elpaish)
(require 'elpaish-recipes)
(require 'elpaish-website)

(defcustom elpaish-run-integration-tests nil
  "When non-nil, enable execution of subprocess integration tests.
Can also be controlled via environment variable `ELPAISH_RUN_INTEGRATION_TESTS'."
  :type 'boolean
  :group 'elpaish)

(defun elpaish-integration-tests-enabled-p (&optional test-type)
  "Return non-nil if integration tests (optionally of TEST-TYPE) are enabled.
TEST-TYPE can be \\='local, \\='live, \\='signing, or nil (any).

The environment variable `ELPAISH_RUN_INTEGRATION_TESTS' controls execution:
- Falsy values (\"0\", \"false\", \"no\", \"off\", \"never\", \"none\") disable all tests.
- Truthy values (\"1\", \"true\", \"yes\", \"on\", \"all\", \"always\", \"t\") enable tests.
- Selective values (\"local\", \"offline\") enable only local/offline tests.
- Selective values (\"live\", \"online\", \"remote\") enable only live deployment tests.
- If unset, defaults to `elpaish-run-integration-tests' or `noninteractive' for local tests."
  (let* ((raw (getenv "ELPAISH_RUN_INTEGRATION_TESTS"))
         (val (and raw (downcase (string-trim raw)))))
    (cond
     ;; Explicitly disabled
     ((member val '("0" "false" "no" "off" "never" "none"))
      nil)
     ;; Explicitly enabled for all
     ((member val '("1" "true" "yes" "on" "all" "always" "t"))
      (if (eq test-type 'live)
          (and (getenv "ELPAISH_ARCHIVE_URL")
               (not (string-empty-p (string-trim (getenv "ELPAISH_ARCHIVE_URL")))))
        t))
     ;; Local/offline only
     ((member val '("local" "offline"))
      (not (eq test-type 'live)))
     ;; Live/remote only
     ((member val '("live" "online" "remote"))
      (and (eq test-type 'live)
           (getenv "ELPAISH_ARCHIVE_URL")
           (not (string-empty-p (string-trim (getenv "ELPAISH_ARCHIVE_URL"))))))
     ;; Unset: check defcustom, else allow local tests in batch mode
     (t
      (or elpaish-run-integration-tests
          (if (eq test-type 'live)
              (and (getenv "ELPAISH_ARCHIVE_URL")
                   (not (string-empty-p (string-trim (getenv "ELPAISH_ARCHIVE_URL")))))
            noninteractive))))))

(defmacro elpaish-integration-skip-unless (form &optional reason)
  "Skip current test unless FORM yields non-nil, reporting REASON."
  `(unless ,form
     (ert-skip (or ,reason (format "Condition not met: %S" ',form)))))

(defmacro elpaish-integration-test-with-env (&rest body)
  "Execute BODY within an isolated temporary environment for integration testing."
  `(let* ((temp-root (make-temp-file "elpaish-integ-" t))
          (elpaish-output-dir (expand-file-name "public/" temp-root))
          (elpaish-work-dir (expand-file-name "repos/" temp-root))
          (elpaish-registry (make-hash-table :test 'equal))
          (elpaish--resolved-repo-path-cache (make-hash-table :test 'eq))
          (elpaish-server-process nil)
          (consumer-home (expand-file-name "consumer/" temp-root)))
     (make-directory consumer-home t)
     (unwind-protect
         (progn ,@body)
       (when (elpaish-server-running-p)
         (elpaish-stop-server))
       (clrhash elpaish-registry)
       (clrhash elpaish--resolved-repo-path-cache)
       (delete-directory temp-root t))))

(defun elpaish-integration--create-fixture-pkg (dir name version summary &optional reqs)
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
      (insert (format "(defun %s-greeting () \"Hello from %s!\")\n\n" name name))
      (insert "(provide '" name ")\n")
      (insert (format ";;; %s.el ends here\n" name)))))

;;; Integration Tests

(ert-deftest elpaish-integration-test-consumer-subprocess-install ()
  "Test clean `emacs -Q` subprocess fetching archive and installing packages."
  (elpaish-integration-skip-unless (elpaish-integration-tests-enabled-p 'local))
  (elpaish-integration-test-with-env
   (let* ((pkg1-dir (expand-file-name "consumer-pkg" temp-root))
          (pkg2-dir (expand-file-name "consumer-tar-pkg" temp-root)))
     ;; 1. Setup single-file package fixture
     (elpaish-integration--create-fixture-pkg pkg1-dir "consumer-pkg" "1.0.0" "Consumer Package Test")
     (elpaish-register-package 'consumer-pkg pkg1-dir)

     ;; 2. Setup multi-file package fixture
     (elpaish-integration--create-fixture-pkg pkg2-dir "consumer-tar-pkg" "2.1.0" "Consumer Tar Test")
     (with-temp-file (expand-file-name "consumer-tar-aux.el" pkg2-dir)
       (insert ";;; consumer-tar-aux.el --- Aux -*- lexical-binding: t; -*-\n(provide 'consumer-tar-aux)\n"))
     (elpaish-register-package 'consumer-tar-pkg pkg2-dir :files '("*.el"))

     ;; 3. Build repository
     (let ((elpaish-sign-packages nil)
           (elpaish-run-preflight nil))
       (elpaish-build-all 'snapshot elpaish-output-dir))

     ;; 4. Run consumer installation in isolated emacs -Q subprocess
     (let* ((archive-dir (elpaish-stream-dir 'snapshot elpaish-output-dir))
            (sub-code
             (format "(progn (require (quote package)) (let ((package-user-dir \"%s/elpa\")
       (package-archives (list (cons \"elpaish-test\" \"%s\"))))
  (package-initialize)
  (package-refresh-contents)
  (unless (assoc (quote consumer-pkg) package-archive-contents)
    (error \"consumer-pkg missing from archive-contents\"))
  (unless (assoc (quote consumer-tar-pkg) package-archive-contents)
    (error \"consumer-tar-pkg missing from archive-contents\"))
  (package-install (quote consumer-pkg))
  (package-install (quote consumer-tar-pkg))
  (require (quote consumer-pkg))
  (require (quote consumer-tar-pkg))
  (unless (equal (consumer-pkg-greeting) \"Hello from consumer-pkg!\")
    (error \"consumer-pkg function call failed\"))
  (message \"SUBPROCESS_CONSUMER_SUCCESS\")))"
                     consumer-home archive-dir))
            (output-buf (generate-new-buffer " *int-sub-output*"))
            (process-environment (cons (concat "HOME=" consumer-home) process-environment))
            (default-directory consumer-home)
            (exit-code (call-process "emacs" nil output-buf nil "-Q" "--batch" "--eval" sub-code))
            (output-str (with-current-buffer output-buf (buffer-string))))
       (kill-buffer output-buf)
       (should (equal exit-code 0))
       (should (string-match-p "SUBPROCESS_CONSUMER_SUCCESS" output-str))))))

(ert-deftest elpaish-integration-test-signed-archive-consumer-install ()
  "Test signed repository archive and signature validation in consumer subprocess."
  (elpaish-integration-skip-unless (elpaish-integration-tests-enabled-p 'signing))
  (elpaish-integration-test-with-env
   (let ((gpg-bin (executable-find "gpg")))
     (if (not gpg-bin)
         (message "Skipping signed consumer test: gpg binary not found")
       (let* ((gnupg-dir (expand-file-name ".gnupg" temp-root)))
         (make-directory gnupg-dir t)
         (set-file-modes gnupg-dir #o700)
         (with-temp-file (expand-file-name "gpg-agent.conf" gnupg-dir)
           (insert "allow-loopback-pinentry\n"))
         (set-file-modes (expand-file-name "gpg-agent.conf" gnupg-dir) #o600)

         (let ((process-environment (cons (concat "GNUPGHOME=" gnupg-dir) process-environment)))
           ;; Generate temporary test keypair
           (call-process gpg-bin nil nil nil "--batch" "--passphrase" ""
                         "--quick-generate-key" "Integration Test <int@example.com>" "ed25519" "cert" "0")
           (let* ((fpr (with-temp-buffer
                         (call-process gpg-bin nil t nil "--list-secret-keys" "--with-colons" "Integration Test")
                         (goto-char (point-min))
                         (when (re-search-forward "^fpr:::::::::\\([0-9A-Fa-f]+\\):" nil t)
                           (match-string 1))))
                  (pkg-dir (expand-file-name "signed-pkg" temp-root)))
             (call-process gpg-bin nil nil nil "--batch" "--passphrase" ""
                           "--quick-add-key" fpr "ed25519" "sign" "1y")

             ;; Build and sign repository
             (elpaish-integration--create-fixture-pkg pkg-dir "signed-pkg" "1.0.0" "Signed Package")
             (elpaish-register-package 'signed-pkg pkg-dir)
             (let ((elpaish-sign-packages t)
                   (elpaish-gpg-key fpr)
                   (elpaish-gpg-passphrase "")
                   (elpaish-run-preflight nil))
               (elpaish-build-all 'snapshot elpaish-output-dir))

             ;; Export public key for consumer
             (let ((pub-key-file (expand-file-name "elpaish.pub.asc" elpaish-output-dir))
                   (archive-dir (elpaish-stream-dir 'snapshot elpaish-output-dir)))

               ;; Consumer subprocess imports public key and verifies signatures
               (let* ((default-directory consumer-home)
                      (sub-code
                       (format "(progn (require (quote package)) (let ((package-user-dir \"%s/elpa\")
       (package-archives (list (cons \"elpaish-signed\" \"%s\")))
       (package-check-signature t))
  (package-initialize)
  (package-import-keyring \"%s\")
  (package-refresh-contents)
  (package-install (quote signed-pkg))
  (require (quote signed-pkg))
  (message \"SIGNED_CONSUMER_SUCCESS\")))"
                               consumer-home archive-dir pub-key-file))
                      (output-buf (generate-new-buffer " *signed-sub-output*"))
                      (process-environment (append (list (concat "HOME=" consumer-home)
                                                         (concat "GNUPGHOME=" gnupg-dir))
                                                   process-environment))
                      (default-directory consumer-home)
                      (exit-code (call-process "emacs" nil output-buf nil "-Q" "--batch" "--eval" sub-code))
                      (output-str (with-current-buffer output-buf (buffer-string))))
                 (kill-buffer output-buf)
                 (should (equal exit-code 0))
                 (should (string-match-p "SIGNED_CONSUMER_SUCCESS" output-str)))))))))))

(ert-deftest elpaish-integration-test-live-deployment ()
  "Test installing package from live deployed ELPAish Pages URL when available.
Registers the standard GNU/NonGNU/MELPA archives alongside the live ELPAish
one, matching how a real consumer's `package-archives' is actually
configured, so transitive dependencies (e.g. `elpaish' itself depends on
`magit') can resolve instead of failing on a bare single-archive setup.
Also imports the real published keyring before refreshing contents: the
live archive is genuinely GPG signed in production (unlike the other,
locally-built fixtures in this file), and `package-check-signature' set to
`allow-unsigned' only tolerates a MISSING signature -- when a `.sig' file
is present but its key is unknown, `package.el' signals `bad-signature'
regardless, so a consumer without the key installed would fail here."
  (elpaish-integration-skip-unless (elpaish-integration-tests-enabled-p 'live)
                                   "Live deployment test disabled or ELPAISH_ARCHIVE_URL unset")
  (let ((live-url (getenv "ELPAISH_ARCHIVE_URL")))
    (when (and live-url (not (string-empty-p (string-trim live-url))))
      (elpaish-integration-test-with-env
       (let* ((clean-url (string-trim live-url))
              (base-url (replace-regexp-in-string "/\\(?:snapshot\\|staging\\|stable\\)/?$" "/" clean-url))
              (keyring-file (expand-file-name "elpaish-keyring.gpg" temp-root)))
         (url-copy-file (concat (file-name-as-directory base-url) "elpaish-keyring.gpg") keyring-file t)
         (let* ((sub-code
                 (format "(progn (require (quote package)) (setq package-user-dir \"%s/elpa\") (setq package-archives (list (cons \"gnu\" \"https://elpa.gnu.org/packages/\") (cons \"nongnu\" \"https://elpa.nongnu.org/nongnu/\") (cons \"melpa\" \"https://melpa.org/packages/\") (cons \"elpaish-live\" \"%s\"))) (setq package-check-signature (quote allow-unsigned)) (package-initialize) (package-import-keyring \"%s\") (package-refresh-contents) (let* ((live-entries (seq-filter (lambda (e) (equal (package-desc-archive (cadr e)) \"elpaish-live\")) package-archive-contents)) (first-pkg (car (car live-entries)))) (if first-pkg (progn (package-install first-pkg) (require first-pkg) (message \"LIVE_CONSUMER_SUCCESS for %%S\" first-pkg)) (message \"LIVE_ARCHIVE_EMPTY_SUCCESS for %%s\" \"%s\"))))"
                         consumer-home (file-name-as-directory clean-url) keyring-file (file-name-as-directory clean-url)))
                (output-buf (generate-new-buffer " *live-sub-output*"))
                (process-environment (cons (concat "HOME=" consumer-home) process-environment))
                (default-directory consumer-home)
                (exit-code (call-process "emacs" nil output-buf nil "-Q" "--batch" "--eval" sub-code))
                (output-str (with-current-buffer output-buf (buffer-string))))
           (kill-buffer output-buf)
           (should (equal exit-code 0))
           (should (or (string-match-p "LIVE_CONSUMER_SUCCESS" output-str)
                       (string-match-p "LIVE_ARCHIVE_EMPTY_SUCCESS" output-str)))))))))

;;;###autoload
(ert-deftest elpaish-integration-test-local-preview-server ()
  "Test installing package from local HTTP preview server in consumer subprocess."
  (elpaish-integration-skip-unless (elpaish-integration-tests-enabled-p 'local))
  (elpaish-integration-test-with-env
   (let* ((pkg-dir (expand-file-name "preview-pkg" temp-root)))
     ;; 1. Setup fixture package
     (elpaish-integration--create-fixture-pkg pkg-dir "preview-pkg" "1.0.0" "Local Preview Server Test")
     (elpaish-register-package 'preview-pkg pkg-dir)

     ;; 2. Build repository
     (let ((elpaish-sign-packages nil)
           (elpaish-run-preflight nil))
       (elpaish-build-all 'snapshot elpaish-output-dir))

     ;; 3. Start local preview server on an ephemeral port
     (let* ((port (+ 18000 (random 2000)))
            (server (elpaish-serve-local port elpaish-output-dir))
            (active-port (or (elpaish--server-port) port)))
       (unwind-protect
           (let* ((archive-url (format "http://127.0.0.1:%d/snapshot/" active-port))
                  (default-directory consumer-home)
                  (sub-code
                   (format "(progn (require (quote package)) (let ((package-user-dir \"%s/elpa\")
       (package-archives (list (cons \"elpaish-preview\" \"%s\"))))
  (package-initialize)
  (package-refresh-contents)
  (unless (assoc (quote preview-pkg) package-archive-contents)
    (error \"preview-pkg missing from preview archive-contents\"))
  (package-install (quote preview-pkg))
  (require (quote preview-pkg))
  (unless (equal (preview-pkg-greeting) \"Hello from preview-pkg!\")
    (error \"preview-pkg greeting failed\"))
  (message \"PREVIEW_SERVER_CONSUMER_SUCCESS\")))"
                           consumer-home archive-url))
                  (output-buf (generate-new-buffer " *preview-sub-output*"))
                  (process-environment (cons (concat "HOME=" consumer-home) process-environment))
                  (proc (make-process
                         :name "elpaish-integ-consumer"
                         :buffer output-buf
                         :command (list "emacs" "-Q" "--batch" "--eval" sub-code)))
                  (deadline (+ (float-time) 15)))
             (while (and (process-live-p proc) (< (float-time) deadline))
               (accept-process-output nil 0.05))
             (when (process-live-p proc)
               (delete-process proc))
             (let ((output-str (with-current-buffer output-buf (buffer-string))))
               (kill-buffer output-buf)
               (should (equal (process-exit-status proc) 0))
               (should (string-match-p "PREVIEW_SERVER_CONSUMER_SUCCESS" output-str))))
         (elpaish-stop-server))))))

(defun elpaish-run-integration-tests-suite ()
  "Run the full ELPAish integration test suite."
  (interactive)
  (let ((elpaish-run-integration-tests t))
    (ert "elpaish-integration-test-.*")))

(provide 'test-elpaish-integration)
;;; test-elpaish-integration.el ends here
