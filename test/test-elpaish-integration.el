;;; test-elpaish-integration.el --- Subprocess integration and consumer install tests -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: tychoish
;; Keywords: test, elpa, package, integration

;;; Commentary:
;; Isolated subprocess integration tests for ELPAish package archives.
;; Exercises real `emacs -Q --batch' consumer installation workflows against
;; built archives (archive fetching, dependency resolution, GPG signature
;; verification, and package activation).
;;
;; Run explicitly via:
;;   emacs --batch -L pkg -L test -l test/test-elpaish-integration.el -f elpaish-run-integration-tests-suite
;; or inside Emacs:
;;   (ert "elpaish-integration-test-.*")

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

(require 'cl-lib)
(require 'ert)
(require 'elpaish)
(require 'elpaish-recipes)
(require 'elpaish-website)

(defcustom elpaish-run-integration-tests nil
  "When non-nil, enable execution of subprocess integration tests.
Can also be enabled via environment variable `ELPAISH_RUN_INTEGRATION_TESTS=1'."
  :type 'boolean
  :group 'elpaish)

(defun elpaish-integration-tests-enabled-p ()
  "Return non-nil if integration tests are enabled."
  (or elpaish-run-integration-tests
      (equal (getenv "ELPAISH_RUN_INTEGRATION_TESTS") "1")
      noninteractive))

(defmacro elpaish-integration-test-with-env (&rest body)
  "Execute BODY within an isolated temporary environment for integration testing."
  `(let* ((temp-root (make-temp-file "elpaish-integ-" t))
          (elpaish-output-dir (expand-file-name "public/" temp-root))
          (elpaish-work-dir (expand-file-name "repos/" temp-root))
          (elpaish-registry (make-hash-table :test 'equal))
          (consumer-home (expand-file-name "consumer/" temp-root)))
     (make-directory consumer-home t)
     (unwind-protect
         (progn ,@body)
       (when (process-live-p elpaish-server-process)
         (elpaish-stop-server))
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
     (let* ((archive-dir (elpaish-track-dir 'snapshot elpaish-output-dir))
            (sub-code
             (format "(progn
  (require (quote package))
  (setq package-user-dir \"%s/elpa\")
  (setq package-archives (list (cons \"elpaish-test\" \"%s\")))
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
  (message \"SUBPROCESS_CONSUMER_SUCCESS\"))"
                     consumer-home archive-dir))
            (output-buf (generate-new-buffer " *int-sub-output*"))
            (process-environment (cons (concat "HOME=" consumer-home) process-environment))
            (exit-code (call-process "emacs" nil output-buf nil "-Q" "--batch" "--eval" sub-code))
            (output-str (with-current-buffer output-buf (buffer-string))))
       (kill-buffer output-buf)
       (should (equal exit-code 0))
       (should (string-match-p "SUBPROCESS_CONSUMER_SUCCESS" output-str))))))

(ert-deftest elpaish-integration-test-signed-archive-consumer-install ()
  "Test signed repository archive and signature validation in consumer subprocess."
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
                   (archive-dir (elpaish-track-dir 'snapshot elpaish-output-dir)))

               ;; Consumer subprocess imports public key and verifies signatures
               (let* ((default-directory consumer-home)
                      (sub-code
                       (format "(progn
  (require (quote package))
  (setq package-user-dir \"%s/elpa\")
  (setq package-archives (list (cons \"elpaish-signed\" \"%s\")))
  (package-initialize)
  (package-import-keyring \"%s\")
  (setq package-check-signature t)
  (package-refresh-contents)
  (package-install (quote signed-pkg))
  (require (quote signed-pkg))
  (message \"SIGNED_CONSUMER_SUCCESS\"))"
                               consumer-home archive-dir pub-key-file))
                      (output-buf (generate-new-buffer " *signed-sub-output*"))
                      (process-environment (cons (concat "HOME=" consumer-home) process-environment))
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
`allow-unsigned' only tolerates a MISSING signature — when a `.sig' file
is present but its key is unknown, `package.el' signals `bad-signature'
regardless (see `package--check-signature-content' in package.el), so a
consumer without the key installed would fail here exactly as this test
used to."
  (when-let* ((live-url (getenv "ELPAISH_ARCHIVE_URL")))
    (unless (string-empty-p live-url)
      (elpaish-integration-test-with-env
         (let* ((base-url (replace-regexp-in-string "\\(?:elpaish\\|snapshot\\)/\\'" "" live-url))
              (keyring-file (expand-file-name "elpaish-keyring.gpg" temp-root)))
         (url-copy-file (concat base-url "elpaish-keyring.gpg") keyring-file t)
         (let* ((default-directory consumer-home)
                (sub-code
                 (format "(progn
  (require (quote package))
  (setq package-user-dir \"%s/elpa\")
  (setq package-archives
        (list (cons \"gnu\" \"https://elpa.gnu.org/packages/\")
              (cons \"nongnu\" \"https://elpa.nongnu.org/nongnu/\")
              (cons \"melpa\" \"https://melpa.org/packages/\")
              (cons \"elpaish-live\" \"%s\")))
  (setq package-check-signature (quote allow-unsigned))
  (package-initialize)
  (package-import-keyring \"%s\")
  (package-refresh-contents)
  (let* ((live-entries
          (seq-filter (lambda (e) (equal (package-desc-archive (cadr e)) \"elpaish-live\"))
                      package-archive-contents))
         (first-pkg (car (car live-entries))))
    (unless first-pkg
      (error \"No packages found in live elpaish archive\"))
    (package-install first-pkg)
    (require first-pkg)
    (message \"LIVE_CONSUMER_SUCCESS for %%S\" first-pkg)))"
                         consumer-home live-url keyring-file))
                (output-buf (generate-new-buffer " *live-sub-output*"))
                (process-environment (cons (concat "HOME=" consumer-home) process-environment))
                (exit-code (call-process "emacs" nil output-buf nil "-Q" "--batch" "--eval" sub-code))
                (output-str (with-current-buffer output-buf (buffer-string))))
           (kill-buffer output-buf)
           (should (equal exit-code 0))
           (should (string-match-p "LIVE_CONSUMER_SUCCESS" output-str))))))))

;;;###autoload
(defun elpaish-run-integration-tests-suite ()
  "Run the full ELPAish integration test suite."
  (interactive)
  (let ((elpaish-run-integration-tests t))
    (ert "elpaish-integration-test-.*")))

(provide 'test-elpaish-integration)
;;; test-elpaish-integration.el ends here
