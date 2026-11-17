;;; test-elpaish-isolation.el --- ERT subprocess isolation tests -*- lexical-binding: t; -*-
;;; Commentary:
;; Verify subprocess state is temporary, including exit hooks.
;;; Code:
(require 'ert)
(require 'elpaish-check)
(ert-deftest elpaish-test-ert-isolates-home-and-cleans-up ()
  "Test processes use a temporary home through their exit hooks."
  (let* ((dir (make-temp-file "elpaish-isolation-test-" t))
         (file (expand-file-name "test-fixture.el" dir))
         (marker (expand-file-name "result" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(require 'ert)\n"
                    "(ert-deftest testpkg/isolation ()\n"
                    "  (should (string-match-p \"elpaish-check-ert-\" (getenv \"HOME\")))\n"
                    "  (should (equal (file-name-as-directory (expand-file-name user-emacs-directory))\n"
                    "                 (file-name-as-directory (expand-file-name (getenv \"HOME\")))))\n"
                    "  (should (equal (expand-file-name default-directory) user-emacs-directory))\n"
                    "  (add-hook 'kill-emacs-hook\n"
                    "            (lambda () (with-temp-file " (prin1-to-string marker)
                    " (insert user-emacs-directory))\n"
                    "              (with-temp-file (expand-file-name \"history\" user-emacs-directory) (insert \"test\")))))\n"))
          (should-not (elpaish-check--ert (list file) "testpkg" nil nil))
          (should (file-exists-p marker))
          (let ((result (with-temp-buffer (insert-file-contents marker) (buffer-string))))
            (should (string-match-p "elpaish-check-ert-" result))
            (should-not (file-exists-p result))))
      (delete-directory dir t))))
(provide 'test-elpaish-isolation)
;;; test-elpaish-isolation.el ends here
