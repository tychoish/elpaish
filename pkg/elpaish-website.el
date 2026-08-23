;;; elpaish-website.el --- Static GitHub Pages catalog generation for ELPAish -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maintenance, tools, elpa, package

;;; Commentary:
;; Renders the static HTML pages published alongside the package archives:
;; a per-track package catalog (`elpaish-generate-github-index') and the
;; top-level landing page (`elpaish-generate-top-index').  HTML is built as
;; `dom' sexps and printed with `dom-print'; CSS is built as sexps via
;; `elpaish-css' and rendered to a single inline `<style>' block.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'elpaish)
(require 'elpaish-css)
(require 'elpaish-keyring)

(defconst elpaish-website--fonts-preamble
  '((link ((rel . "preconnect") (href . "https://fonts.googleapis.com")))
    (link ((rel . "preconnect") (href . "https://fonts.gstatic.com") (crossorigin . "")))
    (link ((rel . "stylesheet")
           (href . "https://fonts.googleapis.com/css2?family=Source+Code+Pro:wght@400;600;700&family=Source+Sans+3:wght@400;600;700&display=swap"))))
  "Shared Google Fonts `<link>' tags for `Source Sans 3' and `Source Code Pro'.")

(defcustom elpaish-website-intro-text
  "ELPAish provides continuously built, cryptographically signed Emacs Lisp package archives directly from upstream Git repositories. Archives are organized across three delivery streams: development snapshots, tagged stable releases, and pre-release staging candidates."
  "Optional introductory text rendered on the landing page and catalog headers."
  :type '(choice (const :tag "None" nil) string)
  :group 'elpaish)

(defcustom elpaish-website-repo-url "https://github.com/tychoish/elpaish"
  "Upstream repository URL for ELPAish itself."
  :type '(choice (const :tag "None" nil) string)
  :group 'elpaish)

(defcustom elpaish-website-license-info '("GPL-3.0-or-later" . "https://www.gnu.org/licenses/gpl-3.0.html")
  "Cons pair of (LICENSE-NAME . LICENSE-URL) for the package repository."
  :type '(choice (const :tag "None" nil)
                 (cons (string :tag "License Name") (string :tag "License URL")))
  :group 'elpaish)

(defcustom elpaish-website-extra-sections nil
  "Alist of (HEADING . CONTENT) additional sections to append to the landing page."
  :type '(alist :key-type string :value-type string)
  :group 'elpaish)

(defconst elpaish-website--track-labels
  '((snapshot . "snapshot")
    (stable . "stable")
    (staging . "staging")
    (elpaish . "snapshot")
    (elpaish-stable . "stable")
    (elpaish-staging . "staging"))
  "Human-readable label for each package archive track, used in page titles.")

(defun elpaish-website--track-label (track)
  "Return the human-readable catalog page label for TRACK."
  (or (alist-get (elpaish-canonical-track track) elpaish-website--track-labels)
      (symbol-name track)))

(defun elpaish-website--track-url (track)
  "Return the published catalog URL for TRACK under `elpaish-base-url'."
  (format "%s/%s/" (string-remove-suffix "/" elpaish-base-url) (elpaish-canonical-track track)))

(defun elpaish-website--navbar (&optional relative-prefix)
  "Return a `<nav class=\"navbar\">' dom node.
RELATIVE-PREFIX is prepended to relative links (e.g. \"../\" for track pages)."
  (let ((pfx (or relative-prefix "")))
    `(nav ((class . "navbar"))
          (a ((href . ,(format "%sindex.html" pfx))) "Home")
          (a ((href . ,(format "%ssnapshot/" pfx))) "Snapshot")
          (a ((href . ,(format "%sstable/" pfx))) "Stable")
          (a ((href . ,(format "%sstaging/" pfx))) "Staging")
          (a ((href . ,(format "%sabout.html" pfx))) "About")
          ,@(when elpaish-website-repo-url
              `((a ((href . ,elpaish-website-repo-url) (target . "_blank") (rel . "noopener")) "Repository")))
          ,@(when elpaish-website-license-info
              `((a ((href . ,(cdr elpaish-website-license-info)) (target . "_blank") (rel . "noopener"))
                   ,(format "License: %s" (car elpaish-website-license-info))))))))
;;; Per-track package catalog

(defun elpaish-website--icon-cell (recipe artifact target-dir)
  "Return dom nodes for RECIPE's repository link, doc link, and signature badge."
  (let ((repo-url (elpaish-recipe-url recipe))
        (doc-url (elpaish-recipe-doc recipe))
        (sig-file (expand-file-name (concat artifact ".sig") target-dir)))
    (delq nil
          (list
           (when repo-url
             `(a ((href . ,repo-url) (class . "icon-link") (title . "Repository")
                  (aria-label . "Repository") (target . "_blank") (rel . "noopener"))
                 (i ((class . "nf nf-fa-github")) "")))
           (when doc-url
             `(a ((href . ,doc-url) (class . "icon-link icon-doc") (title . "Documentation")
                  (aria-label . "Documentation") (target . "_blank") (rel . "noopener"))
                 (i ((class . "nf nf-fa-book")) "")))
           (if (file-exists-p sig-file)
               `(a ((href . ,(concat artifact ".sig")) (class . "icon-link") (title . "GPG signature")
                    (aria-label . "GPG signature"))
                   (i ((class . "nf nf-fa-key")) ""))
             `(i ((class . "nf nf-fa-key icon-disabled") (title . "No signature available")) ""))))))

(defun elpaish-website--package-row (recipe track target-dir)
  "Return an HTML `tr' dom node cataloging RECIPE's build on TRACK.
Includes expandable details with extra metadata if available."
  (unless (elpaish-recipe-disabled-for-track-p recipe track)
    (when-let* ((ver-str (elpaish-recipe-version-for-track recipe track)))
      (let* ((name (elpaish-recipe-name recipe))
             (summary (or (elpaish-recipe-summary recipe) "No description"))
             (is-tar (eq (elpaish-recipe-built-type recipe) 'tar))
             (artifact (format "%s-%s.%s" name ver-str (if is-tar "tar" "el")))
             (reqs (elpaish-recipe-requires recipe))
             (keywords (elpaish-recipe-keywords recipe))
             (url (elpaish-recipe-url recipe))
             (doc (elpaish-recipe-doc recipe))
             (files (elpaish-recipe-files recipe)))
        `(tr nil
             (td ((class . "pkg-name"))
                 (b nil ,name)
                 (details ((class . "pkg-details"))
                          (summary ((class . "pkg-summary")) "details")
                          (div ((class . "pkg-extra-info"))
                               (div ((class . "pkg-extra-item"))
                                    (strong nil "Package Type: ") ,(if is-tar "Multi-file (.tar)" "Single-file (.el)"))
                               ,@(when reqs
                                   `((div ((class . "pkg-extra-item"))
                                          (strong nil "Dependencies: ")
                                          (code nil ,(mapconcat (lambda (r) (format "%s (%s)" (car r) (cadr r))) reqs ", ")))))
                               ,@(when keywords
                                   `((div ((class . "pkg-extra-item"))
                                          (strong nil "Keywords: ") ,(mapconcat #'identity keywords ", "))))
                               ,@(when url
                                   `((div ((class . "pkg-extra-item"))
                                          (strong nil "Upstream: ")
                                          (a ((href . ,url) (target . "_blank") (rel . "noopener")) ,url))))
                               ,@(when doc
                                   `((div ((class . "pkg-extra-item"))
                                          (strong nil "Documentation: ")
                                          (a ((href . ,doc) (target . "_blank") (rel . "noopener")) ,doc))))
                               (div ((class . "pkg-extra-item"))
                                    (strong nil "Files: ") (code nil ,(mapconcat #'identity (or files '("*.el")) ", "))))))
             (td ((class . "pkg-version")) (a ((href . ,artifact)) ,ver-str))
             (td ((class . "pkg-desc")) ,summary)
             (td ((class . "pkg-icons")) ,@(elpaish-website--icon-cell recipe artifact target-dir)))))))

(defun elpaish-website--catalog-table (rows)
  "Return the packages `<table>' dom node for ROWS, or a placeholder if empty."
  (if (null rows)
      '(p nil "No packages published in this track.")
    `(div ((class . "table-wrapper"))
          (table ((id . "pkg-table"))
                 (tr nil
                     (th ((class . "pkg-name sortable") (title . "Click to sort by package name"))
                         "Package " (span ((class . "sort-icon")) "↕"))
                     (th ((class . "pkg-version sortable") (title . "Click to sort by version"))
                         "Version " (span ((class . "sort-icon")) "↕"))
                     (th ((class . "pkg-desc sortable") (title . "Click to sort by description"))
                         "Description " (span ((class . "sort-icon")) "↕"))
                     (th ((class . "pkg-icons")) "Links"))
                 ,@rows))))
;;;###autoload
(cl-defun elpaish-generate-github-index (&optional track output-dir title)
  "Generate static `index.html' package catalog for TRACK in OUTPUT-DIR.
TITLE overrides the default page title derived from TRACK."
  (let* ((effective-track (elpaish-canonical-track (or track elpaish-release-mode)))
         (target-dir (or output-dir (elpaish-track-dir effective-track)))
         (page-title (or title (format "ELPAish Repository — (%s)"
                                        (elpaish-website--track-label effective-track))))
         (rows (delq nil
                     (mapcar (lambda (recipe)
                               (elpaish-website--package-row recipe effective-track target-dir))
                             (hash-table-values elpaish-registry)))))
    (make-directory target-dir t)
    (with-temp-file (expand-file-name "index.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (meta ((name . "viewport") (content . "width=device-width, initial-scale=1")))
                    (title nil ,page-title)
                    ,@elpaish-website--fonts-preamble
                    (link ((rel . "stylesheet") (href . "https://www.nerdfonts.com/assets/css/webfont.css")))
                    (style nil ,(elpaish-css-render (elpaish-css-track-index-stylesheet))))
              (body nil
                    ,(elpaish-website--navbar "../")
                    (div ((class . "header"))
                         (h1 nil ,page-title)
                         (p nil "Track URL: " (code nil ,(elpaish-website--track-url effective-track)))
                         (p nil (a ((href . "../")) "← Back to Archive Setup & Overview")))
                    (h2 nil "Packages")
                    ,(elpaish-website--catalog-table rows)
                    (script nil "
document.addEventListener('DOMContentLoaded', function() {
  const table = document.getElementById('pkg-table');
  if (!table) return;
  const headers = table.querySelectorAll('th.sortable');
  headers.forEach((header, index) => {
    let asc = true;
    header.addEventListener('click', () => {
      const rows = Array.from(table.querySelectorAll('tr')).slice(1);
      rows.sort((a, b) => {
        const aCell = a.children[index];
        const bCell = b.children[index];
        const aText = aCell ? aCell.innerText.trim().toLowerCase() : '';
        const bText = bCell ? bCell.innerText.trim().toLowerCase() : '';
        return asc ? aText.localeCompare(bText) : bText.localeCompare(aText);
      });
      asc = !asc;
      rows.forEach(r => table.appendChild(r));
    });
  });
});
")))))
    (expand-file-name "index.html" target-dir)))

;;; Top-level landing page

(defun elpaish-website--track-card (path heading description button-label)
  "Return a `<div class=\"card\">' dom node linking to PATH.
HEADING and BUTTON-LABEL are strings; DESCRIPTION is a list of dom nodes
and/or strings making up the card's paragraph body."
  `(div ((class . "card"))
        (h2 nil (a ((href . ,path)) ,heading))
        (p nil ,@description)
        (a ((class . "btn") (href . ,path)) ,button-label)))

(defconst elpaish-website--track-cards
  '(("snapshot/" "Snapshot (Development)"
     ("Continuous development snapshots built from the default branch head with pure UTC date versioning ("
      (code nil "YYYYMMDD.HHMMSS") ").")
     "Browse Snapshot")
    ("stable/" "Stable (Releases)"
     ("Official release builds strictly from clean semver Git tags ("
      (code nil "vX.Y.Z") "). Repositories without clean tags are omitted.")
     "Browse Stable")
    ("staging/" "Staging (Pre-release)"
     ("Staging release candidates ("
      (code nil "-rc") ", " (code nil "-pre") ") and " (code nil "git describe")
      " builds for integration testing.")
     "Browse Staging"))
  "PATH, HEADING, DESCRIPTION, and BUTTON-LABEL for each track's landing card.")

(defun elpaish-website--package-archives-snippet ()
  "Return the `package-archives' configuration snippet shown on the landing page."
  (format (concat ";; Primary development snapshot track:\n"
                   "(add-to-list 'package-archives '(\"elpaish-snapshot\" . \"%s\") t)\n\n"
                   ";; Production stable release track:\n"
                   "(add-to-list 'package-archives '(\"elpaish\" . \"%s\") t)\n\n"
                   ";; Pre-release / staging track:\n"
                   "(add-to-list 'package-archives '(\"elpaish-staging\" . \"%s\") t)")
          (elpaish-website--track-url 'snapshot)
          (elpaish-website--track-url 'stable)
          (elpaish-website--track-url 'staging)))
(defun elpaish-website--keyring-import-snippet ()
  "Return the GPG keyring import snippet shown on the landing page."
  (let ((pub-key-url (format "%s/elpaish.pub.asc" (string-remove-suffix "/" elpaish-base-url))))
    (format (concat "# Download and import armored public key into GPG keyring:\n"
                     "curl -sSL %s | gpg --import\n\n"
                     "# Or download key file directly:\n"
                     "curl -O %s\n"
                     "gpg --import < elpaish.pub.asc")
            pub-key-url pub-key-url)))

(defun elpaish-website--package-keyring-snippet ()
  "Return the `package-import-keyring' snippet shown on the landing page.
Distinct from `elpaish-website--keyring-import-snippet': `package-install'
checks signatures against Emacs's own `package-gnupghome-dir' keyring, not
the user's regular GPG keyring, so the key needs to be imported there too
for `package-check-signature' to actually verify anything."
  (let ((binary-key-url (format "%s/elpaish-keyring.gpg" (string-remove-suffix "/" elpaish-base-url))))
    (format (concat ";; Download the binary keyring and import it into `package.el's own keyring,\n"
                     ";; then enable signature checking:\n"
                     "(url-copy-file \"%s\" \"elpaish-keyring.gpg\" t)\n"
                     "(package-import-keyring \"elpaish-keyring.gpg\")\n"
                     "(setq package-check-signature t)")
            binary-key-url)))

;;;###autoload
(defun elpaish-generate-about-page (&optional output-dir)
  "Generate static `about.html' page in OUTPUT-DIR."
  (let ((target-dir (or output-dir elpaish-output-dir)))
    (make-directory target-dir t)
    (with-temp-file (expand-file-name "about.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (meta ((name . "viewport") (content . "width=device-width, initial-scale=1")))
                    (title nil "About ELPAish Package Archives")
                    ,@elpaish-website--fonts-preamble
                    (style nil ,(elpaish-css-render (elpaish-css-top-index-stylesheet))))
              (body nil
                    ,(elpaish-website--navbar "")
                    (h1 nil "About ELPAish")
                    (div ((class . "doc-section"))
                         (h2 nil "Overview & Architecture")
                         (p nil "ELPAish is a toolkit for building (and a prototype application of) an Emacs Lisp package archive/repository. It manages recipe discovery, stream packaging, GPG subkey cryptographic signing, and static website publication without external build dependencies.")
                         (h2 nil "Repository Streams")
                         (ul nil
                             (li nil (strong nil "Snapshot: ") "Automated builds from the default branch head with pure UTC date versioning (" (code nil "YYYYMMDD.HHMMSS") ").")
                             (li nil (strong nil "Stable: ") "Official releases derived strictly from clean semver Git tags (" (code nil "vX.Y.Z") "). Packages without stable tags are omitted.")
                             (li nil (strong nil "Staging: ") "Pre-release and release candidate builds for early testing and integration validation."))
                         (h2 nil "Security & Verification")
                         (p nil "Every package archive artifact and catalog index is signed with an isolated GPG subkey. Consumers can verify package integrity and authenticity using Emacs's built-in " (code nil "package.el") " keyring verification.")
                         ,@(when elpaish-website-license-info
                             `((h2 nil "License")
                               (p nil "This repository and its packages are distributed under "
                                  (a ((href . ,(cdr elpaish-website-license-info)) (target . "_blank") (rel . "noopener"))
                                     ,(car elpaish-website-license-info)) "."))))
                    (div ((class . "meta-footer"))
                         (p nil "Generated by ELPAish — "
                            (a ((href . "index.html")) "Back to Home")))))))
    (expand-file-name "about.html" target-dir)))

;;;###autoload
(defun elpaish-generate-top-index (&optional output-dir)
  "Generate top-level static `index.html' landing page and about page in OUTPUT-DIR."
  (let ((target-dir (or output-dir elpaish-output-dir)))
    (make-directory target-dir t)
    (elpaish-generate-about-page target-dir)
    (with-temp-file (expand-file-name "index.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (meta ((name . "viewport") (content . "width=device-width, initial-scale=1")))
                    (title nil "ELPAish: tychoish Emacs Lisp Package Archives")
                    ,@elpaish-website--fonts-preamble
                    (style nil ,(elpaish-css-render (elpaish-css-top-index-stylesheet))))
              (body nil
                    ,(elpaish-website--navbar "")
                    (h1 nil "ELPAish Emacs Package Archives")
                    ,@(when elpaish-website-intro-text
                        `((div ((class . "intro-box"))
                               (p nil ,elpaish-website-intro-text))))
                    (div ((class . "track-grid"))
                         ,@(mapcar (lambda (card) (apply #'elpaish-website--track-card card))
                                   elpaish-website--track-cards))
                    (h2 nil "Emacs Configuration")
                    (p nil "Add your preferred track to " (code nil "package-archives") " in your "
                       (code nil "init.el") ":")
                    (pre nil (code nil ,(elpaish-website--package-archives-snippet)))
                    (h2 nil "GPG Keyring Verification")
                    (p nil "Packages and index files are GPG signed. Download and import the public key into your GPG keyring:")
                    (pre nil (code nil ,(elpaish-website--keyring-import-snippet)))
                    (p nil "To have "
                       (code nil "package-install")
                       " itself verify signatures, import the key into "
                       (code nil "package.el")
                       "'s own keyring instead (a separate trust store from your regular GPG keyring) and turn on "
                       (code nil "package-check-signature")
                       ":")
                    (pre nil (code nil ,(elpaish-website--package-keyring-snippet)))
                    (h2 nil "Keyring and Certificate Assets")
                    (ul nil
                        (li nil (a ((href . "elpaish-keyring.gpg")) "elpaish-keyring.gpg") " — Binary public keyring")
                        (li nil (a ((href . "elpaish.pub.asc")) "elpaish.pub.asc") " — Armored ASCII public key")
                        (li nil (a ((href . "elpaish.rev.asc")) "elpaish.rev.asc") " — Published revocation certificates (if any)"))
                    ,@(when elpaish-website-extra-sections
                        (mapcar (lambda (sec)
                                  `(div ((class . "doc-section"))
                                        (h2 nil ,(car sec))
                                        (p nil ,(cdr sec))))
                                elpaish-website-extra-sections))
                    (div ((class . "meta-footer"))
                         (p nil "ELPAish package repository — "
                            (a ((href . "about.html")) "About & Architecture")
                            ,@(when elpaish-website-repo-url
                                `(" | " (a ((href . ,elpaish-website-repo-url) (target . "_blank") (rel . "noopener")) "GitHub Repository")))
                            ,@(when elpaish-website-license-info
                                `(" | " (a ((href . ,(cdr elpaish-website-license-info)) (target . "_blank") (rel . "noopener"))
                                           ,(format "License: %s" (car elpaish-website-license-info)))))))))))
    (expand-file-name "index.html" target-dir)))
(provide 'elpaish-website)
;;; elpaish-website.el ends here
