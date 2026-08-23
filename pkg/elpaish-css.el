;;; elpaish-css.el --- CSS-as-sexp stylesheets for ELPAish generated pages -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maintenance, tools, elpa, package

;;; Commentary:
;; Represents the CSS used by the generated GitHub Pages catalog as plain
;; Lisp data (mirroring how `elpaish-website.el' represents HTML as `dom'
;; sexps) instead of as opaque string literals, so shared values like colors
;; and font stacks live in one place and rules can be composed/inspected
;; like any other Lisp data.
;;
;; A stylesheet is a list of rules.  Each rule is (SELECTOR . DECLARATIONS),
;; where DECLARATIONS is an alist of (PROPERTY . VALUE) strings, e.g.:
;;
;;   ((body (font-family . "sans-serif") (margin . "0"))
;;    (".card" (border . "1px solid #c6c6c6")))
;;
;; `elpaish-css-render' turns a stylesheet into a single minified CSS string
;; suitable for inlining into a `(style nil ...)' dom node.

;;; Code:

(require 'modus-themes)
(require 'seq)

;;; Shared design tokens

(defconst elpaish-css-font-sans
  "'Source Sans 3','Source Sans Pro',-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif"
  "Default body font stack for generated ELPAish pages.")

(defconst elpaish-css-font-mono
  "'Source Code Pro',ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace"
  "Monospace font stack used for versions, code, and package archive URLs.")

(defun elpaish-css-color (key &optional theme)
  "Return rendered hex-color string for KEY directly from `modus-themes' THEME (default `modus-operandi')."
  (let ((th (or theme 'modus-operandi)))
    (if (fboundp 'modus-themes-get-color-value)
        (modus-themes-get-color-value key th)
      (let ((palette (or (and (boundp 'modus-operandi-palette) (symbol-value 'modus-operandi-palette))
                         (and (fboundp 'modus-themes--palette-value) (modus-themes--palette-value th)))))
        (cadr (assq key palette))))))

(defconst elpaish-css-color-fg
  (elpaish-css-color 'fg-main)
  "Primary foreground/text color derived from `modus-themes'.")

(defconst elpaish-css-color-bg
  (elpaish-css-color 'bg-main)
  "Page background color derived from `modus-themes'.")

(defconst elpaish-css-color-border
  (elpaish-css-color 'border)
  "Default border color derived from `modus-themes'.")

(defconst elpaish-css-color-border-strong
  (elpaish-css-color 'fg-dim)
  "Emphasized border/rule color derived from `modus-themes'.")

(defconst elpaish-css-color-border-soft
  (elpaish-css-color 'bg-inactive)
  "Subtle row/cell border color derived from `modus-themes'.")

(defconst elpaish-css-color-panel-bg
  (elpaish-css-color 'bg-dim)
  "Background for cards and code blocks derived from `modus-themes'.")

(defconst elpaish-css-color-th-bg
  (elpaish-css-color 'bg-inactive)
  "Table header background derived from `modus-themes'.")

(defconst elpaish-css-color-row-hover
  (elpaish-css-color 'bg-hover-secondary)
  "Table row hover background derived from `modus-themes'.")

(defconst elpaish-css-color-icon
  (elpaish-css-color 'fg-dim)
  "Default icon-link color derived from `modus-themes'.")

(defconst elpaish-css-color-icon-disabled
  (elpaish-css-color 'border)
  "Disabled icon color derived from `modus-themes'.")

(defconst elpaish-css-color-link
  (elpaish-css-color 'fg-link)
  "Default link and heading-accent color derived from `modus-themes'.")

(defconst elpaish-css-color-link-hover
  (elpaish-css-color 'magenta)
  "Link hover color derived from `modus-themes'.")

(defconst elpaish-css-color-code-bg
  (elpaish-css-color 'bg-dim)
  "Inline `code' background derived from `modus-themes'.")

(defconst elpaish-css-color-code-fg
  (elpaish-css-color 'magenta-cooler)
  "Inline `code' text color derived from `modus-themes'.")

(defconst elpaish-css-color-heading-accent
  (elpaish-css-color 'blue-cooler)
  "Card heading accent color derived from `modus-themes'.")

(defconst elpaish-css-color-btn-bg
  (elpaish-css-color 'blue-warmer)
  "Primary button background derived from `modus-themes'.")

(defconst elpaish-css-color-btn-bg-hover
  (elpaish-css-color 'blue-intense)
  "Primary button hover background derived from `modus-themes'.")

(defun elpaish-css-render (stylesheet)
  "Render STYLESHEET (a list of (SELECTOR . DECLARATIONS) rules) to a CSS string.
DECLARATIONS is an alist of (PROPERTY . VALUE) strings."
  (mapconcat
   (lambda (rule)
     (let ((selector (car rule))
           (decls (cdr rule)))
       (format "%s{%s}"
               (if (symbolp selector) (symbol-name selector) selector)
               (mapconcat (lambda (decl) (format "%s:%s;" (car decl) (cdr decl)))
                          decls ""))))
   stylesheet ""))

;;; Shared base rules

(defun elpaish-css--base-rules ()
  "Return the stylesheet rules shared by every generated ELPAish page."
  `((body (font-family . ,elpaish-css-font-sans)
          (margin . "36px auto")
          (max-width . "1240px")
          (font-size . "18px")
          (line-height . "1.6")
          (color . ,elpaish-css-color-fg)
          (background . ,elpaish-css-color-bg)
          (padding . "0 28px"))
    (h1 (font-size . "2.2em")
        (font-weight . "700")
        (color . ,elpaish-css-color-fg)
        (margin-top . "0")
        (margin-bottom . "12px"))
    (h2 (font-size . "1.6em")
        (font-weight . "700")
        (color . ,elpaish-css-color-fg)
        (margin-top . "36px")
        (margin-bottom . "16px"))
    (a (color . ,elpaish-css-color-link)
       (text-decoration . "none")
       (font-weight . "600"))
    ("a:hover" (color . ,elpaish-css-color-link-hover)
     (text-decoration . "underline"))
    (code (font-family . ,elpaish-css-font-mono)
          (font-size . "0.92em")
          (background . ,elpaish-css-color-code-bg)
          (color . ,elpaish-css-color-code-fg)
          (padding . "3px 8px")
          (border-radius . "4px")
          (border . ,(format "1px solid %s" elpaish-css-color-border-soft)))))

;;; Track index page (per-track package catalog table)

(defun elpaish-css-track-index-stylesheet ()
  "Return the stylesheet rules for a per-track package catalog page."
  (append
   (elpaish-css--base-rules)
   `((h1 (border-bottom . ,(format "2px solid %s" elpaish-css-color-border-strong))
         (padding-bottom . "14px"))
     (".navbar" (display . "flex")
      (gap . "16px")
      (margin-bottom . "20px")
      (padding-bottom . "12px")
      (border-bottom . ,(format "1px solid %s" elpaish-css-color-border-soft))
      (font-size . "0.95em"))
     (".navbar a" (color . ,elpaish-css-color-link)
      (font-weight . "600"))
     (".header" (margin-bottom . "28px")
      (border-bottom . ,(format "2px solid %s" elpaish-css-color-border-strong))
      (padding-bottom . "18px"))
     (".table-wrapper" (width . "100%")
      (overflow-x . "auto")
      (-webkit-overflow-scrolling . "touch")
      (margin-top . "24px"))
     (table (border-collapse . "collapse")
            (width . "100%")
            (min-width . "1020px")
            (font-size . "1em")
            (border . ,(format "1px solid %s" elpaish-css-color-border)))
     ("th,td" (padding . "14px 20px")
      (border-bottom . ,(format "1px solid %s" elpaish-css-color-border-soft))
      (text-align . "left")
      (vertical-align . "top"))
     (th (background . ,elpaish-css-color-th-bg)
         (color . ,elpaish-css-color-fg)
         (font-weight . "700")
         (font-size . "1.05em")
         (border-bottom . ,(format "2px solid %s" elpaish-css-color-border-strong)))
     ("th.sortable" (cursor . "pointer")
      (user-select . "none"))
     ("th.sortable:hover" (background . ,elpaish-css-color-row-hover))
     (".sort-icon" (font-size . "0.85em")
      (margin-left . "6px")
      (opacity . "0.7"))
     ("tr:hover" (background . ,elpaish-css-color-row-hover))
     (".pkg-name" (font-weight . "700")
      (font-size . "1.05em")
      (white-space . "nowrap!important")
      (min-width . "320px")
      (width . "320px"))
     (".pkg-name b" (white-space . "nowrap!important")
      (word-break . "keep-all")
      (font-weight . "700"))
     (".pkg-version" (font-family . ,elpaish-css-font-sans)
      (font-size . "0.95em")
      (white-space . "nowrap!important")
      (min-width . "200px")
      (width . "200px"))
     (".pkg-desc" (min-width . "440px")
      (font-size . "1em"))
     (".pkg-icons" (white-space . "nowrap!important")
      (text-align . "center")
      (width . "110px")
      (font-size . "1.2em"))
     (".pkg-icons .icon-link" (display . "inline-block")
      (margin . "0 6px")
      (color . ,elpaish-css-color-icon)
      (text-decoration . "none"))
     (".pkg-icons .icon-link:hover" (color . ,elpaish-css-color-link)
      (text-decoration . "none"))
     (".pkg-icons .icon-disabled" (display . "inline-block")
      (margin . "0 6px")
      (color . ,elpaish-css-color-icon-disabled))
     ("details.pkg-details" (margin-top . "6px")
      (font-size . "0.92em")
      (color . ,elpaish-css-color-border-strong))
     ("summary.pkg-summary" (cursor . "pointer")
      (font-weight . "600")
      (color . ,elpaish-css-color-link)
      (outline . "none"))
     (".pkg-extra-info" (margin-top . "8px")
      (padding . "10px 14px")
      (background . ,elpaish-css-color-panel-bg)
      (border . ,(format "1px solid %s" elpaish-css-color-border-soft))
      (border-radius . "4px"))
     (".pkg-extra-item" (margin . "4px 0")))))
;;; Top-level landing page (track selection cards)

(defun elpaish-css-top-index-stylesheet ()
  "Return the stylesheet rules for the top-level ELPAish landing page."
  (append
   (elpaish-css--base-rules)
   `((h1 (border-bottom . ,(format "2px solid %s" elpaish-css-color-border-strong))
         (padding-bottom . "14px"))
     (".navbar" (display . "flex")
      (gap . "16px")
      (margin-bottom . "20px")
      (padding-bottom . "12px")
      (border-bottom . ,(format "1px solid %s" elpaish-css-color-border-soft))
      (font-size . "0.95em"))
     (".navbar a" (color . ,elpaish-css-color-link)
      (font-weight . "600"))
     (p (font-size . "1.05em")
        (margin . "10px 0"))
     (".intro-box" (padding . "18px 24px")
      (background . ,elpaish-css-color-panel-bg)
      (border . ,(format "1px solid %s" elpaish-css-color-border))
      (border-radius . "8px")
      (margin . "20px 0 28px 0"))
     (".track-grid" (display . "grid")
      (grid-template-columns . "repeat(auto-fit,minmax(340px,1fr))")
      (gap . "24px")
      (margin . "28px 0"))
     (".card" (border . ,(format "1px solid %s" elpaish-css-color-border))
      (border-radius . "8px")
      (padding . "24px")
      (background . ,elpaish-css-color-panel-bg)
      (box-shadow . "0 2px 6px rgba(0,0,0,0.06)"))
     (".card h2" (font-size . "1.45em")
      (font-weight . "700")
      (margin-top . "0")
      (margin-bottom . "12px")
      (color . ,elpaish-css-color-heading-accent))
     (".card p" (font-size . "1em")
      (line-height . "1.6"))
     (pre (font-family . ,elpaish-css-font-mono)
          (font-size . "0.95em")
          (background . ,elpaish-css-color-panel-bg)
          (color . ,elpaish-css-color-fg)
          (border . ,(format "1px solid %s" elpaish-css-color-border))
          (padding . "18px 24px")
          (border-radius . "6px")
          (overflow-x . "auto")
          (line-height . "1.5"))
     ("pre code" (background . "transparent")
      (color . "inherit")
      (padding . "0")
      (border . "none")
      (font-size . "1em"))
     (".btn" (display . "inline-block")
      (padding . "10px 22px")
      (font-size . "1.02em")
      (font-weight . "700")
      (background . ,elpaish-css-color-btn-bg)
      (color . ,elpaish-css-color-bg)
      (border-radius . "5px")
      (text-decoration . "none")
      (margin-top . "10px"))
     (".btn:hover" (background . ,elpaish-css-color-btn-bg-hover)
      (color . ,elpaish-css-color-bg)
      (text-decoration . "none"))
     (".doc-section" (margin . "28px 0")
      (padding . "20px 24px")
      (background . ,elpaish-css-color-panel-bg)
      (border . ,(format "1px solid %s" elpaish-css-color-border-soft))
      (border-radius . "6px"))
     (".meta-footer" (margin-top . "48px")
      (padding-top . "20px")
      (border-top . ,(format "1px solid %s" elpaish-css-color-border-soft))
      (font-size . "0.9em")
      (color . ,elpaish-css-color-border-strong))
     (ul (font-size . "1.05em")
         (padding-left . "28px"))
     (li (margin . "8px 0")))))
(provide 'elpaish-css)
;;; elpaish-css.el ends here
