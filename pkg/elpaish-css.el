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

(require 'seq)

;;; Shared design tokens

(defconst elpaish-css-font-sans
  "'Source Sans 3','Source Sans Pro',-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif"
  "Default body font stack for generated ELPAish pages.")

(defconst elpaish-css-font-mono
  "'Source Code Pro',ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace"
  "Monospace font stack used for versions, code, and package archive URLs.")

(defconst elpaish-css-color-fg "#000000" "Primary foreground/text color.")
(defconst elpaish-css-color-bg "#ffffff" "Page background color.")
(defconst elpaish-css-color-border "#c6c6c6" "Default border color.")
(defconst elpaish-css-color-border-strong "#707070" "Emphasized border/rule color.")
(defconst elpaish-css-color-border-soft "#d7d7d7" "Subtle row/cell border color.")
(defconst elpaish-css-color-panel-bg "#f8f8f8" "Background for cards and code blocks.")
(defconst elpaish-css-color-th-bg "#e5e5e5" "Table header background.")
(defconst elpaish-css-color-row-hover "#eef2f8" "Table row hover background.")
(defconst elpaish-css-color-icon "#333333" "Default icon-link color.")
(defconst elpaish-css-color-icon-disabled "#c6c6c6" "Disabled icon color.")
(defconst elpaish-css-color-link "#0000aa" "Default link and heading-accent color.")
(defconst elpaish-css-color-link-hover "#721045" "Link hover color.")
(defconst elpaish-css-color-code-bg "#f2f2f2" "Inline `code' background.")
(defconst elpaish-css-color-code-fg "#5317ac" "Inline `code' text color.")
(defconst elpaish-css-color-heading-accent "#002f5e" "Card heading accent color.")
(defconst elpaish-css-color-btn-bg "#00538b" "Primary button background.")
(defconst elpaish-css-color-btn-bg-hover "#003494" "Primary button hover background.")

;;; Renderer

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
      (vertical-align . "middle"))
     (th (background . ,elpaish-css-color-th-bg)
         (color . ,elpaish-css-color-fg)
         (font-weight . "700")
         (font-size . "1.05em")
         (border-bottom . ,(format "2px solid %s" elpaish-css-color-border-strong)))
     ("tr:hover" (background . ,elpaish-css-color-row-hover))
     (".pkg-name" (font-weight . "700")
      (font-size . "1.05em")
      (white-space . "nowrap!important")
      (min-width . "320px")
      (width . "320px"))
     (".pkg-name b" (white-space . "nowrap!important")
      (word-break . "keep-all")
      (font-weight . "700"))
     (".pkg-version" (font-family . ,elpaish-css-font-mono)
      (font-size . "0.95em")
      (white-space . "nowrap!important")
      (min-width . "200px")
      (width . "200px"))
     (".pkg-desc" (min-width . "440px")
      (font-size . "1em"))
     (".pkg-icons" (white-space . "nowrap!important")
      (text-align . "center")
      (width . "90px")
      (font-size . "1.2em"))
     (".pkg-icons .icon-link" (display . "inline-block")
      (margin . "0 8px")
      (color . ,elpaish-css-color-icon)
      (text-decoration . "none"))
     (".pkg-icons .icon-link:hover" (color . ,elpaish-css-color-link)
      (text-decoration . "none"))
     (".pkg-icons .icon-disabled" (display . "inline-block")
      (margin . "0 8px")
      (color . ,elpaish-css-color-icon-disabled)))))

;;; Top-level landing page (track selection cards)

(defun elpaish-css-top-index-stylesheet ()
  "Return the stylesheet rules for the top-level ELPAish landing page."
  (append
   (elpaish-css--base-rules)
   `((h1 (border-bottom . ,(format "2px solid %s" elpaish-css-color-border-strong))
         (padding-bottom . "14px"))
     (p (font-size . "1.05em")
        (margin . "10px 0"))
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
     (ul (font-size . "1.05em")
         (padding-left . "28px"))
     (li (margin . "8px 0")))))

(provide 'elpaish-css)
;;; elpaish-css.el ends here
