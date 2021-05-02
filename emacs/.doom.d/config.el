;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Denis Kipchakbaev"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Source Code Pro" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; Enable Menu bar
(menu-bar-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq org-journal-dir "~/Dropbox/org/journal/")

  ;; private snippets directory
(setq yas-snippet-dirs
      '("~/dev/emacs-private/snippets"           ;; personal snippets
))

;; Add a default input method for C-\ to avoid going through the list each time
(setq default-input-method "russian-computer")

;; Add ruler for any programming mode (a vertical line at column 80 by default)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(defun kipcd/fill-column-120 ()
  "Configure display-fill-column-indicator-mode to display a ruler at column 120"
  (interactive)
  (setq display-fill-column-indicator-column 120))

;; Set ruler at column 120 for java-mode
(add-hook 'java-mode-hook 'kipcd/fill-column-120)

;; "-P" "pager=off" - disable pager for psql output (normally it's =less=,
;; could be set to =more=).
;; Prevents a warning “Terminal is not fully functional”
(custom-set-variables
        '(sql-postgres-options (quote ("-P" "pager=off"))))

(defun custom-sql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))

;; Load SQL connections
(load "~/.doom.d/wed-sql.el")

;; (setq
;;  projectile-project-search-path '("~/wed/data/ws/"))

;; LSP
;; Set lsp-log-io to t to inspect communication between client and the server.
;; Use lsp-workspace-show-log to switch to the corresponding log buffer.
(setq lsp-log-io 't)
;; java VM arguments
(setq lsp-java-vmargs
      '("-noverify"
        "-javaagent:/home/denis/.m2/repository/org/projectlombok/lombok/1.18.12/lombok-1.18.12.jar"
))

;; Use spaces instead of TABs for indentation
(setq-default indent-tabs-mode nil)

(setq auth-sources '("~/.authinfo"))
