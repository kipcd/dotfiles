;; My vanilla Emacs config

;; Performance tweaks
;; Following lsp-mode performance guidelines

;; Speed up the initialization reducing garbage collection runs.
(setq gc-cons-threshold 100000000)
;; Increase the amount of data which Emacs reads from the process.
(setq read-process-output-max (* 1024 1024 3)) ;; 3mb

(setq inhibit-startup-message t) ;; Hide welcome buffer
(scroll-bar-mode -1)             ;; Disable visible scrollbar
(tool-bar-mode -1)               ;; Disable the toolbar
(tooltip-mode -1)                ;; Disable tooltips (pop-up help text for buttons and menu-items). When disabled shows tooltips in echo area
(set-fringe-mode 10)             ;; Set right and left edge fringes (empty borders) in px
(menu-bar-mode -1)               ;; Disable menu bar

(setq visible-bell t)            ;; Set up visible bell

;; Answer 'yes' or 'no' questions with 'y' or 'n'
(defalias 'yes-or-no-p #'y-or-n-p)

;; Don't warn about active processes on exit
(setq confirm-kill-processes nil)

(setq initial-scratch-message nil)

;; if there is a Dired buffer displayed in the next window, use its current directory as a target for copy, move, etc.
(setq dired-dwim-target t)

;; Borrowed from https://github.com/rrudakov/dotfiles/blob/master/emacs.d/emacs.org
(defun k/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to clipboard." filename))))

;; Set font and font size (130/100 = 13pt)
;; (set-face-attribute 'default nil :font "Source Code Pro" :height 110)
;; (set-face-attribute 'default nil :font "Iosevka" :height 110)

;; Initialize package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(setq package-archive-priorities
      '(("gnu" . 10)
        ("nongnu" . 5)
        ("melpa" . 0)))
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; refresh package contents if not present (i.e. first start)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; ensure guarantees that use-package will always download a package for you on first run
(setq use-package-always-ensure t)

;; A bunch of old-school color themes
;; (use-package color-theme-modern)
;; (load-theme 'gnome2 t) ;; very nice old-school slate theme

;; (use-package humanoid-themes)
(use-package dracula-theme
  :config
  ;; 't' avoids prompting for loading theme each time
  (load-theme 'dracula t))
;; (use-package zenburn-theme
;;   :config
;;   ;; use variable-pitch fonts for some headings and titles
;;   (setq zenburn-use-variable-pitch t)
;;   ;; scale headings in org-mode
;;   (setq zenburn-scale-org-headlines t)
;;   ;; scale headings in outline-mode
;;   (setq zenburn-scale-outline-headlines t)
;;   )
;; (load-theme 'zenburn t)

;; (use-package monokai-pro-theme
;;   :ensure t
;;   :config
;;   (load-theme 'monokai-pro-machine t))

;; (use-package subatomic-theme
;;   :config
;;   (load-theme 'subatomic t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  )

;; Fix the error provoked by long file paths:
;; after-find-file: Getting attributes: File name too long, /home/denis/.emacs.default/var/auto-save/#!...
(defun my-shorten-auto-save-file-name (&rest args)
  (let ((buffer-file-name
         (when buffer-file-name (sha1 buffer-file-name))))
    (apply args)))

(advice-add 'make-auto-save-file-name :around
            #'my-shorten-auto-save-file-name)

(use-package recentf
  :after no-littering
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  ;; disable recentf-cleanup on Emacs start, because it can cause
  ;; problems with remote files
  ;; recentf-auto-cleanup 'never)
  (recentf-mode 1))

;; Remember point position in a file between sessions
(use-package saveplace
  :init (save-place-mode)
  ;; load after `no-littering' because it moves save-place.el file to var dir
  :after no-littering
)

(require 'kipcd-core (expand-file-name "kipcd-core/kipcd-core" user-emacs-directory))

;; VTerm terminal mode
(use-package vterm)

;; Follow compilation buffer output automatically
(setq compilation-scroll-output t)

;; Support for ANSI escape color codes in emacs compilation buffer
;; Fixes build and test execution output in LSP and DAP
(defun colorize-compilation-buffer ()
  (when (derived-mode-p 'compilation-mode)
    (ansi-color-process-output nil)
    (setq-local comint-last-output-start (point-marker))))
(use-package ansi-color
  :config
  (add-hook 'compilation-filter-hook
            #'colorize-compilation-buffer))

;; install command-log-mode - a buffer with a log of executed commands
(use-package command-log-mode)

;; Log of interactions with Emacs.
;; Enable with interaction-log-mode.
;; The log is in **Emacs Log** buffer
(use-package interaction-log)

;; Enhanced M-x - provides sorted command history
;; Used by counsel-M-x automatically
(use-package smex)

(use-package counsel
  :bind
  (:map minibuffer-local-map
        ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1)
  ;; Enable selecting of what you are typing in prompt with C-n / C-p.
  ;; Helpful when trying to create a file in `counsel-find-file' with a name which is a subset of existing file name.
  (setq ivy-use-selectable-prompt t)
  ;; Remove ^ from M-x input - ^ means match only from beginning of the string
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

(use-package ivy
  ;;  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Show function descriptions and keybindings in counsel M-x and other buffers
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Required by doom-modeline
;; After first install run ~all-the-icons-install-fonts~
(use-package all-the-icons)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/dev/org/")

;; Show column number in modeline
(column-number-mode)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(;;org-mode-hook
                org-agenda-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                compilation-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Automatically insert balanced brackets
(electric-pair-mode 1)

;; Enhance describe
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 8)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Markdown mode. Use gfm-mode - Github-flavored markdown
;; Idea is taken from https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  ;; pandoc should be installed in the system
  (setq markdown-command "pandoc -t html5"))

;; HTTP server used for live markdown preview
(use-package simple-httpd
  :config
  (setq httpd-port 7070)
  (setq httpd-host (system-name)))

(use-package impatient-mode
  :commands impatient-mode)

;; A filter function to process the Markdown buffer
;; The function uses the markdown command from markdown-mode to generate HTML content.
;; To mimic the look of GitHub, it uses the CSS from github-markdown-css.
(defun kipcd/github-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

;; A function to show preview.
;; `impatient-mode' takes the content of your buffer, passes it through a filter
;; and serves the result via simple-httpd HTTP server.
(defun kipcd/live-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'kipcd/github-markdown-filter)
  (imp-visit-buffer))

;; Fast search. Required by projectile-ripgrep
(use-package ripgrep)

(use-package projectile
  :config
  (projectile-mode)
  ;; Override compile and test commands for Maven projects
  (projectile-register-project-type 'maven '("pom.xml")
                                  :project-file "pom.xml"
                                  :compile "mvn -T 4 clean package"
                                  :test "mvn clean verify"
                                  :test-suffix "Test"
                                  :src-dir "main/src/"
                                  :test-dir "main/test/")

  :custom
  (projectile-completion-system 'ivy)
  :init
  ;; (when (file-directory-p "~/wed/data/ws")
  ;;   (setq projectile-project-search-path '("~/wed/data/ws")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package git-gutter
  :defer t
  :hook ((markdown-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode)
         (conf-mode . git-gutter-mode))
  :config
  (setq git-gutter:disabled-modes '(org-mode asm-mode image-mode)
        git-gutter:update-interval 1
        git-gutter:window-width 2
        git-gutter:ask-p nil))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))

;; Update `vc-state' for each opened buffer.
;; It will affect all modes which relies on `vc-mode' state (for example `vc-status' in the modeline and `diff-hl')
;; Borrowed from https://github.com/rrudakov/dotfiles/blob/master/emacs.d/emacs.org
(defun k/refresh-vc-state ()
  "Refresh `vc-state' on all buffers."
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (vc-mode)
        (vc-refresh-state)))))

(use-package magit
  :config
  ;; Update git status in all buffers
  (add-hook 'magit-post-refresh-hook #'k/refresh-vc-state 5)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Magit integration with GitHub API
;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(defun kipcd/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)  ;; line wrapping (visually break the line when it exceeds the screen size)
  )

(use-package org
  :hook (org-mode . kipcd/org-mode-setup)
  :config
  (setq org-log-done 'time) ;; Write time on completing TODO task
  (setq org-agenda-time-grid '((daily today require-timed)
                              (530 800 1000 1200 1400 1600 1800 2000 2130)
                              "......" "----------------"))
  (setq org-agenda-start-with-log-mode t) ;; Show completed tasks in timetable
  (setq org-agenda-start-on-weekday nil) ;; Unset the value to not start agenda on Monday
  (setq org-agenda-start-day "-1d") ;; Start agenda from yesterday
  (setq org-agenda-span 8) ;; Show 8 days in agenda
  (setq org-clock-into-drawer t)
  (setq org-agenda-files
        '("~/dev/org/2.emacs.org"
          "~/dev/org/habits.org"
          "~/dev/org/8.birthdays.org"
          "~/dev/org/1.TODO.org"
          "~/dev/org/3.linux.org"
          "~/dev/org/5.phone.org"
          "~/dev/org/6.health.org"
          "~/dev/org/7.cycling.org"))
  (setq org-refile-targets
        '(("9.archive.org" :maxlevel . 2) ;; maxlevel sets how deep in subtree can go
          ("1.TODO.org" :maxlevel . 1)))
  (setq org-agenda-include-diary t)
  (setq diary-file "~/dev/org/agenda-diary.org")
  (setq calendar-location-name "Barcelona, Spain")
  (setq calendar-latitude 41.383333)
  (setq calendar-longitude 2.183333)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-habit-show-all-today t)

  ;; Save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; Save org buffers after changing TODO state in agenda
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  ;; Save org buffers for repeating scheduled tasks
  (advice-add 'org-store-log-note :after 'org-save-all-org-buffers)
  ;; Check if need this also
  ;; (advice-add 'org-deadline :after 'org-save-all-org-buffers)
  ;; (advice-add 'org-schedule :after 'org-save-all-org-buffers)
  (setq org-capture-templates
        `(
          ("t" "Task" entry (file+olp "1.TODO.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "journal.org")
           "\n* %<%I:%M %p>\n%?\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :empty-lines 1)

          ("w" "Work")
          ("wm" "Meeting" entry
           (file+olp+datetree "4.weekendesk.org" "Meetings")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
          ("mc" "Cycling" table-line (file+headline "metrics.org" "Cycling")
           "| %U | %^{Distance} | %^{Notes} |" :kill-buffer t)))

  ;; required for expanding structure templates with <KEY TAB
  (require 'org-tempo)

  ;; expand a template with <KEY TAB, i.e. <sh TAB
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  )

(defun diary-sunrise ()
  (let ((dss (diary-sunrise-sunset)))
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ",")
      (buffer-substring (point-min) (match-beginning 0)))))

(defun diary-sunset ()
  (let ((dss (diary-sunrise-sunset))
        start end)
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ", ")
      (setq start (match-end 0))
      (search-forward " at")
      (setq end (match-beginning 0))
      (goto-char start)
      (capitalize-word 1)
      (buffer-substring start end))))

;; (use-package org-super-agenda
;;   :config
;;   (org-super-agenda-mode)
;;   (setq org-super-agenda-groups
;;          '(;; Each group has an implicit boolean OR operator between its selectors.
;;            ;; Discard duplicated items that have been scheduled for today and marked as DONE
;;            ;; (:discard (:and (
;;            ;;                  :scheduled t
;;            ;;                  :todo "DONE"
;;            ;;                  :not (:log closed))))
;;            ;; (:name "Done today"
;;            ;;        :todo "DONE"
;;            ;;        :log closed ;; entries from log-mode
;;            ;;        :order 4)
;;            (:name "Important"
;;                   :priority "A")
;;            ;; Groups supply their own section names when none are given
;;            (:todo "WAITING" :order 8)  ; Set order of this section
;;            (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                   ;; Show this group at the end of the agenda (since it has the
;;                   ;; highest number). If you specified this group last, items
;;                   ;; with these todo keywords that e.g. have priority A would be
;;                   ;; displayed in that group instead, because items are grouped
;;                   ;; out in the order the groups are listed.
;;                   :order 9)
;;            (:priority<= "B"
;;                         ;; Show this section after "Today" and "Important", because
;;                         ;; their order is unspecified, defaulting to 0. Sections
;;                         ;; are displayed lowest-number-first.
;;                         :order 1)
;;            (:tag "emacs"
;;                  :order 3)
;;            (:discard (:time-grid t))
;;            ;; After the last group, the agenda will display items that didn't
;;            ;; match any of these groups, with the default order position of 99
;;            )
;;     )
;;   )

;; Auth tokens location. Define Github token for magit forge
(setq auth-sources '("~/.authinfo"))

;; Stop asking each time to execute code from babel blocks
(setq org-confirm-babel-evaluate nil)

;; ensure $PATH inside Emacs look the same as in the user's shell.
;; Check exec-path variable.
;; Fixing unseeble npm executable managed through nvm, which in turn
;; used to identify installed typescript-language-server for lsp-mode.
;; Confirm the npm executable is found: M-: (executable-find "npm")
(use-package exec-path-from-shell
  ;; :init
  ;; (add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "JAVA_HOME")
  ;; Make keyring work inside Emacs (Makes Magit not ask for SSH passphrase every time)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  :custom
  ;; Show warning when exec-path-from-shell take more than 100 ms to initialize
  (exec-path-from-shell-warn-duration-millis 100)
  ;; start a non-interactive shell instead of interactive
  ;; to spead up the init process.
  ;; PATH variable should be set correctly for non-interactive shells,
  ;; in .zshenv in my case
  (exec-path-from-shell-arguments nil)
  )

;; Set ssh-agent and gpg-agent environment variables when starting Emacs from X11.
;; Makes magit not asking for ssh passphrase every time
(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package flycheck
  :config
  (setq-default flycheck-indication-mode 'right-margin)
  (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode))

(use-package yasnippet :config (yas-global-mode))

;; Java snippets for yasnippet. Unmaintained
(use-package java-snippets)

;; Convert to/from camelCase/snake_case/kebab-case
(use-package string-inflection)

(defun kipcd/prog-mode-setup ()
  ;; Highlight line trailing whitespaces
  (setq show-trailing-whitespace t)
  ;; Indent with spaces
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  ;; (setq c-basic-offset 4)

  ;; Highlight tabs
  (setq whitespace-style '(face tabs))
  (whitespace-mode)
  )

(add-hook 'prog-mode-hook  #'kipcd/prog-mode-setup)

;; Highlight tabs and trailing whitespaces in nxml-mode
(defun kipcd/nxml-mode-setup ()
  ;; Highlight line trailing whitespaces
  (setq show-trailing-whitespace t)
  ;; Indent with spaces
  (setq indent-tabs-mode nil)
  (setq nxml-child-indent 4)
  ;; Highlight tabs
  (setq whitespace-style '(face tabs))
  (whitespace-mode)
  )

(add-hook 'nxml-mode-hook #'kipcd/nxml-mode-setup)

;; Groovy mode
;; Required for Jenkinsfile mode
(use-package groovy-mode
  :config
  (setq groovy-indent-offset 4)
  )
(use-package jenkinsfile-mode
  :after groovy-mode
  )

(use-package json-mode
  :config
  (setq js-indent-level 2))

;; LSP
;; lsp-deferred is used to prevent starting LSP server when you are cycling
;; through buffers with autoloading, like with counsel-switch-buffer

;; Go to LSP mode documentation for the instructions about installing
;; desired language server on your machine
(defun kipcd/lsp-mode-setup ()
  ;; Force lsp-mode to forget the workspace folders for multi root servers so the workspace folders are added on demand
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  )

(use-package lsp-mode
  ;; :commands (lsp lsp-deferred)
  :hook
  ((lsp-mode . kipcd/lsp-mode-setup)
   (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  :general
  (kipcd/leader
    "l" '(:keymap lsp-command-map :which-key "LSP")
    "l =" '(:ignore t :wk "formatting")
    "l T" '(:ignore t :wk "toggles")
    "l s" '(:ignore t :wk "session management")
    "l g" '(:ignore t :wk "go to")
    "l h" '(:ignore t :wk "help")
    "l r" '(:ignore t :wk "refactor")
    "l a" '(:ignore t :wk "action")
    "l F" '(:ignore t :wk "folder")
    "l G" '(:ignore t :wk "peek"))
  :config
  (setq lsp-completion-enable-additional-text-edit nil)
  ;; (lsp-register-custom-settings
  ;;  ;; Fix indentation for java code actions.
  ;;  ;; When user selects a code action (create a constant, for example), LSP server sends a
  ;;  ;; request for this 2 params to apply proper indentation to the proposed code.
  ;;  ;; This way we will respond to LSP server with correct values for those params.
  ;;  '(("java.format.insertSpaces" t)
  ;;    ("java.format.tabSize" 4)))
  ;; Auto-save project buffers after code actions
  (add-hook 'lsp-after-apply-edits-hook (lambda (&rest _) (projectile-save-project-buffers))))

;; Set lsp-log-io to t to inspect communication between client and the server.
;; Use lsp-workspace-show-log to switch to the corresponding log buffer.
;; WARNING: Slows down the performance significantly
;; (setq lsp-log-io 't)

;; LSP UI improvements (sideline with code actions, pop-ups, etc)
(use-package lsp-ui
  ;; :hook (lsp-mode . lsp-ui-mode)
  ;; :custom
  ;; (lsp-ui-doc-position 'bottom) ;; Documentation pop-up frame position
  )
;; Project file tree
(use-package lsp-treemacs
  ;; :after lsp-mode
  :config
  ;; Decrease treemacs font size
  (setq treemacs-text-scale -1)
  :bind
  ("<M-right>" . treemacs-increase-width)
  ("<M-left>" . treemacs-decrease-width)
  :custom
  (treemacs-width-increment 5 "Resize treemacs width in steps of this value")
  (lsp-treemacs-error-list-severity 1 "Show only errors in lsp-treemacs-error-list")
  (lsp-treemacs-error-list-current-project-only t "Show errors only from current workspace")
  )

(use-package lsp-ivy)

(use-package lsp-java
  :config
  (setq lsp-java-java-path (substitute-in-file-name "$HOME/.sdkman/candidates/java/11.0.11.hs-adpt/bin/java"))
  (let ((lombok-file (substitute-in-file-name "$HOME/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar")))
    (setq lsp-java-vmargs
          (list
           ;; "-noverify"
              ;; "-Xmx4G"
              ;; "-Xms100m"
              ;; "-XX:+UseG1GC"
              ;; "-XX:+UseStringDeduplication"
              ;; "-Dsun.zip.disableMemoryMapping=true"
              (concat "-javaagent:" lombok-file))))
  ;; Add assertj to the list of static import completions
  (setq lsp-java-completion-favorite-static-members
        (vconcat lsp-java-completion-favorite-static-members '("org.assertj.core.api.Assertions.*")))
  ;; (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  ;; (setq lsp-java-format-settings-profile "GoogleStyle")

  ;; Performance improvements
  (setq lsp-java-completion-max-results 10)
  ;; List Java Runtime Environments
  ;; Configuration syntax: https://github.com/eclipse/eclipse.jdt.ls/issues/1307#issuecomment-573154969
  (setq lsp-java-configuration-runtimes '[
                                          (:name "JavaSE-17"
                                                 :path (substitute-in-file-name "$HOME/.sdkman/candidates/java/17.0.1-tem"))
                                          (:name "JavaSE-16"
                                                 :path (substitute-in-file-name "$HOME/.sdkman/candidates/java/16.0.1.hs-adpt"))
                                          (:name "JavaSE-15"
                                                 :path (substitute-in-file-name "$HOME/.sdkman/candidates/java/15.0.2.hs-adpt"))
                                          (:name "JavaSE-11"
                                                 :path (substitute-in-file-name "$HOME/.sdkman/candidates/java/11.0.11.hs-adpt"))
                                          (:name "JavaSE-1.8"
                                                 :path (substitute-in-file-name "$HOME/.sdkman/candidates/java/8.0.292.hs-adpt")
                                                 :default t)])

  (add-hook 'java-mode-hook 'lsp)
  :custom
  (lsp-java-sources-organize-imports-static-star-threshold 3)
  (lsp-java-sources-organize-imports-star-threshold 5)
  (lsp-java-completion-import-order ["" "javax" "java" "#"]
                                    "Order import statements as in IntelliJ. '#' is for static imports, empty string stands for 'everything esle'")
  )

(defun kipcd/fill-column-120 ()
  "Configure display-fill-column-indicator-mode to display a ruler at column 120"
  (interactive)
  (setq display-fill-column-indicator-character ?\u2502)
  (setq display-fill-column-indicator-column 120)
  (setq display-fill-column-indicator t)
  )

;; Set ruler at column 120 for java-mode
(add-hook 'java-mode-hook 'kipcd/fill-column-120)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package dap-java
  :ensure nil
  :bind
  ("<f5>" . dap-debug)
  ("<f7>" . dap-step-in)
  ("<M-f7>" . dap-step-out)
  ("<f8>" . dap-next)
  ("<f9>" . dap-continue)
  :custom
  (dap-java-test-additional-args '("-n" "\".*(Test|IT).*\""))
)

;; Load Java run configurations for dap-java. t inhibits error if file not present
(require 'wed-dap-templates (substitute-in-file-name "$HOME/wed/wed-dap-templates.el") t)

(use-package helm-lsp)

(use-package helm
  :config (helm-mode))

;; LSP typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Better completion
(use-package company
  ;; :after lsp-mode
  ;; :hook (lsp-mode . company-mode)
  :bind (
         :map company-active-map
         ("<tab>" . company-complete-common-or-cycle) ;; Complete common part or cycle through suggestions with TAB
         ("<return>" . company-complete-selection) ;; Complete selection on Enter
         :map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common)) ;; Indent or show completions on TAB on empty line
  :custom
  (company-minimum-prefix-length 3) ;; Show suggestion after typing 3 character
  (company-idle-delay 1.5) ;; Disable delay for completions to appear
  )

;; Better frontend for company
;; (use-package company-box
;;   :hook
;;   (company-mode . company-box-mode))

(defun k/open-agenda-with-todo ()
  "Display the weekly org-agenda and all todos."
  (interactive)
  (org-agenda nil "n"))
(global-set-key (kbd "<M-f5>") 'k/open-agenda-with-todo)

;; Major mode to manage .pacnew and .pacsave of Arch Linux's Pacman
(use-package pacfiles-mode)

(use-package restclient
  :config
  ;; Create new buffer for each response
  (setq restclient-same-buffer-response nil))

(use-package docker-compose-mode)

(use-package emacs-everywhere)
(remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-major-mode-org-or-markdown) ; or #'org-mode if that's what's present
(add-hook 'emacs-everywhere-init-hooks #'gfm-mode)
;; (eval-after-load "emacs-everywhere"
;;   '(defun emacs-everywhere-markdown-p ()
;;      't))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
