;;; kipcd-keybindings.el --- Keybinding configuration -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; The initial configuration is borrowed from the excellent `Amalthea' config:
;; https://github.com/sondr3/dotfiles/blob/104db61e0e5150b3ac56219274dd3adede851d4b/home/emacs/core/am-keybindings.el

;;; Commentary:
;; This is probably the hardest thing by far to configure and properly do in
;; Emacs, at least in my opinion. I could use something like Spacemacs or Doom
;; which has a proper consistent theme for keybindings, but that's no fun.
;; Instead we'll roll our own built around `Evil', `General.el' and `which-key'.
;; Lastly, we'll mimick how I used to do things in Vim (and how Spacemacs and
;; others does things) by letting `SPC' be our leader key and `,' be our major
;; mode leader key. If you are in the `insert' state, you can use `C-SPC' for
;; the leader key and `M-,' for the major mode leader key.

(defcustom kipcd-leader-key "SPC"
  "The default leader key"
  :type 'string
  :group 'kipcd)

(defcustom kipcd-leader-secondary-key "C-SPC"
  "The secondary leader key"
  :type 'string
  :group 'kipcd)

(defcustom kipcd-major-leader-key ","
  "The default major mode leader key"
  :type 'string
  :group 'kipcd)

(defcustom kipcd-major-leader-secondary-key "M-,"
  "The secondary major mode leader key"
  :type 'string
  :group 'kipcd)

;; Easy transition between buffers: M-arrow-keys.
(windmove-default-keybindings 'meta)

;; Bind keyboard-escape-quit to ESC instead of ESC ESC ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; `which-key':
;; This is a really cool package, I initially discovered this from Spacemacs (as
;; I have done with a great many things). What it does is show you any and all
;; keybindings you can complete from the binding you just executed. For example,
;; if you are in Org-mode and run `C-c', `which-key' will show on the bottom of
;; the screen and show all the keybindings you can complete from there. It's
;; really great for discoverability.
(use-package which-key
  :demand t
  :commands which-key-mode
  :config
  (setq which-key-idle-delay 0.3                          ;; Reduce the time before which-key pops up
        which-key-sort-order 'which-key-key-order-alpha   ;; Sort things properly alphabetical
        which-key-max-description-length 50)
  (which-key-mode))

;; Keybindings manager. Facilitates to maintain a personal global prefix for keybindings
(use-package general
  :demand t
  :commands general-evil-setup
  :config
  ;; keys bound in general-override-mode-map will take precedence over keys bound in any other minor mode keymaps
  (general-override-mode)
  (general-evil-setup)
  ;; define personal global prefix for keybindings - leader keys
  (general-create-definer kipcd/leader
    :keymaps '(normal insert visual emacs)
    :prefix kipcd-leader-key
    :global-prefix kipcd-leader-secondary-key)
  (general-create-definer kipcd/major-leader
    :states '(normal insert visual emacs)
    :prefix kipcd-major-leader-key
    :non-normal-prefix kipcd-major-leader-secondary-key)
  (general-nmap "SPC m" (general-simulate-key "," :which-key "major mode"))
  ;;; Global bindings
  ;; Default `which-key' prefixes
  ;; This keeps all the main menus in one place instead of spread throughout the
  ;; whole project.
  (kipcd/leader
   "SPC" '(counsel-M-x :wk "M-x")
   "a" '(:ignore t :wk "applications")
   "b" '(:ignore t :wk "buffers")
   "f" '(:ignore t :wk "files")
   "fr" '(counsel-recentf :wk "recent files")
   "fs" '(save-buffer :wk "save")
   "g" '(:ignore t :wk "git")
   "h" '(:ignore t :wk "help")
   "S" '(:ignore t :wk "spelling")
   "w" '(:ignore t :wk "windows")
   "ww" '(other-window :wk "switch window")
   "p" '(projectile-command-map :which-key "projectile")
   "t" '(org-capture :which-key "capture templates")
   "c" '(:ignore t :wk "code")
   "ce" '(next-error :wk "next error")
   "s" '(:ignore t :which-key "settings")
   "st" '(counsel-load-theme :which-key "choose theme")
   "ss" '(hydra-text-scale/body :which-key "scale-text")))

(general-define-key
 "C-M-j" 'counsel-switch-buffer)

;; Become evil
(defun kipcd/evil-hook ()
  ;; Define modes to start in emacs state by default
  (dolist (mode '(
                  eshell-mode
                  term-mode
                  ))
    (add-to-list 'evil-emacs-state-modes mode)))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (add-hook 'evil-mode-hook  'kipcd/evil-hook)
  (evil-mode 1)
  ;; backspace alternative
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  )

;; Visual hint for common evil actions
(use-package evil-goggles
  :config
  (evil-goggles-mode))

;; Pre-configured evil mode for many different modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :custom
  (evil-collection-company-use-tng nil "Disable company-tng-mode.
This will disable auto-populating buffer with pre-selected suggestion.
It was giving confusing results for lsp-mode and Java, as it was also outputting java package name
in the buffer (although not actually inserting it after accepting the suggestion).")
  )

;; Better comments
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(provide 'kipcd-keybindings)

;;; kipcd-keybindings.el ends here
