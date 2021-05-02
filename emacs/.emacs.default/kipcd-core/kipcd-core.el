;;; kipcd-core.el --- kipcd core -*- lexical-binding: t -*-

;; Core configuration.

;;; Kipcd group and customizations

(defgroup kipcd nil
  "Kipcd settings and configurations."
  :group 'convenience
  :prefix "kipcd")

;;;; Directories
(defcustom kipcd-emacs-dir (eval-when-compile (file-truename user-emacs-directory))
  "Path to the current Emacs directory."
  :type 'directory
  :group 'kipcd)

(defcustom kipcd-core-dir (expand-file-name "kipcd-core/" kipcd-emacs-dir)
  "Core packages and configuration for Kipcd."
  :type 'directory
  :group 'kipcd)

;; (defcustom kipcd-module-dir (expand-file-name "kipcd-modules/" kipcd-emacs-dir)
;;   "Modules directory for Kipcd."
;;   :type 'directory
;;   :group 'kipcd)

;; (defcustom kipcd-util-dir (expand-file-name "kipcd-utils/" kipcd-emacs-dir)
;;   "Utility directory for Kipcd."
;;   :type 'directory
;;   :group 'kipcd)

;; (defcustom kipcd-org-dir (expand-file-name "kipcd-org/" kipcd-emacs-dir)
;;   "Org directory for Kipcd."
;;   :type 'directory
;;   :group 'kipcd)

;; (defcustom kipcd-dotfiles-dir (expand-file-name ".dotfiles" (getenv "HOME"))
;;   "Location of dotfiles for Kipcd."
;;   :type 'directory
;;   :group 'kipcd)

;;; Load core configuration
(add-to-list 'load-path kipcd-core-dir t)

(require 'kipcd-keybindings)

(provide 'kipcd-core)

;;; kipcd-core.el ends here
