;; Config to execute on automatic Emacs startup when Linux starts

(defun emacs-startup-screen ()
  "Display the weekly org-agenda and all todos."
  (org-agenda nil "n"))
(add-hook 'emacs-startup-hook #'emacs-startup-screen)
