;; Provide the means of running external applications in background, like panels and network or bluetooth applets
(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;;(eshell)

  ;; Show battery status in the mode line
  (display-battery-mode 1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (setq display-time-default-load-average nil)
  (display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Launch apps that will run in the background
  ;; (efs/run-in-background "nm-applet")
  ;; (efs/run-in-background "pasystray")
  ;; (efs/run-in-background "blueman-applet")
  (efs/run-in-background "dropbox")
  (efs/run-in-background "pasystray")
  )

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ;; Adds a current web page name to buffer name
    ("Chromium" (exwm-workspace-rename-buffer (format "Chromium: %s" exwm-title)))))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Chromium" (exwm-workspace-move-window 8))
    ("mpv" (exwm-floating-toggle-floating)
           (exwm-layout-toggle-mode-line))))

;; This function isn't currently used, only serves as an example how to
;; position a window
(defun efs/position-window ()
  "Position a window on a top left corner of the screen"
  (let* ((pos (frame-position))
         (pos-x (car pos))
          (pos-y (cdr pos)))

    (exwm-floating-move (- pos-x) (- pos-y))))

(defun efs/update-displays ()
  (efs/run-in-background "autorandr --change --force")
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)
  ;; Display all EXWM buffers in every workspace buffer list
  (setq exwm-workspace-show-all-buffers t)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When EXWM starts up, do some extra configuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

  ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
  ;; (setq exwm-workspace-minibuffer-position 'bottom)

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 2048x1152 --pos 0x0 --rotate normal")

  ;; Assign workspaces to monitors
  ;; run `exwm-randr-refresh' after changing this map
  (setq exwm-randr-workspace-monitor-plist '(
                                             1 "eDP-1-1"
                                             2 "eDP-1-1"
                                             3 "eDP-1-1"
                                             4 "eDP-1-1"
                                             5 "eDP-1-1"
                                             6 "HDMI-0"
                                             7 "HDMI-0"
                                             8 "HDMI-0"
                                             9 "HDMI-0"
                                             0 "HDMI-0"
                                             ))

  ;; React to display connectivity changes, do initial display update
  (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
  ;; Update display config on init
  (efs/update-displays)
  ;; Teleport mouse to display assigned to workspace after switching
  (setq exwm-workspace-warp-cursor t)

  (setq
   ;; mouse-autoselect-window t
   focus-follows-mouse t)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  ;; Sometimes systemtray could not display all icons if the height isn't set explicitely
  (setq exwm-systemtray-height 32)
  (exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly to the app
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([?\s-j] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))
