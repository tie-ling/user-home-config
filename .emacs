;; credit: yorickvP on Github
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process
       (make-process :name "wl-copy"
                      :buffer nil
                      :command '("wl-copy" "-f" "-n")
                      :connection-type 'pipe
                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (shell-command-to-string "wl-paste -n"))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-fill-function 'do-auto-fill t)
 '(custom-enabled-themes '(modus-operandi) nil nil "Customized with use-package custom")
 '(default-input-method "german-postfix")
 '(inhibit-startup-screen t)
 '(mail-envelope-from 'header)
 '(mail-specify-envelope-from t)
 '(menu-bar-mode nil)
 '(message-sendmail-envelope-from 'header)
 '(mode-line-compact 'long)
 '(modus-themes-bold-constructs nil)
 '(modus-themes-inhibit-reload nil)
 '(modus-themes-italic-constructs t)
 '(modus-themes-mixed-fonts t)
 '(modus-themes-variable-pitch-ui t)
 '(read-buffer-completion-ignore-case t)
 '(ring-bell-function nil)
 '(scroll-bar-mode nil)
 '(send-mail-function 'sendmail-send-it)
 '(sendmail-program "msmtp")
 '(tool-bar-mode nil)
 '(user-mail-address "gyuchen86@gmail.com"))

(use-package dired
  :config
  (defun dired-open-pdf ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "zathura" nil 0 nil file)))
  :bind
  (:map dired-mode-map
        ("C-o" . dired-open-pdf)))
(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package battery
  :custom
  (display-battery-mode t))

(use-package time
  :custom
  (display-time-mode t))

(use-package auth-source
  :custom
  (auth-sources '((concat (getenv "HOME") "/.password-store/authinfo.gpg"))))

(use-package shr
  :custom
  (shr-cookie-policy nil)
  (shr-inhibit-images t)
  (shr-use-colors nil))

(use-package files
  :custom
  (require-final-newline t))

(use-package minibuffer
  :custom
  (read-file-name-completion-ignore-case t))

(use-package mwheel
  :custom
  (mouse-wheel-mode nil))

(use-package simple
  :custom
  (indent-tabs-mode nil))
