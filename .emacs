(require 'pyim)
(require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
(pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置

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
 '(menu-bar-mode nil)
 '(mode-line-compact 'long)
 '(modus-themes-bold-constructs nil)
 '(modus-themes-inhibit-reload nil)
 '(modus-themes-italic-constructs t)
 '(modus-themes-mixed-fonts t)
 '(modus-themes-variable-pitch-ui t)
 '(read-buffer-completion-ignore-case t)
 '(ring-bell-function nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package battery
  :custom
  (display-battery-mode t))

(use-package time
  :custom
  (display-time-mode t))

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

(use-package simple
  :custom
  (indent-tabs-mode nil))
