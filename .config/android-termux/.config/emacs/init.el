;; -*- lexical-binding: t; -*-

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JuliaMono")))))

(use-package emacs
  :bind
  (("<volume-up>" . scroll-down-command)
   ("<volume-down>" . scroll-up-command))
  :config
  ;; See (info "(elisp) Easy Menu")
  ;; remove menu-bar-tools; to be redefined
  (define-key global-map [menu-bar tools] nil t)
  (easy-menu-define my-menu global-map
    "My Customized Menu for using E<2025-03-18 Wed 19:00>
macs on Android."
    '("My"
      ("Line Wrapping"
       ["Visual fill" visual-fill-mode]
       ["Visual line" visual-line-mode]
       ["Adaptive wrap prefix" adaptive-wrap-prefix-mode])
      ("Window"
       ["New below" split-window-below]
       ["Remove other" delete-other-windows])
      ("File"
       ["Save buffers" save-some-buffers])
      ("Org"
       ["Agenda List" org-agenda-list]
       ["Global TODO List" org-todo-list]
       ["Search for Keywords" org-search-view])
      ("Magit"
       ["Status" magit-status])))
  :custom
  (touch-screen-display-keyboard t)
  (face-font-family-alternatives
   '(("JuliaMono" "Noto Sans Mono CJK SC")
     ("Monospace" "Cascadia Code" "Lucida Console" "courier" "fixed")
     ("Monospace Serif" "Courier 10 Pitch" "Consolas" "Courier Std"
      "FreeMono" "Nimbus Mono L" "courier" "fixed")
     ("courier" "CMU Typewriter Text" "fixed")
     ("Sans Serif" "Calibri" "Tahoma" "Lucida Sans Unicode" "helv"
      "helvetica" "arial" "fixed")
     ("helv" "helvetica" "arial" "fixed")))
  (auto-fill-function 'do-auto-fill t)
  (calendar-week-start-day 1)
  (completion-ignore-case t)
  (custom-enabled-themes '(modus-vivendi) nil nil)
  (display-battery-mode nil)
  (display-time-mode nil)
  (electric-pair-mode t)
  (enable-local-variables nil)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (mail-envelope-from 'header)
  (mail-specify-envelope-from t)
  (menu-bar-mode t)
  (message-default-mail-headers "Reply-To: Yuchen Guo <yc@apvc.uk>")
  (message-sendmail-envelope-from 'header)
  (mode-line-compact 'long)
  (modus-themes-bold-constructs nil)
  (modus-themes-inhibit-reload nil)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (network-security-level 'paranoid)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (require-final-newline t)
  (send-mail-function 'sendmail-send-it)
  (sendmail-program "msmtp")
  (tab-always-indent 'complete)
  (tool-bar-mode t)
  (tool-bar-position 'bottom)
  (modifier-bar-mode t)
  (visible-bell t)
  (user-mail-address "yc@apvc.uk")
  (xterm-mouse-mode nil))

(use-package visual-fill
  :ensure t)

(use-package adaptive-wrap
  :ensure t)

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-mode t))

(use-package magit
  :ensure t)

(use-package pyim
  :ensure t)

(use-package pyim-basedict
  :ensure t
  :config
  (pyim-basedict-enable))

(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c r" . org-shiftright))
  :custom
  (org-agenda-inhibit-startup t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-start-with-log-mode nil)
  (org-clock-mode-line-total 'current)
  (org-agenda-prefix-format
   '((agenda . " %i %?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  (org-agenda-files
   '("~/Projects/agenda/agenda.org"
     "~/Projects/agenda/uni.org"))
  (org-directory '("~/Documents/org"))
  (org-agenda-span 'day)
  (org-display-custom-times nil)
  (org-time-stamp-custom-formats '("%m-%d" . "%H:%M"))
  (org-latex-compiler "lualatex")
  (org-export-initial-scope 'buffer)
  (org-modules
   '(ol-bbdb ol-bibtex ol-doi ol-eww ol-info ol-irc ol-mhe ol-rmail org-tempo))
  (org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")
     ("py" . "src python"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(adaptive-wrap magit pyim-basedict vertico visual-fill)))
