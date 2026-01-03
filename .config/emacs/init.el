;; -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (auto-fill-function 'do-auto-fill t)
  (calendar-week-start-day 1)
  (completion-ignore-case t)
  (custom-enabled-themes '(modus-operandi) nil nil)
  (default-input-method "pyim")
  (display-battery-mode t)
  (display-time-mode t)
  (electric-pair-mode t)
  (enable-local-variables nil)
  (face-font-family-alternatives
   '(("JuliaMono" "Noto Sans Mono CJK SC")
     ("Libertinus Serif" "Noto Serif CJK SC")))
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (mail-envelope-from 'header)
  (mail-specify-envelope-from t)
  (menu-bar-mode t)
  (message-default-mail-headers "Reply-To: Yuchen Guo <yc@apvc.uk>")
  (message-sendmail-envelope-from 'header)
  (modus-themes-bold-constructs nil)
  (modus-themes-inhibit-reload nil)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (network-security-level 'paranoid)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (require-final-newline t)
  (scroll-bar-mode nil)
  (send-mail-function 'sendmail-send-it)
  (sendmail-program "msmtp")
  (tab-always-indent 'complete)
  (tool-bar-mode nil)
  (user-mail-address "yc@apvc.uk"))

(use-package pyim-basedict
  :config
  (pyim-basedict-enable))

(use-package shr
  :custom
  (shr-cookie-policy nil)
  (shr-inhibit-images t)
  (shr-use-colors nil))

(use-package notmuch
  :custom
  (notmuch-archive-tags '("-inbox" "+archived"))
  (notmuch-crypto-process-mime nil)
  (notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox not tag:deleted" :key "i")
     (:name "deleted" :query "tag:deleted")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")))
  (notmuch-show-logo nil))

(use-package dictionary
  :hook
  ((dictionary-mode . variable-pitch-mode)
   (text-mode . text-mode-tool-bar)
   (dictionary-mode . text-mode-tool-bar))

  :config
  (defun text-mode-tool-bar (&rest _ignored)
    "Set up tool bar for text mode"
    (interactive)
    (define-key menu-bar-goto-menu [scroll-up]
                '(menu-item "Scroll up" scroll-up-command :help "Scroll up a full screen"))
    (let ((map (make-sparse-keymap)))
      (tool-bar-local-item-from-menu 'dictionary-lookup-definition "index" map dictionary-mode-map  :label "Look up word at point")
      (tool-bar-local-item-from-menu 'scroll-up-command "save" map global-map  :label "Scroll up")
      (setq-local secondary-tool-bar-map map)))

  :bind
  (("C-c d" . dictionary-lookup-definition)
   ("<f6>" . dictionary-lookup-definition)))


(use-package text-mode
  :hook
  ((text-mode . variable-pitch-mode)))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :custom
  (vertico-mode t))

(use-package ledger-mode
  :custom
  (ledger-report-use-strict t)
  (ledger-default-date-format "%y-%m-%d")
  :mode ("\\.ledger\\'"))


(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link))
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
   "~/Projects/org/agenda-file-list.txt")
  (org-directory "~/Projects/org")
  (org-agenda-span 'day)
  (org-display-custom-times nil)
  (org-time-stamp-custom-formats '("%m-%d" . "%H:%M"))
  (org-export-initial-scope 'buffer)
  (org-export-backends '(ascii beamer html icalendar latex md odt))
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
     ("py" . "src python")))
  :hook
  ;; in org mode, do not use <> electric pairs, as this is used by
  ;; org-tempo for structure templates
  (org-mode . (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t
                    (,electric-pair-inhibit-predicate c)))))))

(use-package haskell-ts-mode
  :mode ("\\.hs\\'"))

(use-package nix-ts-mode
  :mode ("\\.nix\\'"))


(use-package auctex
  :ensure t)

(use-package ConTeXt-mode
  :mode ("\\.tex\\'")
  :hook
  ((ConTeXt-mode . turn-on-reftex)

   ;; show \alpha as α and \mathbb{R} as ℝ
   (ConTeXt-mode . prettify-symbols-mode)

   ;; shortcuts for symbols
   (ConTeXt-mode . LaTeX-math-mode))

  :custom
  ;; AUCTeX defaults to mkii; change to iv for iv and lmtx
  (ConTeXt-Mark-version "IV")

  ;; Enable electric left right brace
  (LaTeX-electric-left-right-brace t)

  ;; Do not unprettify symbol at point
  (prettify-symbols-unprettify-at-point nil)

  ;; Let AUCTeX properly detect formula environment as math mode
  (texmathp-tex-commands
   '(("\\startformula" sw-on)
     ("\\stopformula" sw-off)
     ("\\m" arg-on)))

  ;; Set PDF viewer
  (TeX-view-program-selection '((output-pdf "Zathura")))

  ;; Don't as for permission, just save all files
  (TeX-save-query nil)

  ;; Auto-save
  (TeX-auto-save t)

  ;; Debug bad boxes and warnings after compilation via
  ;; C-c ` key
  (TeX-debug-bad-boxes t)
  (TeX-debug-warnings t)

  ;; Electric inline math, 
  (TeX-electric-math '("\\m{" . "}"))

  ;; Electric sub and superscript, inserts {} after ^ and _
  ;; such as a^{}.
  (TeX-electric-sub-and-superscript t)

  ;; RefTex
  (reftex-plug-into-AUCTeX t)

  ;; Customize keyboard shortcuts for TeX math macros
  (LaTeX-math-list
   '(("o r" "mathbb{R}" nil nil)
     ("o Q" "qquad" nil nil)
     ("o o" "sim" nil nil)
     ("o ," "smblksquare" nil nil)
     ("o q" "quad" nil nil)
     ("o b" LaTeX-math-bf nil nil)
     ("o n" "mathbb{N}" nil nil)
     (?= "coloneq" nil nil)
     ("o c" "mathbb{C}" nil nil)))

  :bind
  ;; Electric \left(\right) \left[\right] \left\{\right\}
  ;; only left brace; there is no right electric brace function
  (:map ConTeXt-mode-map ("(" . LaTeX-insert-left-brace))
  (:map ConTeXt-mode-map ("[" . LaTeX-insert-left-brace))
  (:map ConTeXt-mode-map ("{" . LaTeX-insert-left-brace))

  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; in context mode, override auctex function for inserting mathcal
  (defun LaTeX-math-cal (char dollar)
      "Insert a {\\cal CHAR}.  If DOLLAR is non-nil, put $'s around it.
If `TeX-electric-math' is non-nil wrap that symbols around the
char."
      (interactive "*c\nP")
      (insert "\\mathcal{" (char-to-string char) "}"))


  (defun LaTeX-math-bf (char dollar)
      "Insert a {\\cal CHAR}.  If DOLLAR is non-nil, put $'s around it.
If `TeX-electric-math' is non-nil wrap that symbols around the
char."
      (interactive "*c\nP")
      (insert "\\mathbf{" (char-to-string char) "}"))

  ;; Prettify symbols mode, customizable.
  (with-eval-after-load "tex-mode"
    (dolist (symb
             '(("\\colon" . ?:)
               ("\\msansS" . ?𝖲)
               ("\\smblksquare" . ?▪)
               ("\\mathbf{A}" . ?𝐀)
               ("\\mathbf{B}" . ?𝐁)
               ("\\mathbf{C}" . ?𝐂)
               ("\\mathbf{D}" . ?𝐃)
               ("\\mathbf{X}" . ?𝐗)
               ("\\mathbf{Y}" . ?𝐘)
               ("\\mathbf{p}" . ?𝐩)
               ("\\mathbf{q}" . ?𝐪)
               ("\\mathbb{C}" . ?ℂ)
               ("\\mathbb{K}" . ?𝕂)))
      (add-to-list 'tex--prettify-symbols-alist symb))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JuliaMono"))))
 '(variable-pitch ((t (:family "Libertinus Serif" :height 120)))))
