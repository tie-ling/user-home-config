(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process
        (make-process
         :name "wl-copy"
         :buffer nil
         :command '("wl-copy" "-f" "-n")
         :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(use-package emacs
  :custom
  (auto-fill-function 'do-auto-fill t)
  (calendar-week-start-day 1)
  (custom-enabled-themes '(modus-operandi) nil nil)
  (default-input-method "pyim")
  (electric-pair-mode t)
  (enable-local-variables nil)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (mail-envelope-from 'header)
  (mail-specify-envelope-from t)
  (menu-bar-mode nil)
  (message-default-mail-headers "Reply-To: Yuchen Guo <yc@apvc.uk>")
  (message-sendmail-envelope-from 'header)
  (modus-themes-bold-constructs nil)
  (modus-themes-inhibit-reload nil)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (network-security-level 'paranoid)
  (require-final-newline t)
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

(use-package counsel
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))

(use-package ledger-mode
  :custom
  ((ledger-binary-path "hledger")
   (ledger-report-use-strict t)
   (ledger-mode-should-check-version nil)
   (ledger-report-auto-width nil)
   (ledger-report-links-in-register nil)
   (ledger-default-date-string "%Y-%m-%d")
   (ledger-report-native-highlighting-arguments '("--color=always")))
  :mode ("\\.hledger\\'" "\\.ledger\\'"))

(use-package org
  :bind
  (("C-c a" . org-agenda))
  :custom
  (org-agenda-inhibit-startup t)
  (org-agenda-window-setup 'only-window)
  (org-agenda-start-with-log-mode nil)
  (org-clock-mode-line-total 'current)
  (org-agenda-prefix-format
   '((agenda . " %i %?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  (org-agenda-files '("~/Documents/org/agenda/logbook/"
                      "~/Documents/org/agenda/schedule/"
                      "~/Documents/org/agenda/todo/"))
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
