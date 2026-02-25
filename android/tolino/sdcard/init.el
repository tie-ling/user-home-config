;;; -*- lexical-binding: t -*-
(server-start)

(use-package emacs
  :config
  (global-set-key (kbd "<volume-down>") (kbd "<next>"))
  (global-set-key (kbd "<volume-up>") (kbd "<prior>"))
   (global-set-key (kbd "<right>") (kbd "<next>"))
  (global-set-key (kbd "<left>") (kbd "<prior>"))
  :custom
  (auto-fill-function 'do-auto-fill t)
  (calendar-week-start-day 1)
  (completion-ignore-case t)
  (custom-enabled-themes '(modus-operandi) nil nil)
  (display-battery-mode nil)
  (display-time-mode nil)
  (electric-pair-mode t)
  (enable-local-variables nil)
  (global-visual-line-mode t)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (mode-line-compact 'long)
  (touch-screen-enable-hscroll nil)
  (tool-bar-button-margin 18)
  (menu-bar-mode t)
  (face-font-family-alternatives
   '(("JuliaMono" "Noto Sans SC") ("Libertinus Serif" "Noto Serif SC")))
  (modus-themes-bold-constructs nil)
  (modus-themes-inhibit-reload nil)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (network-security-level 'paranoid)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (require-final-newline t)
  (tab-always-indent 'complete)
  (tool-bar-mode t)
  (tool-bar-position 'bottom)
  (blink-cursor-mode nil)
  ; monochrome eink display
  (android-display-depth 1)
  (visible-bell t))

(use-package visual-fill
  :ensure t
  :hook ((text-mode . visual-fill-mode)))

(use-package adaptive-wrap
  :ensure t)

(use-package text-mode
  :hook
  ((text-mode . variable-pitch-mode)
   (text-mode . text-mode-tool-bar))
  :bind
  (("C-c d" . dictionary-search))
  :config
  (defun text-mode-tool-bar (&rest _ignored)
    "Set up tool bar for text mode"
    (interactive)
    (define-key text-mode-menu [dictionary]
                '(menu-item "Dictionary" yc-dictionary-lookup-definition :help "Look up word at point"))
    (let ((map (make-sparse-keymap)))
      ;; toolbar icon at https://cgit.git.savannah.gnu.org/cgit/emacs.git/tree/etc/images
      (tool-bar-local-item-from-menu 'kill-this-buffer "close" map global-map)
      (tool-bar-local-item-from-menu 'save-buffer "save" map global-map)
      (tool-bar-local-item-from-menu 'undo "undo" map global-map)
      (tool-bar-local-item-from-menu 'kill-region "cut" map global-map)
      (tool-bar-local-item-from-menu 'kill-ring-save "copy" map global-map)
      (tool-bar-local-item-from-menu 'yank "paste" map global-map)
      (tool-bar-local-item-from-menu 'isearch-forward "search" map global-map)
      (tool-bar-local-item-from-menu 'yc-dictionary-lookup-definition "index" map text-mode-map  :label "Look up word at point")
      (setq-local tool-bar-map map))))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-mode t))

(use-package shr
  :custom
  (shr-cookie-policy nil)
  (shr-inhibit-images t)
  (shr-use-colors nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 300 :family "JuliaMono"))))
 '(menu ((t (:family "Libertinus Serif"))))
 '(variable-pitch ((t (:family "Libertinus Serif" :height 350)))))

(defun android-font-list ()
  "list all available fonts"
  (dolist (f (font-family-list))
    (princ f)
    (princ "\n")))


(defun yc-dict-scale ()
  (setq-local text-scale-mode-amount -1)
  (text-scale-mode t))

(use-package dictionary
  :commands yc-dictionary-lookup-definition
  :config
  (defun yc-dictionary-lookup-definition (&rest _ignored)
    "With DictD Capitalisation Fix. Unconditionally lookup the word at point."
    (interactive)
    (let* ((case-fold-search nil)
           (cword (current-word nil t))
           (word  (if (string-match-p "^[A-ZÄÜÖ]" cword) (concat "9" cword) cword)))
      (unless word
        (user-error "No word at point"))
      (dictionary-new-search (cons word dictionary-default-dictionary))))
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-default-strategy "re")
  :hook
  ((dictionary-mode . variable-pitch-mode)
   (dictionary-mode . text-mode-tool-bar)
   (dictionary-mode . yc-dict-scale)))



(defun yc-touch-screen-handle-scroll (dx dy)
  "Scroll the display assuming that a touch point has moved by DX and DY.
Perform vertical scrolling by DY, using `pixel-scroll-precision'
if `touch-screen-precision-scroll' is enabled.  Next, perform
horizontal scrolling according to the movement in DX."
  ;; Perform vertical scrolling first.  Do not ding at buffer limits.
  ;; Show a message instead.
  (condition-case nil
      (if touch-screen-precision-scroll
          (progn
            (if (> dy 0)
                (pixel-scroll-precision-scroll-down-page dy)
              (pixel-scroll-precision-scroll-up-page (- dy)))
            ;; Now set `lines-vscrolled' to an value that will result
            ;; in hscroll being disabled if dy looks as if a
            ;; significant amount of scrolling is about to take
            ;; Otherwise, horizontal scrolling may then interfere with
            ;; precision scrolling.
            (when (> (abs dy) 10)
              (setcar (nthcdr 7 touch-screen-current-tool) 10)))
        ;; Start conventional scrolling.  First, determine the
        ;; direction in which the scrolling is taking place.  Load the
        ;; accumulator value.
        (let ((accumulator (or (nth 5 touch-screen-current-tool) 0))
              (window (cadr touch-screen-current-tool))
              (lines-vscrolled (or (nth 7 touch-screen-current-tool) 0)))
          (setq accumulator (+ accumulator dy)) ; Add dy.
          ;; Figure out how much it has scrolled and how much remains
          ;; on the top or bottom of the window.
          (while (catch 'again
                   (let* ((line-height (window-default-line-height window)))
                     (if (and (< accumulator 0)
                              (>= (- accumulator) line-height))
                         (progn
                           (setq accumulator (+ accumulator line-height))
                           (execute-kbd-macro (kbd "<prior>"))
                           (setq lines-vscrolled (1+ lines-vscrolled))
                           (when (not (zerop accumulator))
                             ;; If there is still an outstanding
                             ;; amount to scroll, do this again.
                             (throw 'again t)))
                       (when (and (> accumulator 0)
                                  (>= accumulator line-height))
                         (setq accumulator (- accumulator line-height))
                         (execute-kbd-macro (kbd "<next>"))

                         (setq lines-vscrolled (1+ lines-vscrolled))
                         (when (not (zerop accumulator))
                           ;; If there is still an outstanding amount
                           ;; to scroll, do this again.
                           (throw 'again t)))))
                   ;; Scrolling is done.  Move the accumulator back to
                   ;; touch-screen-current-tool and break out of the
                   ;; loop.
                   (setcar (nthcdr 5 touch-screen-current-tool) accumulator)
                   (setcar (nthcdr 7 touch-screen-current-tool)
                           lines-vscrolled)
                   nil))))
    (beginning-of-buffer
     (message (error-message-string '(beginning-of-buffer))))
    (end-of-buffer
     (message (error-message-string '(end-of-buffer))))))


(defun yc-touch-screen-scroll (event)
  "Scroll the window within EVENT, a `touchscreen-scroll' event.
If `touch-screen-precision-scroll', scroll the window vertically
by the number of pixels specified within that event.  Else,
scroll the window by one line for every
`window-default-line-height' pixels worth of movement.

If EVENT also specifies horizontal motion and no significant
amount of vertical scrolling has taken place, also scroll the
window horizontally in conjunction with the number of pixels in
the event."
  (interactive "e")
  (let ((window (nth 1 event))
        (dx (nth 2 event))
        (dy (nth 3 event)))
    (with-selected-window window
      (yc-touch-screen-handle-scroll  (/ dx 4) (/ dy 4)))))


                                        ; redefine touch scrolling to scroll-up/down
(global-set-key [touchscreen-scroll] #'yc-touch-screen-scroll)


(define-key menu-bar-options-menu [view-mode]
            '(menu-item "View Mode" view-mode :enable t))
(define-key menu-bar-options-menu [fullscreen]
            '(menu-item "Toggle fullscreen" toggle-frame-fullscreen :enable t))

(use-package desktop
  :custom
  (desktop-save-mode t)
  (desktop-save t)
  (desktop-dirname (expand-file-name "~")))

(use-package saveplace
  :custom
  (save-place-mode t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-operandi) nil nil "Customized with use-package emacs")
 '(package-selected-packages nil))
