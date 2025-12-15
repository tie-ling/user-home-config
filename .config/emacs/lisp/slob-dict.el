;;;  -*- lexical-binding:t -*-
;; A client for accessing slob dictionary while reading text inside
;; emacs. This program is adapted from the built-in emacs
;; dictionary.el program.

;; goal: while point is on word, prompt that word as default query.
;; Then perform searching by executing the command
;; ~/.local/bin/dict.py --source dict --query Apfel
;; Finally, capture command output, insert output into Dictionary
;; buffer.

;; overview: define Dictionary mode.  Define how to create and close a
;; Dictionary buffer.  Define its keymap.

;; define default query-at-point function.  define minibuffer prompt.

;; define function for saving and restoring previous position within
;; text.

;; define function for performing the search.

(define-derived-mode slob-dict-mode special-mode "Slob-Dict"
 "Mode for searching a slob-dict."
  (buffer-disable-undo)
  (add-hook 'kill-buffer-hook #'slob-dict-close t t))

(defun slob-dict-store-positions ()
  "Store the current positions for later restore."
  (setq slob-dict-positions (cons (point) (window-start))))

(defun slob-dict ()
  "Create a new slob-dict buffer and install `slob-dict-mode'."
  (interactive)
  (let ((buffer (or (get-buffer "*Slob-Dict*")
                    (generate-new-buffer "*Slob-Dict*")))
        (window-configuration (current-window-configuration))
        (selected-window (frame-selected-window)))

    (switch-to-buffer buffer)
    (slob-dict-mode)

    (setq-local slob-dict-window-configuration window-configuration)
    (setq-local slob-dict-selected-window selected-window)
    (slob-dict-store-positions)))

(defun slob-dict-close (&rest _ignored)
  "Close the current slob-dict buffer and its connection."
  (interactive)
  (when (derived-mode-p 'slob-dict-mode)
    (setq major-mode nil)
    (let ((configuration slob-dict-window-configuration)
          (selected-window slob-dict-selected-window))
      (kill-buffer (current-buffer))
      (set-window-configuration configuration)
      (select-window selected-window))))

(defvar-keymap slob-dict-mode-map
  :doc "Keymap for the slob-dict mode."
  :suppress t :parent button-buffer-map
  "q"     #'slob-dict-close
  "SPC"   #'scroll-up-command
  "S-SPC" #'scroll-down-command
  "M-SPC" #'scroll-down-command
  "DEL"   #'scroll-down-command)

(defun slob-dict-search-default ()
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((car (get-char-property (point) 'data)))
   (t (current-word t))))

(defun slob-dict-read-word-default ()
  "Prompt for a word to search in the slob-dict."
  (let ((default (slob-dict-search-default)))
    (read-string (format-prompt "Search word" default)
                 default nil default)))

(defun slob-dict-search (word)
  "Search for WORD in all the known dictionaries.
Interactively, prompt for WORD, and offer the word at point as default."
  (interactive
     (list (slob-dict-read-word-default)))
    (slob-dict-new-search (list word)))

(defun slob-dict-mode-p ()
  "Return non-nil if current buffer has `slob-dict-mode'."
  (eq major-mode 'slob-dict-mode))

(defun slob-dict-ensure-buffer ()
  "If current buffer is not a slob-dict buffer, create a new one."
  (unless (slob-dict-mode-p)
    (slob-dict)))

(defun slob-dict-new-search (args &optional all)
  "Save the current state and start a new search based on ARGS."
  (slob-dict-store-positions)
  (let ((word (car args)))
    (slob-dict-ensure-buffer)

    ;; pre-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq slob-dict-marker (point-marker))

    ;; call-process, calls python, infile=nil, destination=t(current buffer),
    ;; display=nil(no redisplay), rest of arguments
    (call-process "~/.local/bin/dict.py" nil t nil "--query" word)


    ;; post-buffer
    (goto-char slob-dict-marker)

    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

(provide 'slob-dict)
