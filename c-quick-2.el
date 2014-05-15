;; -*- coding: utf-8 -*-

(setq scroll-conservatively 1)

(global-set-key "\C-x\C-x" 'c-quick-toggle-mode)
(global-set-key [down]  'c-quick-down-key)
(global-set-key [up]    'c-quick-up-key)
(global-set-key [right] 'c-quick-right-key)
(global-set-key [left]  'c-quick-left-key)
(global-set-key "\M-c" 'c-quick-copy-sexp)
(global-set-key "\M-d" 'c-quick-delete-sexp)
(global-set-key "\M-k" 'c-quick-kill-sexp)

(defvar *c-quick-ding* t)

(defvar _c-quick-mode_ nil)
(defvar _c-quick-in-minibuffer_ nil)

(defun c-quick-toggle-mode ()
  (interactive)
  (setq _c-quick-mode_ (not _c-quick-mode_))
  (c-quick-set-mode _c-quick-mode_)
  (cond
   (_c-quick-mode_
    (message "c-quick-mode is ON"))
   (t
    (message "c-quick-mode is OFF"))))

(defun c-quick-set-mode (arg)
  (if (not arg)
      (show-paren-mode 0)
    (setq show-paren-style 'expression)
    (setq show-paren-delay 0)
    (show-paren-mode 1)))

(defun c-quick-mode ()
  (and 
   (not (window-minibuffer-p (selected-window)))
   _c-quick-mode_))

(defun c-quick-ding ()
  (if *c-quick-ding* (ding)))

(defun c-quick-down-key ()
  (interactive)
  (if (not (c-quick-mode)) (next-line) (c-quick-next-line)))

(defun c-quick-up-key ()
  (interactive)
  (if (not (c-quick-mode)) (previous-line) (c-quick-previous-line)))

(defun c-quick-right-key ()
  (interactive)
  (if (not (c-quick-mode)) (forward-char) (c-quick-forward-sexp)))

(defun c-quick-left-key ()
  (interactive)
  (if (not (c-quick-mode)) (backward-char) (c-quick-backward-sexp)))

(defun c-quick-next-line ()
  (if (not (bolp))
      (forward-char)
    (if (eobp) (c-quick-ding) (forward-line 1)))
  (c-quick-recenter))

(defun c-quick-previous-line ()
  (if (not (bolp))
      (backward-char)
    (if (bobp) (c-quick-ding) (forward-line -1)))
  (c-quick-recenter))

(defun c-quick-forward-sexp ()
  (cond
   ((eobp) (c-quick-ding))
   ((looking-at "\\s)") (c-quick-ding))
   ((looking-at "\\s-*\\s<")
    (forward-line)
    (while (looking-at "\\s-*\\s<") (forward-line)))
   ((looking-at "\\s-") (while (looking-at "\\s-") (forward-char)))
   ((looking-at "\n") (forward-char))
   (t (ignore-errors (forward-sexp))))
  (c-quick-recenter))

(defun c-quick-backward-sexp ()
  (cond
   ((bobp) (c-quick-ding))
   ((looking-back "\\s(") (c-quick-ding))
   ((and (looking-back "\\s>")
         (save-excursion (forward-line -1) (looking-at "\\s-*\\s<")))
    (goto-char (match-beginning 0))
    (while (and (looking-back "\\s>")
                (save-excursion (forward-line -1) (looking-at "\\s-*\\s<")))
      (goto-char (match-beginning 0))))
   ((looking-back "\\s-") (while (looking-back "\\s-") (backward-char)))
   ((looking-back "\\s<") (while (looking-back "\\s<") (backward-char)))
   ((looking-back "\n") (backward-char))
   (t (ignore-errors (backward-sexp))))
  (c-quick-recenter))

(defun c-quick-recenter ()
  (cond
   ((<= (point) (window-start)) (recenter 0))
   ((>= (point) (window-end))   (recenter -1))))

(defun c-quick-copy-sexp ()
  (interactive)
  (let ((opoint (point)))
    (c-quick-forward-sexp)
    (copy-region-as-kill opoint (point))))

(defun c-quick-delete-sexp ()
  (interactive)
  (let ((opoint (point)))
    (c-quick-forward-sexp)
    (delete-region opoint (point))))

(defun c-quick-kill-sexp ()
  (interactive)
  (let ((opoint (point)))
    (c-quick-forward-sexp)
    (kill-region opoint (point))))
