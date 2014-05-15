;; -*- coding: utf-8 -*-

(global-set-key "\C-x\C-x" 'c-quick-toggle-mode)
(global-set-key [down]  'c-quick-down-key)
(global-set-key [up]    'c-quick-up-key)
(global-set-key [right] 'c-quick-right-key)
(global-set-key [left]  'c-quick-left-key)

(defvar *c-quick-mode* nil)
(defvar *c-quick-ding* t)
(setq *c-quick-in-minibuffer* nil)

(defun c-quick-toggle-mode ()
  (interactive)
  (setq *c-quick-mode* (not *c-quick-mode*))
  (c-quick-set-mode *c-quick-mode*)
  (cond
   (*c-quick-mode*
    (message "c-quick-mode is ON"))
   (t
    (message "c-quick-mode is OFF"))))

(defun c-quick-set-mode (arg)
  (cond
   (arg
    (setq show-paren-style 'expression)
    (setq show-paren-delay 0)
    (show-paren-mode 1))
   (t
    (show-paren-mode 0))))

(defun c-quick-mode ()
  (and (not *c-quick-in-minibuffer*) *c-quick-mode*)
  )

;; 通常のミニバッファ
(add-hook 'minibuffer-setup-hook '(lambda ()
                                    (c-quick-set-mode nil)
                                    (setq *c-quick-in-minibuffer* t)))
(add-hook 'minibuffer-exit-hook  '(lambda ()
                                    (c-quick-set-mode *c-quick-mode*)
                                    (setq *c-quick-in-minibuffer* nil)))
;; インクリメンタル検索
(add-hook 'isearch-mode-hook     '(lambda ()
                                    (c-quick-set-mode nil)
                                    (setq *c-quick-in-minibuffer* t)))
(add-hook 'isearch-mode-end-hook '(lambda ()
                                    (c-quick-set-mode *c-quick-mode*)
                                    (setq *c-quick-in-minibuffer* nil)))

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
    (if (eobp) (c-quick-ding) (forward-line 1))))

(defun c-quick-previous-line ()
  (if (not (bolp))
      (backward-char)
    (if (bobp) (c-quick-ding) (forward-line -1))))

(defun c-quick-forward-sexp ()
  (cond
   ((eobp) (c-quick-ding))
   ((looking-at "\\s)") (c-quick-ding))
   ((looking-at "\\s-*\\s<")
    (forward-line)
    (while (looking-at "\\s-*\\s<") (forward-line)))
   ((looking-at "\\s-") (while (looking-at "\\s-") (forward-char)))
   ((looking-at "\n") (forward-char))
   (t (ignore-errors (forward-sexp)))))

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
   (t (ignore-errors (backward-sexp)))))
