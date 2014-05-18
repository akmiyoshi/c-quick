;; -*- coding: utf-8 -*-
;;
;; 34.2.1 構文クラス一覧
;; http://www.geocities.co.jp/SiliconValley-Bay/9285/ELISP-JA/elisp_565.html
;;
;; 3.1 Integer Basics
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Integer-Basics.html

(global-set-key (kbd "<down>")     'c-quick-down-key)
(global-set-key (kbd "<up>")       'c-quick-up-key)
(global-set-key (kbd "<right>")    'c-quick-right-key)
(global-set-key (kbd "<left>")     'c-quick-left-key)
(global-set-key (kbd "C-z")        'c-quick-toggle-mode)
(global-set-key (kbd "M-w")        'c-quick-copy-region)
(global-set-key (kbd "C-w")        'c-quick-kill-region)
(global-set-key (kbd "C-M-\\")     'c-quick-indent-region)
(global-set-key (kbd "<C-delete>") 'c-quick-delete-region)
(global-set-key (kbd "C-M-SPC")    'c-quick-mark-sexp)
(global-set-key (kbd "C-M-@")      'c-quick-mark-sexp)

(defvar *c-quick-ding* t)

(defvar _c-quick-mode_ nil)

(defun c-quick-toggle-mode ()
  (interactive)
  (setq _c-quick-mode_ (not _c-quick-mode_))
  (c-quick-set-mode _c-quick-mode_)
  (cond
   (_c-quick-mode_ (message "c-quick-mode is ON"))
   (t (message "c-quick-mode is OFF"))))

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

(defun c-quick-redisplay ()
  (c-quick-recenter)
  (c-quick-show-info))

(defun c-quick-down-key ()
  (interactive)
  (if (not (c-quick-mode))
      (next-line)
    (c-quick-next-line))
  (c-quick-redisplay))

(defun c-quick-up-key ()
  (interactive)
  (if (not (c-quick-mode))
      (previous-line)
    (c-quick-previous-line))
  (c-quick-redisplay))

(defun c-quick-right-key ()
  (interactive)
  (if (not (c-quick-mode))
      (forward-char)
    (c-quick-forward-sexp))
  (c-quick-redisplay))

(defun c-quick-left-key ()
  (interactive)
  (if (not (c-quick-mode))
      (backward-char)
    (c-quick-backward-sexp))
  (c-quick-redisplay))

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
   ;; ((looking-at "\n") (forward-char))
   ((looking-at "\n")
    (let ((bol? (bolp)))
      (forward-char)
      (when bol?
        (while (and (bolp) (looking-at "\n"))
          (forward-char)))))
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
   ;; ((looking-back "\n") (backward-char))
   ((looking-back "\n")
    (backward-char)
    (while (and (bolp) (looking-back "\n")
                (save-excursion (backward-char) (bolp)))
      (backward-char)))
   (t (ignore-errors (backward-sexp)))))

(defun c-quick-show-info ()
  (when (c-quick-mode)
    (save-excursion
      (cond
       ((looking-back "\\s)\\|\\s\"\\|\\sw\\|\\s_")
        (let ((opoint (point)))
          (c-quick-backward-sexp)
          (c-quick-count-lines opoint (point))))
       ((looking-at "\\(\\s-*\\)\\(\\sw\\|\\s_\\|\\s(\\|\\s<\\|\\s\"\\|\\s'\\)")
        (goto-char (match-end 1))
        (let ((opoint (point)))
          (c-quick-forward-sexp)
          (c-quick-count-lines opoint (point))))))))

(defun c-quick-count-lines (start end)
  (let ((lines (count-lines start end)))
    (if (= lines 1) (message "1 line.") (message "%s lines." lines))))

(defun c-quick-window-end ()
  (let ((left (save-excursion
                (goto-char (window-start))
                (forward-line (- (window-height) 2)))))
    (if (> left 0)
        most-positive-fixnum
      (let ((opoint (point))
            (wend (window-end nil t)))
        (save-excursion
          (goto-char wend)
          (forward-line -1)
          (backward-char)
          (point))))))

(defun c-quick-recenter ()
  ;;(message "%s %s %s" (point) (window-start) (c-quick-window-end))
  (cond
   ((< (point) (window-start)) (recenter 0))
   ((> (point) (c-quick-window-end)) (recenter -1))))

(defun c-quick-operate-on-region-or-sexp (op)
  (interactive)
  (cond
   ((not transient-mark-mode)
    (error "transient-mark-mode should not be nil."))
   (t
    (funcall op
     (point)
     (if (region-active-p) (mark) (c-quick-forward-sexp) (point))))))

(defun c-quick-copy-region ()
  (interactive)
  (c-quick-operate-on-region-or-sexp
   #'(lambda (beg end)
       (kill-ring-save beg end)
       (setq this-command 'kill-region))))

(defun c-quick-delete-region ()
  (interactive)
  (c-quick-operate-on-region-or-sexp #'delete-region))

(defun c-quick-indent-region ()
  (interactive)
  (c-quick-operate-on-region-or-sexp #'indent-region))

(defun c-quick-kill-region ()
  (interactive)
  (c-quick-operate-on-region-or-sexp #'kill-region))

(defun c-quick-mark-sexp ()
  (interactive)
  (if (eq last-command this-command)
      (c-quick-forward-sexp)
    (let ((opoint (point)))
      (set-mark (point))
      (c-quick-forward-sexp))))

(provide 'c-quick-2)
;;; c-quick-2.el ends here
