;; -*- coding: utf-8 -*-
;;; c-quick-2.el --- Intelligent Cursor Movement for GNU Emacs
;;
;; Copyright (C) 1993-2014  akmiyoshi
;;
;; Author: akmiyoshi
;; URL: https://github.com/akmiyoshi/c-quick/
;; Keywords: lisp, scheme, clojure
;; Version: 0.9.1
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

;;;; Customization

(defgroup c-quick nil
  "c-quick."
  :group 'c-quick
  :prefix "c-quick-")

(defcustom c-quick-ding-dings t ""
  :group 'c-quick
  :type  'boolean)

(defcustom c-quick-paren-only nil ""
  :group 'c-quick
  :type  'boolean)

;;;; Internal Variables

(defvar _c-quick-mode_is_on_ nil)

;;;; Functions

(defun c-quick-toggle-mode ()
  (interactive)
  (setq _c-quick-mode_is_on_ (not _c-quick-mode_is_on_))
  (c-quick-set-mode _c-quick-mode_is_on_)
  (cond
   (_c-quick-mode_is_on_ (message "c-quick-mode is ON"))
   (t (message "c-quick-mode is OFF"))))

(defun c-quick-set-mode (arg)
  (if (not arg)
      (show-paren-mode 0)
    (setq show-paren-style
          (if c-quick-paren-only 'parenthesis 'expression))
    (setq show-paren-delay 0)
    (show-paren-mode 1)))

(defun c-quick-mode ()
  (and
   (not (window-minibuffer-p (selected-window)))
   _c-quick-mode_is_on_))

(defun c-quick-ding ()
  (if c-quick-ding-dings (ding)))

(defun c-quick-redisplay (dir)
  (c-quick-recenter dir)
  (c-quick-show-info)
  (force-mode-line-update))

(defun c-quick-down-key ()
  (interactive)
  (if (c-quick-mode)
      (c-quick-slide-down)
    (c-quick-next-line)
    )
  (c-quick-redisplay 'down))

(defun c-quick-up-key ()
  (interactive)
  (if (c-quick-mode)
      (c-quick-slide-up)
    (c-quick-previous-line)
    )
  (c-quick-redisplay 'up))

(defun c-quick-right-key ()
  (interactive)
  (if (c-quick-mode)
      (c-quick-forward-sexp)
    (c-quick-forward-char))
  (c-quick-redisplay 'down))

(defun c-quick-left-key ()
  (interactive)
  (if (c-quick-mode)
      (c-quick-backward-sexp)
    (c-quick-backward-char))
  (c-quick-redisplay 'up))

(defun c-quick-slide-down ()
  (if (not (bolp))
      (forward-char)
    (if (eobp) (c-quick-ding) (forward-line 1))))

(defun c-quick-slide-up ()
  (if (not (bolp))
      (backward-char)
    (if (bobp) (c-quick-ding) (forward-line -1))))

(defun c-quick-forward-char ()
  (if (eobp)
      (c-quick-ding)
    (forward-char)))

(defun c-quick-backward-char ()
  (if (bobp)
      (c-quick-ding)
    (backward-char)))

(defun c-quick-next-line ()
  (if (save-excursion (end-of-line) (eobp))
      (c-quick-ding)
    (next-line)))

(defun c-quick-previous-line ()
  (if (save-excursion (beginning-of-line) (bobp))
      (c-quick-ding)
    (previous-line)))

(defun c-quick-forward-sexp ()
  (cond
   ((eobp) (c-quick-ding))
   ((looking-at "\\s)") (c-quick-ding))
   ((looking-at "\\s-*\\s<")
    (forward-line)
    (while (looking-at "\\s-*\\s<") (forward-line)))
   ((looking-at "\\s-") (while (looking-at "\\s-") (forward-char)))
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
       ((looking-at
         "\\(\\s-*\\)\\(\\sw\\|\\s_\\|\\s(\\|\\s<\\|\\s\"\\|\\s'\\)")
        (goto-char (match-end 1))
        (let ((opoint (point)))
          (c-quick-forward-sexp)
          (c-quick-count-lines opoint (point))))))))

(defun c-quick-count-lines (start end)
  (let ((lines (count-lines start end)))
    (if (= lines 1) (message "1 line.") (message "%s lines." lines))))

(defun c-quick-window-end ()
  (let ((wend (window-end)))
    (save-excursion
      (goto-char wend)
      (forward-line -1)
      (backward-char)
      (point))))

(defun c-quick-recenter (dir)
  (cond
   ((pos-visible-in-window-p (point)) nil)
   ((eq dir 'up) (when (< (point) (window-start)) (recenter 0)))
   ((eq dir 'down) (when (> (point) (c-quick-window-end)) (recenter -1)))
   (t
    ;; to be deleted.
    (cond
     ((< (point) (window-start)) (recenter 0))
     ((> (point) (c-quick-window-end)) (recenter -1))))))

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
    (set-mark (point))
    (c-quick-forward-sexp)))

(provide 'c-quick-2)
;;; c-quick-2.el ends here

;; [参考文献]
;;
;; 3.1 Integer Basics
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Integer-Basics.html
;;
;; 28.19 The Window Start and End Positions
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Start-and-End.html
;;
;; 34.2.1 構文クラス一覧
;; http://www.geocities.co.jp/SiliconValley-Bay/9285/ELISP-JA/elisp_565.html
