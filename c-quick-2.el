;; -*- coding: utf-8 -*-
;;; c-quick-2.el --- Intelligent Cursor Movement for GNU Emacs
;;
;; Copyright (C) 1993-2014  akmiyoshi
;;
;; Author: akmiyoshi
;; URL: https://github.com/akmiyoshi/c-quick/
;; Keywords: lisp, clojure
;; Version: 2.0.0
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

(require 'find-func)

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
(global-set-key (kbd "C-M-a")      'c-quick-beginning-of-defun)
(global-set-key (kbd "C-M-e")      'c-quick-end-of-defun)
(global-set-key (kbd "C-M-h")      'c-quick-mark-defun)
(global-set-key (kbd "<C-tab>")    'c-quick-rotate-file-buffer)
(global-set-key (kbd "C-M-p")      'c-quick-parse-region)

(global-set-key (kbd "<C-right>")  'c-quick-right-quick)
(global-set-key (kbd "<C-left>")   'c-quick-left-quick)
(global-set-key (kbd "<C-up>")     'c-quick-up-quick)
(global-set-key (kbd "<C-down>")   'c-quick-down-quick)

;; (global-set-key (kbd "C-x C-x")     'c-quick-jump-to-function)
(global-set-key (kbd "M-j")        'c-quick-jump-to-function)

;;;; Customization

(defgroup c-quick nil
  "c-quick."
  :group 'c-quick
  :prefix "c-quick-")

(defcustom c-quick-ding-dings t ""
  :group 'c-quick
  :type  'boolean)

(defcustom c-quick-paren-only t ""
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
  (force-mode-line-update)
  (when (input-pending-p) (discard-input)))

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

(defun c-quick-right-quick ()
  (interactive)
  (c-quick-forward-sexp)
  (c-quick-recenter))

(defun c-quick-left-quick ()
  (interactive)
  (c-quick-backward-sexp)
  (c-quick-recenter))

(defun c-quick-up-quick ()
  (interactive)
  (beginning-of-defun)
  (c-quick-recenter 'dummy))

(defun c-quick-down-quick ()
  (interactive)
  (end-of-defun)
  (c-quick-recenter 'dummy))

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
  (interactive)
  (cond
   ((eobp) (c-quick-ding))
   ((looking-at "\\s)") (c-quick-ding))
   ((looking-at "\\s-*\\s<")
    (let ((opoint (point)))
      (forward-line)
      (while (looking-at "\\s-*\\s<")
        (setq opoint (point))
        (forward-line))
      (goto-char (max opoint (save-excursion (beginning-of-line) (point))))))
   ((looking-at "\\s-") (while (looking-at "\\s-") (forward-char)))
   ((looking-at "\n")
    (let ((bol? (bolp)))
      (forward-char)
      (when bol?
        (while (and (bolp) (looking-at "\n"))
          (forward-char)))))
   (t (ignore-errors (forward-sexp)))))

(defun c-quick-backward-sexp ()
  (interactive)
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
    (let ((found (c-quick-find-comment (point))))
      (if found
          (goto-char found)
        (while (and (bolp) (looking-back "\n")
                    (save-excursion (backward-char) (bolp)))
          (backward-char)))))
   (t (ignore-errors (backward-sexp)))))

(defun c-quick-find-comment (eol)
  (save-excursion
    (goto-char eol)
    (while (looking-back "\\s-")
      (backward-char))
    (setq eol (point))
    (if (looking-back "\\s\"")
        nil
      (beginning-of-line)
      (let ((found nil))
        (while (and (not found) (< (point) eol))
          (cond
           ((looking-at "\\s\"") (forward-sexp)) ;; 文字列クォート(ダブルクオート)
           ((looking-at "\\sw") (forward-sexp))  ;; 単語構成文字(大小英文字、数字)
           ((looking-at "\\s_") (forward-sexp))  ;; $&*+-_<>
           ((looking-at "\\s'") (forward-char))  ;; 式前置子
           ((looking-at "\\s(") (forward-char))  ;; ([{
           ((looking-at "\\s)") (forward-char))  ;; )]}
           ((looking-at "\\s-*\\s<") (setq found (point))) ;; コメント開始
           ((looking-at "\\s-") (forward-char))  ;; 白文字(whitespace character)
           (t (forward-char))))
        found))))

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

(defun c-quick-recenter (dir)
  (cond
   ((pos-visible-in-window-p (point)) nil)
   ((< (point) (window-start)) (recenter 0))
   (t (recenter -1))))

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

(defun c-quick-mark-defun ()
  (interactive)
  (if (eq last-command this-command)
      (c-quick-end-of-defun) ;;(c-quick-forward-sexp)
    (c-quick-beginning-of-defun)
    (let ((beg (point)) end)
      (set-mark beg)
      (c-quick-end-of-defun)
      (setq end (point)))))

(defun c-quick-rotate-file-buffer ()
  (interactive)
  (let ((bufflist (buffer-list))
        (bufforig (current-buffer))
        (found nil)
        currbuff buffname)
    (while (and bufflist (not found))
      (setq currbuff (pop bufflist))
      (setq buffname (buffer-name currbuff))
      (cond
       ((eq bufforig currbuff) nil)
       ((minibufferp currbuff) nil)         ;; minibuffer
       ((string-match "^[*]" buffname) nil) ;; *scratch*, *Help* etc
       ((string-match "^[ ]" buffname) nil) ;; work buffer
       (t (setq found currbuff))))
    (if (not found)
        (ding)
      (switch-to-buffer found)
      (bury-buffer found))))

;;;; Testing

(defvar _c-quick-parse-data_ nil)
(defun c-quick-parse-region ()
  (interactive)
  (c-quick-operate-on-region-or-sexp
   #'(lambda (beg end)
       ;; (setq _c-quick-parse-data_ (parse-partial-sexp beg end))
       (setq _c-quick-parse-data_ (scan-sexps beg 1))
       )))

(defun c-quick-jump-to-function ()
  (interactive)
  (let* ((func-name (find-tag-default))
         (interned (intern func-name)))
    (message "func-name: %s" func-name)
    (if (c-quick-is-built-in-func interned)
        (error "%s is a built-in function(test)" interned)
      (or
       (and (fboundp interned)
            (find-function-do-it interned nil 'switch-to-buffer))
       (and (boundp interned)
            (find-function-do-it interned 'defvar 'switch-to-buffer)))
      )
    ))

(defun c-quick-is-built-in-func (function)
  (if (not (fboundp function))
      nil
  (let ((def (symbol-function (find-function-advised-original function))))
    (while (symbolp def)
      (setq function (symbol-function (find-function-advised-original function))
            def (symbol-function (find-function-advised-original function))))
    (subrp def))))

(provide 'c-quick-2)
;;; c-quick-2.el ends here
