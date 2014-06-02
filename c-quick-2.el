;; -*- coding: utf-8 -*-
;;; c-quick-2.el --- Intelligent Cursor Movement for GNU Emacs/XEmacs
;;
;; Copyright (C) 1993-2014  akmiyoshi
;;
;; Author: akmiyoshi
;; URL: https://github.com/akmiyoshi/c-quick/
;; Keywords: lisp, clojure
;; Version: 2.0.15
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

(require 'eieio)
(require 'etags)
(require 'find-func)


(global-set-key (kbd "<down>")     'cq-down-key)
(global-set-key (kbd "<up>")       'cq-up-key)
(global-set-key (kbd "<right>")    'cq-right-key)
(global-set-key (kbd "<left>")     'cq-left-key)
(global-set-key (kbd "C-z")        'cq-toggle-mode)
(global-set-key (kbd "M-w")        'cq-copy-region)
(global-set-key (kbd "C-w")        'cq-kill-region)
(global-set-key (kbd "C-M-\\")     'cq-indent-region)
(global-set-key (kbd "<C-delete>") 'cq-delete-region)
(global-set-key (kbd "C-M-SPC")    'cq-mark-sexp)
(global-set-key (kbd "C-M-@")      'cq-mark-sexp)
(global-set-key (kbd "C-M-a")      'cq-beginning-of-defun)
(global-set-key (kbd "C-M-e")      'cq-end-of-defun)
(global-set-key (kbd "C-M-h")      'cq-mark-defun)
(global-set-key (kbd "<C-tab>")    'cq-rotate-buffer-for-file)

(global-set-key (kbd "<C-right>")  'cq-right-quick)
(global-set-key (kbd "<C-left>")   'cq-left-quick)
(global-set-key (kbd "<C-up>")     'cq-up-quick)
(global-set-key (kbd "<C-down>")   'cq-down-quick)

(global-set-key (kbd "C-M-.")      'cq-jump-to-function-or-variable)
(global-set-key (kbd "C-x C-x")    'cq-exchange-point-and-mark)

;;;; Customization

(defgroup c-quick nil
  "c-quick."
  :group 'c-quick
  :prefix "cq-")

(defcustom cq-ding-dings t ""
  :group 'c-quick
  :type  'boolean)

(defcustom cq-paren-only t ""
  :group 'c-quick
  :type  'boolean)

(defcustom cq-whitespace t ""
  :group 'c-quick
  :type  'boolean)

;;;; Internal Variables

(defvar _cq-mode_is_on_ nil)

;;;; Classes

(defclass <cq-lexer-for-lisp> () ())
(defclass <cq-lexer-for-C>    () ())

(setq *cq-lexer-for-lisp* (<cq-lexer-for-lisp> "<cq-lexer-for-lisp>"))
(setq *cq-lexer-for-C*    (<cq-lexer-for-C>    "<cq-lexer-for-C>"))

(setq cq-lexer-alist nil)

(add-to-list 'cq-lexer-alist
             (list 'emacs-lisp-mode *cq-lexer-for-lisp*))

(add-to-list 'cq-lexer-alist
             (list 'lisp-interaction-mode *cq-lexer-for-lisp*))

(add-to-list 'cq-lexer-alist
             (list 'clojure-mode *cq-lexer-for-lisp*))

(add-to-list 'cq-lexer-alist
             (list 'js-mode *cq-lexer-for-C*))

(add-to-list 'cq-lexer-alist
             (list 'js2-mode *cq-lexer-for-C*))

(defmethod !cq-forward-1exp ((this <cq-lexer-for-lisp>)
                             &optional $limit)
  (let (($opoint (point)))
    (cond
     ((eobp) (cq-ding))
     ((and (not $limit) (cq-within-string (point)))
      (cq-forward-within-string))
     ((and (not $limit) (cq-within-comment (point)))
      (cq-forward-within-comment))
     ((and $limit (looking-at "\\s-+")) (goto-char (match-end 0)))
     ((and $limit (looking-at "\\s<+")) (goto-char (match-end 0)))
     ((looking-at "\\s-*\\s<")
      (let (($opoint (point)))
        (forward-line)
        (while (looking-at "\\s-*\\s<")
          (setq $opoint (point))
          (forward-line))
        (goto-char (max $opoint (save-excursion (beginning-of-line) (point))))))
     ((looking-at "\\s-") (while (looking-at "\\s-") (forward-char)))
     ((looking-at "\n")
      (let (($bolp (bolp)))
        (forward-char)
        (when $bolp
          (while (and (bolp) (looking-at "\n"))
            (forward-char)))))
     (t (condition-case err (forward-sexp) (error (cq-ding)))))
    (when (and $limit (> (point) $limit))
      (cq-ding)
      (goto-char $opoint))))

(defmethod !cq-backward-1exp ((this <cq-lexer-for-lisp>)
                              &optional $limit)
  (let (($opoint (point)) $comment-begin)
    (cond
     ((bobp) (cq-ding))
     ((and (not $limit) (cq-within-string (point)))
      (cq-backward-within-string))
     ((and (not $limit) (cq-within-comment (point)))
      (cq-backward-within-comment))
     ((and (cq-looking-back "\\s>")
           (save-excursion
             (backward-char)
             (setq $comment-begin
                   (cq-find-comment-beginning (point)))))
      (goto-char $comment-begin)
      (while (and (cq-looking-back "\\s>")
                  (save-excursion
                    (backward-char)
                    (setq $comment-begin
                          (cq-find-comment-beginning (point)))))
        (goto-char $comment-begin)))
     ((cq-looking-back "\\s-")
      (while (cq-looking-back "\\s-") (backward-char)))
     ((cq-looking-back "\\s<")
      (while (cq-looking-back "\\s<") (backward-char)))
     ((cq-looking-back "\n")
      (backward-char)
      (while (and (bolp) (cq-looking-back "\n")
                  (save-excursion (backward-char) (bolp)))
        (backward-char)))
     (t (condition-case $err (backward-sexp) (error (cq-ding)))))
    (when (and $limit (< (point) $limit))
      (cq-ding)
      (goto-char $opoint))))

(defmethod !cq-forward-1exp ((this <cq-lexer-for-C>)
                             &optional $limit)
  (let (($opoint (point)))
    (cond
     ((eobp) (cq-ding))
     ((and (not $limit) (cq-within-string (point)))
      (cq-forward-within-string))
     ((and (not $limit) (cq-within-comment (point)))
      (cq-forward-within-comment))
     ((and $limit (looking-at "\\s-+")) (goto-char (match-end 0)))
     ((and $limit (looking-at "\\s<+")) (goto-char (match-end 0)))
     ((looking-at "\\s-*//")
      (let (($opoint (point)))
        (forward-line)
        (while (looking-at "\\s-*//")
          (setq $opoint (point))
          (forward-line))
        (goto-char (max $opoint (save-excursion (beginning-of-line) (point))))))
     ((looking-at "\\s-") (while (looking-at "\\s-") (forward-char)))
     ((looking-at "/[*]") (search-forward "*/"))
     ((looking-at "\\s.+") (goto-char (match-end 0)))
     ((looking-at "\n")
      (let (($bol (bolp)))
        (forward-char)
        (when $bol
          (while (and (bolp) (looking-at "\n"))
            (forward-char)))))
     (t (condition-case $err (forward-sexp) (error (cq-ding)))))
    (when (and $limit (> (point) $limit))
      (cq-ding)
      (goto-char $opoint))))

(defmethod !cq-backward-1exp ((this <cq-lexer-for-C>)
                              &optional $limit)
  (let (($opoint (point)) comment-begin)
    (cond
     ((bobp) (cq-ding))
     ((and (not $limit) (cq-within-string (point)))
      (cq-backward-within-string))
     ((and (not $limit) (cq-within-comment (point)))
      (cq-backward-within-comment))
     ((and (cq-looking-back "\\s>")
           (save-excursion
             (backward-char)
             (setq comment-begin
                   (cq-find-comment-beginning (point)))))
      (goto-char comment-begin)
      (while (and (cq-looking-back "\\s>")
                  (save-excursion
                    (backward-char)
                    (setq comment-begin
                          (cq-find-comment-beginning (point)))))
        (goto-char comment-begin)))
     ((cq-looking-back "[*]/")
      (search-backward "/*")
      (while (cq-within-comment (point))
        (search-backward "/*")))
     ((cq-looking-back "\\s.+")
      (goto-char (match-beginning 0)))
     ((cq-looking-back "\\s-")
      (while (cq-looking-back "\\s-") (backward-char)))
     ((cq-looking-back "\\s<")
      (while (cq-looking-back "\\s<") (backward-char)))
     ((cq-looking-back "\n")
      (backward-char)
      (while (and (bolp) (cq-looking-back "\n")
                  (save-excursion (backward-char) (bolp)))
        (backward-char)))
     (t (condition-case $err (backward-sexp) (error (cq-ding)))))
    (when (and $limit (< (point) $limit))
      (cq-ding)
      (goto-char $opoint))))

;;;; Functions

(defun cq-toggle-mode ()
  (interactive)
  (setq _cq-mode_is_on_ (not _cq-mode_is_on_))
  (cq-set-mode _cq-mode_is_on_)
  (cond
   (_cq-mode_is_on_ (message "c-quick-mode is ON"))
   (t (message "c-quick-mode is OFF")))
  (cq-extend-region-for-xemacs))

(defun cq-set-mode ($arg)
  (if (not $arg)
      (progn
        (and cq-whitespace
             (fboundp 'global-whitespace-mode) (global-whitespace-mode 0))
        (and (fboundp 'show-paren-mode) (show-paren-mode 0)))
    (and cq-whitespace
         (fboundp 'global-whitespace-mode) (global-whitespace-mode 1))
    (setq show-paren-style
          (if cq-paren-only 'parenthesis 'expression))
    (setq show-paren-delay 0)
    (and (fboundp 'show-paren-mode) (show-paren-mode 1))))

(defun cq-mode ()
  (and
   (not (window-minibuffer-p (selected-window)))
   _cq-mode_is_on_))

(defun cq-ding ()
  (if cq-ding-dings (ding)))

(defun cq-looking-back ($regexp &optional $limit $greedy)
  (let (($start (point))
        ($pos
         (save-excursion
           (and (re-search-backward (concat "\\(?:" $regexp "\\)\\=") $limit t)
                (point)))))
    (if (and $greedy $pos)
        (save-restriction
          (narrow-to-region (point-min) $start)
          (while (and (> $pos (point-min))
                      (save-excursion
                        (goto-char $pos)
                        (backward-char 1)
                        (looking-at (concat "\\(?:"  $regexp "\\)\\'"))))
            (setq $pos (1- $pos)))
          (save-excursion
            (goto-char $pos)
            (looking-at (concat "\\(?:"  $regexp "\\)\\'")))))
    (not (null $pos))))

(defun cq-activate-region-for-xemacs ()
  (and (fboundp 'activate-region) (activate-region)))

(defun cq-extend-region-for-xemacs ()
  (and (fboundp 'activate-region)
       (region-active-p)
       (activate-region)))

(defun cq-redisplay ()
  (cq-recenter)
  (cq-show-info)
  (force-mode-line-update)
  (when (input-pending-p) (discard-input)))

(defun cq-down-key ()
  (interactive)
  (if (cq-mode)
      (cq-slide-down)
    (cq-next-line))
  (cq-extend-region-for-xemacs)
  (cq-redisplay))

(defun cq-up-key ()
  (interactive)
  (if (cq-mode)
      (cq-slide-up)
    (cq-previous-line))
  (cq-extend-region-for-xemacs)
  (cq-redisplay))

(defun cq-right-key ()
  (interactive)
  (if (cq-mode)
      (cq-forward-1exp)
    (cq-forward-char))
  (cq-extend-region-for-xemacs)
  (cq-redisplay))

(defun cq-left-key ()
  (interactive)
  (if (cq-mode)
      (cq-backward-1exp)
    (cq-backward-char))
  (cq-extend-region-for-xemacs)
  (cq-redisplay))

(defun cq-right-quick ()
  (interactive)
  (cq-forward-1exp)
  (cq-extend-region-for-xemacs)
  (cq-recenter))

(defun cq-left-quick ()
  (interactive)
  (cq-backward-1exp)
  (cq-extend-region-for-xemacs)
  (cq-recenter))

(defun cq-up-quick ()
  (interactive)
  (beginning-of-defun)
  (cq-extend-region-for-xemacs)
  (recenter))

(defun cq-down-quick ()
  (interactive)
  (end-of-defun)
  (cq-extend-region-for-xemacs)
  (recenter))

(defun cq-slide-down ()
  (if (not (bolp))
      (forward-char)
    (if (eobp) (cq-ding) (forward-line 1))))

(defun cq-slide-up ()
  (if (not (bolp))
      (backward-char)
    (if (bobp) (cq-ding) (forward-line -1))))

(defun cq-forward-char ()
  (if (eobp)
      (cq-ding)
    (forward-char)))

(defun cq-backward-char ()
  (if (bobp)
      (cq-ding)
    (backward-char)))

(defun cq-next-line ()
  (setq this-command 'next-line)
  (if (save-excursion (end-of-line) (eobp))
      (cq-ding)
    (next-line 1)))

(defun cq-previous-line ()
  (setq this-command 'previous-line)
  (if (save-excursion (beginning-of-line) (bobp))
      (cq-ding)
    (previous-line 1)))

(defun cq-forward-1exp (&optional $limit)
  (interactive)
  (let (($syntax (assoc major-mode cq-lexer-alist)))
    (if (not $syntax)
        (cq-forward-char)
      (!cq-forward-1exp (nth 1 $syntax) $limit))))

(defun cq-backward-1exp (&optional $limit)
  (interactive)
  (let (($syntax (assoc major-mode cq-lexer-alist)))
    (if (not $syntax)
        (cq-backward-char)
      (!cq-backward-1exp (nth 1 $syntax) $limit))))

(defun cq-within-string ($pos)
  (save-excursion
    (goto-char $pos)
    (let (($ppss (cq-syntax-ppss))
          $beg $end)
      (if (not (nth 3 $ppss))
          nil
        (setq $beg (nth 8 $ppss))
        (goto-char $beg)
        (forward-sexp)
        (setq $end (point))
        (list (1+ $beg) (1- $end))))))

(defun cq-forward-within-string ()
  (let (($parsed (cq-within-string (point))))
    (if (>= (point) (nth 1 $parsed))
        (cq-ding)
      (cq-forward-1exp (nth 1 $parsed)))))

(defun cq-backward-within-string ()
  (let (($parsed (cq-within-string (point))))
    (if (<= (point) (nth 0 $parsed))
        (cq-ding)
      (cq-backward-1exp (nth 0 $parsed)))))

(defun cq-within-comment ($pos)
  (save-excursion
    (goto-char $pos)
    (let (($ppss (cq-syntax-ppss))
          ($is-line-comment t))
      (if (not (nth 4 $ppss))
          nil
        (goto-char (nth 8 $ppss))
        (cond
         ((looking-at "//") ;; C/C++/Java/JavaScript
          (while (looking-at "/")
            (forward-char)))
         ((looking-at "/[*]") ;; C/C++/Java/JavaScript
          (setq $is-line-comment nil)
          (goto-char (match-end 0)))
         (t (while (looking-at "\\s<")
              (forward-char))))
        (list (nth 8 $ppss)
              (point)
              (progn
                (if $is-line-comment
                    (end-of-line)
                  (search-forward "*/")
                  (goto-char (match-beginning 0)))
                (point)))))))

(defun cq-forward-within-comment ()
  (let ($within-comment)
    (cond
     ((and
       (looking-at "\\s>")
       (save-excursion
         (forward-line)
         (end-of-line)
         (setq $within-comment
               (cq-within-comment (point)))))
      (goto-char (nth 1 $within-comment)))
     ((looking-at "\\s>")
      (cq-ding))
     (t
      (setq $within-comment
            (cq-within-comment (point)))
      (cq-forward-1exp (nth 2 $within-comment))))))

(defun cq-backward-within-comment ()
  (let (($within-comment (cq-within-comment (point))))
    (cond
     ((and
       (<= (point) (nth 1 $within-comment))
       (save-excursion
         (beginning-of-line)
         (and
          (cq-looking-back "\\s>")
          (progn
            (backward-char)
            (cq-within-comment (point))))))
      (beginning-of-line)
      (backward-char))
     ((<= (point) (nth 1 $within-comment))
      (cq-ding))
     (t (cq-backward-1exp (nth 1 $within-comment))))))

(defun cq-find-comment-beginning ($eol)
  (save-excursion
    (goto-char $eol)
    (let (($parsed (cq-within-comment (point))))
      (if (not $parsed)
          nil
        (goto-char (nth 0 $parsed))
        (while (cq-looking-back "\\s-") (backward-char))
        (point)))))

(defun cq-show-info ()
  (when (and (cq-mode) (not (cq-within-comment (point))))
    (save-excursion
      (let ((cq-ding-dings nil))
        (cond
         ((cq-within-string (point)) nil)
         ((cq-looking-back "\\s)\\|\\s\"\\|\\sw\\|\\s_")
          (let (($opoint (point)))
            (cq-backward-1exp)
            (cq-count-lines $opoint (point))))
         ((looking-at
           "\\(\\s-*\\)\\(\\sw\\|\\s_\\|\\s(\\|\\s<\\|\\s\"\\|\\s'\\)")
          (goto-char (match-end 1))
          (let (($opoint (point)))
            (cq-forward-1exp)
            (cq-count-lines $opoint (point)))))))))

(defun cq-count-lines ($start $end)
  (let ((lines (count-lines $start $end)))
    (if (= lines 1) (message "1 line.") (message "%s lines." lines))))

(defun cq-recenter ()
  (cond
   ((pos-visible-in-window-p (point)) nil)
   ((< (point) (window-start)) (recenter 0))
   (t (recenter -1))))

(defun cq-operate-on-region-or-sexp (op)
  (interactive)
  (funcall op
           (point)
           (if (region-active-p)
               (mark)
             (cq-forward-1exp)
             (point))))

(defun cq-copy-region ()
  (interactive)
  (cq-operate-on-region-or-sexp
   #'(lambda ($beg $end)
       (kill-ring-save $beg $end)
       (setq this-command 'kill-region))))

(defun cq-delete-region ()
  (interactive)
  (cq-operate-on-region-or-sexp #'delete-region))

(defun cq-indent-region ()
  (interactive)
  (cq-operate-on-region-or-sexp
   #'(lambda ($beg $end)
       (indent-region $beg $end nil))))

(defun cq-kill-region ()
  (interactive)
  (cq-operate-on-region-or-sexp #'kill-region))

(defun cq-mark-sexp ()
  (interactive)
  (if (eq last-command this-command)
      (cq-forward-1exp)
    (set-mark (point))
    (cq-forward-1exp))
  (cq-activate-region-for-xemacs))

(defun cq-mark-defun ()
  (interactive)
  (if (eq last-command this-command)
      (end-of-defun)
    (unless (looking-at "^\\s(")
      (beginning-of-defun))
    (set-mark (point))
    (end-of-defun))
  (cq-activate-region-for-xemacs))

(defun cq-rotate-buffer-for-file ()
  (interactive)
  (let (($bufflist (buffer-list))
        ($bufforig (current-buffer))
        ($found nil)
        $currbuff $buffname)
    (while (and $bufflist (not $found))
      (setq $currbuff (pop $bufflist))
      (setq $buffname (buffer-name $currbuff))
      (cond
       ((eq $bufforig $currbuff) nil)
       ((string-match "^[ ]" $buffname) nil);; minibuffer
       ((string-match "^[*]" $buffname) nil);; *scratch*, *Help*, etc
       (t (setq $found $currbuff))))
    (if (not $found)
        (ding)
      (switch-to-buffer $found)
      (if (featurep 'xemacs)
          (bury-buffer $found nil)
        (bury-buffer $found)))))

;;;; Testing

(defun cq-jump-to-function-or-variable ()
  (interactive)
  (let* (($funcname (find-tag-default))
         ($interned (intern $funcname)))
    (cond
     ((cq-built-in-function-p $interned)
      (describe-function $interned))
     ((fboundp $interned)
      (find-function $interned))
     ((user-variable-p $interned)
      (find-variable $interned))
     ((boundp $interned)
      (describe-variable $interned))
     (t (error "%s is not a lisp function nor a user variable" $interned)))))

(defun cq-built-in-function-p ($symbol)
  ;; (require 'cl) (assert (symbolp $symbol))
  (if (not (fboundp $symbol))
      nil
    (subrp
     (symbol-function
      (cq-find-function-advised-original $symbol)))))

(defun cq-find-function-advised-original ($func)
  "Return the original function symbol of an advised function FUNC.
If FUNC is not the symbol of an advised function, just returns FUNC."
  (or (and (symbolp $func)
           (featurep 'advice)
           (let (($ofunc (cdr (assq 'origname (ad-get-advice-info $func)))))
             (and (fboundp $ofunc) $ofunc)))
      $func))

(defun cq-exchange-point-and-mark ($arg)
  (interactive "P")
  (let (($active (region-active-p)))
    (exchange-point-and-mark $arg)
    (when (not $active)
      (and (fboundp 'deactivate-mark) (deactivate-mark))
      (and (fboundp 'zmacs-deactivate-region) (zmacs-deactivate-region)))))

;; http://www.loveshack.ukfsn.org/emacs/syntax.el
;;
;;; syntax.el --- helper functions to find syntactic context
;;
;; Copyright (C) 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006 Free Software Foundation, Inc.
;;
;; Maintainer: FSF
;; Keywords: internal
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The main exported function is `cq-syntax-ppss'.  You might also need
;; to call `cq-syntax-ppss-flush-cache' or to add it to
;; `cq-before-change-functions'(although this is automatically done by
;; cq-syntax-ppss when needed, but that might fail if cq-syntax-ppss is
;; called in a context where cq-before-change-functions is temporarily
;; let-bound to nil).

;;; Todo:

;; - do something about the case where the syntax-table is changed.
;;   This typically happens with tex-mode and its `$' operator.
;; - move font-lock-syntactic-keywords in here.  Then again, maybe not.
;; - new functions `syntax-state', ... to replace uses of parse-partial-state
;;   with something higher-level (similar to cq-syntax-ppss-context).
;; - interaction with mmm-mode.

;;; Code:

;; Note: PPSS stands for `parse-partial-sexp state'

(eval-when-compile (require 'cl))

(defvar cq-font-lock-beginning-of-syntax-function)

(defvar cq-syntax-ppss-max-span 20000
  "Threshold below which cache info is deemed unnecessary.
We try to make sure that cache entries are at least this far apart
from each other, to avoid keeping too much useless info.")

(defvar cq-syntax-begin-function nil
  "Function to move back outside of any comment/string/paren.
This function should move the cursor back to some syntactically safe
point (where the PPSS is equivalent to nil).")

(defvar cq-syntax-ppss-cache nil
  "List of (POS . PPSS) pairs, in decreasing POS order.")
(make-variable-buffer-local 'cq-syntax-ppss-cache)
(defvar cq-syntax-ppss-last nil
  "Cache of (LAST-POS . LAST-PPSS).")
(make-variable-buffer-local 'cq-syntax-ppss-last)

(defalias 'cq-syntax-ppss-after-change-function 'cq-syntax-ppss-flush-cache)
(defun cq-syntax-ppss-flush-cache (beg &rest ignored)
  "Flush the cache of `cq-syntax-ppss' starting at position BEG."
  ;; Flush invalid cache entries.
  (while (and cq-syntax-ppss-cache (> (caar cq-syntax-ppss-cache) beg))
    (setq cq-syntax-ppss-cache (cdr cq-syntax-ppss-cache)))
  ;; Throw away `last' value if made invalid.
  (when (< beg (or (car cq-syntax-ppss-last) 0))
    ;; If cq-syntax-begin-function jumped to BEG, then the old state at BEG can
    ;; depend on the text after BEG (which is presumably changed).  So if
    ;; BEG=(car (nth 10 cq-syntax-ppss-last)) don't reuse that data because the
    ;; assumed nil state at BEG may not be valid any more.
    (if (<= beg (or (car (nth 10 cq-syntax-ppss-last))
                    (nth 9 cq-syntax-ppss-last)
                    (nth 3 cq-syntax-ppss-last)
                    0))
        (setq cq-syntax-ppss-last nil)
      (setcar cq-syntax-ppss-last nil)))
  ;; Unregister if there's no cache left.  Sadly this doesn't work
  ;; because `cq-before-change-functions' is temporarily bound to nil here.
  ;; (unless cq-syntax-ppss-cache
  ;;   (remove-hook 'cq-before-change-functions 'cq-syntax-ppss-flush-cache t))
  )

(defvar cq-syntax-ppss-stats
  [(0 . 0.0) (0 . 0.0) (0 . 0.0) (0 . 0.0) (0 . 0.0) (1 . 2500.0)])
(defun cq-syntax-ppss-stats ()
  (mapcar (lambda (x)
            (condition-case nil
                (cons (car x) (truncate (/ (cdr x) (car x))))
              (error nil)))
          cq-syntax-ppss-stats))

(defun cq-syntax-ppss (&optional pos)
  "Parse-Partial-Sexp State at POS.
The returned value is the same as `parse-partial-sexp' except that
the 2nd and 6th values of the returned state cannot be relied upon.
Point is at POS when this function returns.

The match data may be altered through running `cq-syntax-begin-function' (or
`cq-font-lock-beginning-of-syntax-function')."
  ;; Default values.
  (unless pos (setq pos (point)))
  ;;
  (let ((old-ppss (cdr cq-syntax-ppss-last))
        (old-pos (car cq-syntax-ppss-last))
        (ppss nil)
        (pt-min (point-min)))
    (if (and old-pos (> old-pos pos)) (setq old-pos nil))
    ;; Use the OLD-POS if usable and close.  Don't update the `last' cache.
    (condition-case nil
        (if (and old-pos (< (- pos old-pos)
                            ;; The time to use cq-syntax-begin-function and
                            ;; find PPSS is assumed to be about 2 * distance.
                            (* 2 (/ (cdr (aref cq-syntax-ppss-stats 5))
                                    (1+ (car (aref cq-syntax-ppss-stats 5)))))))
            (progn
              (incf (car (aref cq-syntax-ppss-stats 0)))
              (incf (cdr (aref cq-syntax-ppss-stats 0)) (- pos old-pos))
              (parse-partial-sexp old-pos pos nil nil old-ppss))

          (cond
           ;; Use OLD-PPSS if possible and close enough.
           ((and (not old-pos) old-ppss
                 ;; BEWARE! We rely on the undocumented 9th field.  The 9th
                 ;; field currently contains the list of positions of
                 ;; open-parens of the enclosing parens.  I.e. those
                 ;; positions are outside of any string/comment
                 ;; and the first of those is outside of any paren
                 ;; (i.e. corresponds to a nil ppss).  If this list is empty
                 ;; but we are in a string or comment, then the 8th field
                 ;; contains a similar "toplevel" position.  If `pt-min' is
                 ;; too far from `pos', we could try to use other positions
                 ;; in (nth 9 old-ppss), but that doesn't seem to happen in
                 ;; practice and it would complicate this code (and the
                 ;; before-change-function code even more).  But maybe it
                 ;; would be useful in "degenerate" cases such as when the
                 ;; whole file is wrapped in a set of parentheses.
                 (setq pt-min (or (car (nth 9 old-ppss))
                                  (nth 8 old-ppss)
                                  (nth 2 old-ppss)))
                 (<= pt-min pos) (< (- pos pt-min) cq-syntax-ppss-max-span))
            (incf (car (aref cq-syntax-ppss-stats 1)))
            (incf (cdr (aref cq-syntax-ppss-stats 1)) (- pos pt-min))
            (setq ppss (parse-partial-sexp pt-min pos)))
           ;; The OLD-* data can't be used.  Consult the cache.
           (t
            (let ((cache-pred nil)
                  (cache cq-syntax-ppss-cache)
                  (pt-min (point-min))
                  ;; I differentiate between PT-MIN and PT-BEST because
                  ;; I feel like it might be important to ensure that the
                  ;; cache is only filled with 100% sure data (whereas
                  ;; cq-syntax-begin-function might return incorrect data).
                  ;; Maybe that's just stupid.
                  (pt-best (point-min))
                  (ppss-best nil))
              ;; look for a usable cache entry.
              (while (and cache (< pos (caar cache)))
                (setq cache-pred cache)
                (setq cache (cdr cache)))
              (if cache (setq pt-min (caar cache) ppss (cdar cache)))

              ;; Setup the before-change function if necessary.
              (unless (or cq-syntax-ppss-cache cq-syntax-ppss-last)
                (add-hook 'cq-before-change-functions
                          'cq-syntax-ppss-flush-cache t t))

              ;; Use the best of OLD-POS and CACHE.
              (if (or (not old-pos) (< old-pos pt-min))
                  (setq pt-best pt-min ppss-best ppss)
                (incf (car (aref cq-syntax-ppss-stats 4)))
                (incf (cdr (aref cq-syntax-ppss-stats 4)) (- pos old-pos))
                (setq pt-best old-pos ppss-best old-ppss))

              ;; Use the `cq-syntax-begin-function' if available.
              ;; We could try using that function earlier, but:
              ;; - The result might not be 100% reliable, so it's better to use
              ;;   the cache if available.
              ;; - The function might be slow.
              ;; - If this function almost always finds a safe nearby spot,
              ;;   the cache won't be populated, so consulting it is cheap.
              (when (and (not cq-syntax-begin-function)
                         (if (boundp 'cq-font-lock-beginning-of-syntax-function)
                             cq-font-lock-beginning-of-syntax-function))
                (set (make-local-variable 'cq-syntax-begin-function)
                     cq-font-lock-beginning-of-syntax-function))
              (when (and cq-syntax-begin-function
                         (progn (goto-char pos)
                                (funcall cq-syntax-begin-function)
                                ;; Make sure it's better.
                                (> (point) pt-best))
                         ;; Simple sanity check.
                         (not (memq (get-text-property (point) 'face)
                                    '(font-lock-string-face font-lock-doc-face
                                      font-lock-comment-face))))
                (incf (car (aref cq-syntax-ppss-stats 5)))
                (incf (cdr (aref cq-syntax-ppss-stats 5)) (- pos (point)))
                (setq pt-best (point) ppss-best nil))

              (cond
               ;; Quick case when we found a nearby pos.
               ((< (- pos pt-best) cq-syntax-ppss-max-span)
                (incf (car (aref cq-syntax-ppss-stats 2)))
                (incf (cdr (aref cq-syntax-ppss-stats 2)) (- pos pt-best))
                (setq ppss (parse-partial-sexp pt-best pos nil nil ppss-best)))
               ;; Slow case: compute the state from some known position and
               ;; populate the cache so we won't need to do it again soon.
               (t
                (incf (car (aref cq-syntax-ppss-stats 3)))
                (incf (cdr (aref cq-syntax-ppss-stats 3)) (- pos pt-min))

                ;; If `pt-min' is too far, add a few intermediate entries.
                (while (> (- pos pt-min) (* 2 cq-syntax-ppss-max-span))
                  (setq ppss (parse-partial-sexp
                              pt-min (setq pt-min (/ (+ pt-min pos) 2))
                              nil nil ppss))
                  (let ((pair (cons pt-min ppss)))
                    (if cache-pred
                        (push pair (cdr cache-pred))
                      (push pair cq-syntax-ppss-cache))))

                ;; Compute the actual return value.
                (setq ppss (parse-partial-sexp pt-min pos nil nil ppss))

                ;; Debugging check.
                ;; (let ((real-ppss (parse-partial-sexp (point-min) pos)))
                ;;   (setcar (last ppss 4) 0)
                ;;   (setcar (last real-ppss 4) 0)
                ;;   (setcar (last ppss 8) nil)
                ;;   (setcar (last real-ppss 8) nil)
                ;;   (unless (equal ppss real-ppss)
                ;;     (message "!!Syntax: %s != %s" ppss real-ppss)
                ;;     (setq ppss real-ppss)))

                ;; Store it in the cache.
                (let ((pair (cons pos ppss)))
                  (if cache-pred
                      (if (> (- (caar cache-pred) pos) cq-syntax-ppss-max-span)
                          (push pair (cdr cache-pred))
                        (setcar cache-pred pair))
                    (if (or (null cq-syntax-ppss-cache)
                            (> (- (caar cq-syntax-ppss-cache) pos)
                               cq-syntax-ppss-max-span))
                        (push pair cq-syntax-ppss-cache)
                      (setcar cq-syntax-ppss-cache pair)))))))))

          (setq cq-syntax-ppss-last (cons pos ppss))
          ppss)
      (args-out-of-range
       ;; If the buffer is more narrowed than when we built the cache,
       ;; we may end up calling parse-partial-sexp with a position before
       ;; point-min.  In that case, just parse from point-min assuming
       ;; a nil state.
       (parse-partial-sexp (point-min) pos)))))


(provide 'c-quick-2)
;;; c-quick-2.el ends here
