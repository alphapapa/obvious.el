;;; obvious.el --- Who needs comments when the code is so obvious  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/obvious.el
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by Anders Lindgren's `nocomments-mode' (which works
;; differently, by preserving whitespace where comments are):
;; <https://github.com/Lindydancer/nocomments-mode>.

;;; Code:

;;;; Requirements

(require 'cl-lib)

;; TODO: Option to keep showing the first line of comments.

;; FIXME: Adjust overlay advance settings, because currently,
;; modifying the buffer contents seems to unintentionally overwrite
;; comments sometimes.

;;;; Variables

(defvar-local obvious-overlays nil
  "Overlays used to hide comments.")

(defvar obvious-font-lock-keywords
  '((;; MATCHER
     obvious-matcher
     ;; HIGHLIGHTERS
     (0 ;; SUBEXP
      ;; FACESPEC
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
        (when obvious-fringe
          (obvious-fringe overlay))
        (cl-loop for (property . value) in obvious-overlay-properties
                 do (overlay-put overlay property value))
        (push overlay obvious-overlays)
        ;; Return nil.
        nil))))
  "Font-lock keywords used in `obvious-mode'.")

(defvar font-lock-end)

;;;; Customization

(defgroup obvious nil
  "Who needs comments when the code is so obvious."
  :group 'tools
  :link '(url-link "https://github.com/alphapapa/obvious.el")
  :link '(emacs-library-link "obvious"))

(defcustom obvious-headers t
  "Keep file headers visible.
That is, don't hide comments at the beginning of the file, before
any code."
  :type 'boolean)

(defcustom obvious-fringe t
  "Indicate comments in the fringe."
  :type 'boolean)

(defcustom obvious-preserve-blank-lines t
  "Preserve newline characters.
Otherwise, when a comment runs to the end of a line, the newline
and following whitespace will be hidden."
  :type 'boolean)

(defcustom obvious-overlay-properties
  '((invisible . t))
  "Alist of properties to apply to comment overlays."
  :type '(choice (const :tag "Invisible" ((invisible . t)))
                 (alist :tag "Custom properties" :key-type sexp :value-type sexp)))

(define-fringe-bitmap 'obvious-fringe-bitmap
  ;; With thanks to the package `fringe-helper':
  ;; <http://nschum.de/src/emacs/fringe-helper/>.
  [0 24 24 0 0 24 24 48] nil nil 'center)

(defcustom obvious-fringe-bitmap 'obvious-fringe-bitmap
  "Fringe bitmap used to indicate comments."
  :type '(choice (const :tag "Semi-colon" obvious-fringe-bitmap)
                 (const :tag "Right arrow" right-arrow)
                 (symbol :tag "Specified fringe (see `(elisp) Fringe Bitmaps')")))

;;;;; Faces

(defface obvious-fringe-face '((t :inherit font-lock-comment-face))
  "Face applied to fringe bitmaps when so-configured.
See option `obvious-fringe'.")

;;;; Mode

;;;###autoload
(define-minor-mode obvious-mode
  "Hide comments, because what this code does is obvious."
  :lighter " Obvious"
  (if obvious-mode
      (progn
        (font-lock-add-keywords nil obvious-font-lock-keywords 'append)
        (font-lock-flush))
    ;; Disable mode.
    (font-lock-remove-keywords nil obvious-font-lock-keywords)
    (mapc #'delete-overlay obvious-overlays)
    (setf obvious-overlays nil)
    (font-lock-flush)))

;;;; Functions

(defun obvious-fringe (overlay)
  "Make OVERLAY display in the fringe."
  (overlay-put
   overlay 'before-string
   (propertize
    " " 'display `(left-fringe ,obvious-fringe-bitmap obvious-fringe-face))))

(defun obvious-matcher (limit)
  "Font-lock matching function.
Matches up to LIMIT."
  (cl-labels ((in-comment-p
               () (pcase-let ((`( _depth _list-start _lcst-start _stringp
                                  ,in-comment-p _after-quote _mpd _comment-style
                                  ,_comment-start _open-paren-poss _2cc-pos . _rest)
                               (syntax-ppss)))
                    in-comment-p))
              (looking-at-comment-p
               () (equal '(11) (syntax-after (point))))
              (point-at-boc
               (pos &optional limit)
               (save-excursion
                 (goto-char pos)
                 (cond ((in-comment-p)
                        (comment-search-backward limit t))
                       ((looking-at-comment-p)
                        (point))
                       (t (comment-search-forward limit t)))))
              (point-at-eoc
               (pos &optional limit)
               (ignore limit)
               (save-excursion
                 (goto-char (point-at-boc pos))
                 (forward-comment most-positive-fixnum)
                 (when (and obvious-preserve-blank-lines
                            (not (comment-only-line-p pos)))
                   (re-search-backward (rx (not space)))
                   (when (bolp)
                     (forward-line -1))
                   (goto-char (point-at-eol)))
                 ;; We intentionally ignore the limit.  It seems to be necessary.
                 (point)))
              (comment-only-line-p
               (pos) (save-excursion
                       (goto-char pos)
                       (looking-back (rx bol (0+ blank)) (point-min))))
              (comment-at-file-header-p
               (pos)
               (cond ((and (= (point-min) pos)
                           (save-excursion
                             (goto-char pos)
                             (or (looking-at-comment-p)
                                 (re-search-forward (rx (1+ space) (syntax comment-delimiter)) nil t))))
                      t)))
              (comment-header-p  ;; e.g. a Lisp-style header
               (pos) (save-excursion
                       (goto-char pos)
                       (looking-at-p (rx (>= 3 (syntax comment-start)))))))
    (let ((comment-start (point-at-boc (point)))
          comment-end)
      (when comment-start
        (setf comment-end (point-at-eoc comment-start limit))
        (if (and obvious-headers
                 (or (comment-at-file-header-p comment-start)
                     (comment-header-p comment-start)))
            (progn
              ;; A header comment: skip past it and return non-nil.
              (goto-char (point-at-boc comment-end limit))
              t)
          ;; Not showing headers, or a non-header comment: set match
          ;; data, move past it, and return non-nil.
          (set-match-data (list comment-start comment-end))
          (goto-char (if obvious-preserve-blank-lines
                         (1+ comment-end)
                       comment-end))
          t)))))

;;;; Footer

(provide 'obvious)

;;; obvious.el ends here
