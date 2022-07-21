;;; obvious.el --- Who needs comments when the code is so obvious  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
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

;; Inspired by Anders Lindgren's `nocomments-mode':
;; <https://github.com/Lindydancer/nocomments-mode>.

;;; Code:

(defvar-local obvious-overlays nil
  "Overlays used to hide comments.")

(defvar font-lock-end)

(defgroup obvious nil
  "Who needs comments when the code is so obvious."
  ;; FIXME: Other properties.
  )

(defcustom obvious-headers t
  "Keep file headers visible.
That is, don't hide comments at the beginning of the file, before
any code."
  ;; FIXME: Make this work.
  :type 'boolean)

(defun obvious-matcher (limit)
  (cl-labels ((in-comment-p
               () (pcase-let ((`( _depth _list-start _lcst-start _stringp
                                  ,in-comment-p _after-quote _mpd _comment-style
                                  ,comment-start _open-paren-poss _2cc-pos . _rest)
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
               (save-excursion
                 (goto-char (point-at-boc pos))
                 (forward-comment most-positive-fixnum)
                 ;; We intentionally ignore the limit.  It seems to be necessary.
                 (point))))
    (message "IN MATCHER AT:%S" (point))
    (let ((comment-start (point-at-boc (point)))
          comment-end)
      (when comment-start
        (setf comment-end (point-at-eoc comment-start limit))
        (message "COMMENT-START:%S  COMMENT-END:%S" comment-start comment-end)
        (set-match-data (list comment-start comment-end))
        (goto-char comment-end)
        t))))

(defvar obvious-font-lock-keywords
  '((;; MATCHER
     obvious-matcher
     ;; HIGHLIGHTERS
     (0 ;; SUBEXP
      ;; FACESPEC
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
        (message "ARGH: %S  %S" (match-beginning 0) (match-end 0))
        (overlay-put overlay :obvious t)
        (overlay-put overlay 'invisible t)
        (push overlay obvious-overlays)
        ;; Return nil.
        nil)))))

(define-minor-mode obvious-mode
  "Hide comments, because what this code does is obvious."
  :lighter " Obvious"
  (if obvious-mode
      (progn
        (font-lock-add-keywords nil obvious-font-lock-keywords 'append)
        (font-lock-flush))
    ;; Disable mode.
    (font-lock-remove-keywords nil obvious-font-lock-keywords)
    (dolist (overlay obvious-overlays)
      (delete-overlay overlay))
    (setf obvious-overlays nil)
    (font-lock-flush)))

(provide 'obvious)

;;; obvious.el ends here
