;;; prism.el --- Disperse lisp forms into a spectrum of color by depth  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: lisp

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

;; Disperse lisp forms into a spectrum of color by depth.  Like
;; `rainbow-blocks', but respects existing non-color face properties,
;; and allows flexible configuration of faces and colors.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'color)
(require 'thingatpt)

(require 'anaphora)
(require 'dash)

;;;; Variables

(defvar prism-num-faces nil
  "Number of `prism' faces.  Set automatically by `prism-set-faces'.")

(defvar prism-faces nil
  "Alist mapping depth levels to faces.")

(defvar prism-faces-comments nil
  "Alist mapping depth levels to string faces.")

(defvar prism-faces-strings nil
  "Alist mapping depth levels to string faces.")

(defvar prism-face nil
  "Set by `prism-match' during fontification.")

(defvar prism-debug nil
  "Enables `prism' debug output.
Only takes effect by recompiling the `prism' package with setting
non-nil.")

(defvar-local prism-syntax-table nil
  "Syntax table used by `prism-mode'.
Set automatically.")

;;;; Customization

(defgroup prism nil
  "Disperse lisp forms into a spectrum of colors according to depth."
  :group 'font-lock)

(defcustom prism-color-attribute :foreground
  "Face attribute set in `prism' faces."
  :type '(choice (const :tag "Foreground" :foreground)
                 (const :tag "Background" :background)))

(defcustom prism-desaturations '(40 50 60)
  "Default desaturation values applied to faces at successively deeper depths.
Extrapolated to the length of `prism-faces'."
  :type '(repeat number))

(defcustom prism-lightens '(0 5 10)
  "Default lightening values applied to faces at successively deeper depths.
Extrapolated to the length of `prism-faces'."
  :type '(repeat number))

(defcustom prism-comments nil
  "Whether to colorize comments.
Note that comments at depth 0 are not colorized, which preserves
e.g. commented Lisp headings."
  :type 'boolean)

(defcustom prism-strings nil
  "Whether to fontify strings."
  :type 'boolean)

;;;; Minor mode

(define-minor-mode prism-mode
  "Disperse lisp forms into a spectrum of colors according to depth."
  :global nil
  (let ((keywords '((prism-match 0 prism-face prepend))))
    (if prism-mode
        (progn
          (unless prism-faces
            (setq prism-mode nil)
            (user-error "Please set `prism' colors with `prism-set-faces'"))
          (setq prism-syntax-table (prism-syntax-table (syntax-table)))
          (font-lock-add-keywords nil keywords 'append)
          (add-hook 'font-lock-extend-region-functions #'prism-extend-region nil 'local)
          (font-lock-flush))
      (font-lock-remove-keywords nil keywords)
      (remove-hook 'font-lock-extend-region-functions #'prism-extend-region 'local)
      (prism-remove-faces))))

;;;; Functions

(defmacro prism-debug (obj)
  (when prism-debug
    `(with-current-buffer (or (get-buffer "*prism-debug*")
                              (with-current-buffer (get-buffer-create "*prism-debug*")
                                (buffer-disable-undo)
                                (current-buffer)))
       (save-excursion
         (goto-char (point-max))
         (print ,obj (current-buffer))))))

;; Silence byte-compiler for these special variables that are bound
;; around `font-lock-extend-region-functions'.
(defvar font-lock-beg)
(defvar font-lock-end)

(defun prism-extend-region ()
  "Extend region to the current sexp.
For `font-lock-extend-region-functions'."
  (prism-debug (list (cons 'extend-region 1)
                     (cons 'point (point))))
  (let (changed-p)
    (unless (= 0 (nth 0 (syntax-ppss)))
      ;; Not at top level: extend region backward/up.
      (let ((orig-pos (point)))
        (save-excursion
          (when (ignore-errors
                  (backward-up-list 1 t t))
            (setf font-lock-beg (point))
            (unless (= font-lock-beg orig-pos)
              (setf changed-p t))))))
    (save-excursion
      (goto-char font-lock-end)
      (unless (= 0 (nth 0 (syntax-ppss)))
        ;; Not at top level: extend region forward.
        (let ((end (save-excursion
                     (ignore-errors
                       ;; This function signals an error, (scan-error "Containing
                       ;; expression ends prematurely"), when called with point
                       ;; immediately before the closing paren of an sexp.  In that
                       ;; case, we're already at the end, so ignore the error.
                       ;; FIXME: Maybe use something other than `thing-at-point--end-of-sexp',
                       ;; although its implementation looks very simple.
                       (thing-at-point--end-of-sexp))
                     (point))))
          (when (> end font-lock-end)
            (setf font-lock-end end
                  changed-p t)))))
    (prism-debug (list (cons 'extend-region 2)
                       (cons 'point (point))
                       (cons 'font-lock-beg font-lock-beg)
                       (cons 'font-lock-end font-lock-end)))
    changed-p))

(defun prism-syntax-table (syntax-table)
  "Return SYNTAX-TABLE modified for `prism-mode'."
  ;; Copied from `rainbow-blocks-make-syntax-table'.
  (let ((table (copy-syntax-table syntax-table)))
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table))

(defun prism-match (_limit)
  "Matcher function for `font-lock-keywords'."
  ;; Trying to rewrite this function.
  ;; NOTE: Be sure to return non-nil when a match is found.
  ;; NOTE: It feels wrong, but since we're not using `re-search-forward' until
  ;; after we've found the end by other means, we don't use the limit argument
  ;; provided by `font-lock'.  Seems to work okay...
  (cl-macrolet ((parse-syntax ()
                              `(-setq (depth _ _ in-string-p comment-level-p)
                                 (syntax-ppss)))
                (comment-p ()
                           `(or comment-level-p (looking-at-p (rx (syntax comment-start)))))
                (face-at ()
                         ;; Return face to apply.  Should be called with point at `start'.
                         `(cond ((comment-p)
                                 (pcase depth
                                   (0 'font-lock-comment-face)
                                   (_ (if prism-faces-comments
                                          (alist-get depth prism-faces-comments)
                                        (alist-get depth prism-faces)))))
                                ((or in-string-p (looking-at-p (rx (syntax string-quote))))
                                 (pcase depth
                                   (0 'font-lock-string-face)
                                   (_ (if prism-faces-strings
                                          (alist-get depth prism-faces-strings)
                                        (alist-get depth prism-faces)))))
                                (t (alist-get depth prism-faces)))))
    (with-syntax-table prism-syntax-table
      (unless (eobp)
        ;; Not at end-of-buffer: start matching.
        (let ((parse-sexp-ignore-comments t)
              depth in-string-p comment-level-p comment-or-string-start start end
              found-comment-p found-string-p)
          (while
              ;; Skip to start of where we should match.
              (cond ((eolp)
                     (forward-line 1))
                    ((looking-at-p (rx blank))
                     (forward-whitespace 1))
                    ((unless prism-strings
                       (when (looking-at-p (rx (syntax string-quote)))
                         ;; At a string: skip it.
                         (forward-sexp))))
                    ((unless prism-comments
                       (forward-comment (buffer-size))))))
          (parse-syntax)
          (when in-string-p
            ;; In a string: go back to its beginning (before its delimiter).  It would
            ;; be nice to leave this out and rely on the check in the `while' above, but
            ;; if partial fontification starts inside a string, we have to handle that.
            (goto-char comment-or-string-start)
            (unless prism-strings
              (forward-sexp))
            (parse-syntax))
          ;; Set start and end positions.
          (setf start (point)
                ;; I don't know if `ignore-errors' is going to be slow, but since
                ;; `scan-lists' and `scan-sexps' signal errors, it seems necessary if we want
                ;; to use them (and they seem to be cleaner to use than regexp searches).
                end (save-excursion
                      (or (when (looking-at-p (rx (syntax close-parenthesis)))
                            ;; I'd like to just use `scan-lists', but I can't find a way around this initial check.
                            ;; The code (scan-lists start 1 1), when called just inside a list, scans past the end
                            ;; of it, to just outside it, which is not what we want, because we want to highlight
                            ;; the closing paren with the shallower depth.  But if we just back up one character,
                            ;; we never exit the list.  So we have to check whether we're looking at the close of a
                            ;; list, and if so, move just past it.
                            (cl-decf depth)
                            (1+ start))
                          (when (and prism-comments
                                     (or comment-level-p
                                         (looking-at-p (rx (syntax comment-start)))))
                            (forward-comment (buffer-size))
                            (setf found-comment-p t)
                            (point))
                          (when (looking-at-p (rx (syntax string-quote)))
                            (forward-sexp 1)
                            (setf found-string-p t)
                            (point))
                          (ignore-errors
                            ;; Scan to the past the delimiter of the next deeper list.
                            (scan-lists start 1 -1))
                          (ignore-errors
                            ;; Scan to the end of the current list delimiter.
                            (1- (scan-lists start 1 1)))
                          ;; NOTE: Leaving out error for now.  Will try just returning nil to avoid hanging Emacs.
                          ;; (error "Unable to find end")
                          )))
          (when end
            ;; End found: Try to fontify.
            (save-excursion
              (or (unless (or found-string-p found-comment-p)
                    ;; Neither in a string nor looking at nor in a comment: set `end' to any comment found before it.
                    (when (re-search-forward (rx (syntax comment-start)) end t)
                      (setf end (match-beginning 0))))
                  (unless (or found-comment-p found-string-p)
                    ;; Neither in nor looking at a comment: set `end' to any string or comment found before it.
                    (when (re-search-forward (rx (syntax string-quote)) end t)
                      (setf end (match-beginning 0))))))
            (if (and (comment-p) (= 0 depth))
                (setf prism-face nil)
              (setf prism-face (face-at)))
            (goto-char end)
            (set-match-data (list start end (current-buffer)))
            ;; Be sure to return non-nil!
            t))))))

(cl-defun prism-remove-faces (&optional (beg (point-min)))
  "Remove `prism' faces from buffer.
Note a minor bug at the moment: anonymous faces are also
removed."
  (cl-macrolet ((without-prism-faces (faces)
                                     `(cl-loop for face in ,faces
                                               ;; FIXME: This removes anonymous faces.
                                               unless (or (not (facep face))
                                                          (string-prefix-p "prism-level-" (symbol-name face)))
                                               collect face)))
    (with-silent-modifications
      (save-excursion
        (goto-char beg)
        (cl-loop for end = (or (next-single-property-change (point) 'face) (point-max))
                 for faces = (get-text-property (point) 'face)
                 when faces
                 do (put-text-property (point) end 'face (without-prism-faces faces))
                 for next-change = (next-single-property-change (point) 'face)
                 while (and next-change
                            (/= next-change (point-max)))
                 do (goto-char next-change))))))

;;;;; Colors

(cl-defun prism-set-faces (&key colors shuffle
                                (attribute prism-color-attribute) (num 16)
                                (desaturations prism-desaturations) (lightens prism-lightens)
                                (comments-fn (lambda (color)
                                               (--> color
                                                    (color-desaturate-name it 30)
                                                    (color-lighten-name it -10))))
                                (strings-fn (lambda (color)
                                              (--> color
                                                   (color-desaturate-name it 20)
                                                   (color-lighten-name it 10)))))
  ;; FIXME: Docstring.
  "Set NUM `prism' faces according to COLORS.
COLORS is a list of one or more color name strings (like
\"green\" or \"#ff0000\") or face symbols (of which the
foreground color is used)."
  (declare (indent defun))
  (when shuffle
    (setf colors (prism-shuffle colors)))
  (cl-flet ((faces (colors &optional suffix (fn #'identity))
                   (setf suffix (if suffix
                                    (concat "-" suffix)
                                  ""))
                   (cl-loop for i from 0 below num
                            for face = (intern (format "prism-level-%d%s" i suffix))
                            for color = (funcall fn (nth i colors))
                            ;; Delete existing face, important if e.g. changing :foreground to :background.
                            when (internal-lisp-face-p face)
                            do (face-spec-set face nil 'customized-face)
                            do (custom-declare-face face '((t)) (format "`prism' face%s #%d" suffix i))
                            do (set-face-attribute face nil attribute color)
                            collect (cons i face))))
    (let* ((colors (->> colors
                        (--map (cl-etypecase it
                                 (face (face-attribute it :foreground nil 'inherit))
                                 (string it)))
                        -cycle
                        (prism-modify-colors :num num :desaturations desaturations :lightens lightens
                                             :colors))))
      (setf prism-num-faces num
            prism-faces (faces colors)
            prism-faces-strings (faces colors "strings" strings-fn)
            prism-faces-comments (faces colors "comments" comments-fn)))))

(cl-defun prism-modify-colors (&key num colors desaturations lightens &allow-other-keys)
  ;; FIXME: Docstring.
  "Return list of NUM colors for use in `rainbow-identifiers', `rainbow-blocks', etc.
Modifies COLORS according to DESATURATIONS and LIGHTENS."
  (cl-flet ((modify-color (color desaturate lighten)
                          (--> color
                               (color-desaturate-name it desaturate)
                               (color-lighten-name it lighten))))
    (when (< (length desaturations) num)
      (setf desaturations (prism-expand-list num desaturations)))
    (when (< (length lightens) num)
      (setf lightens (prism-expand-list num lightens)))
    (cl-loop for i from 0 below num
             for desaturate = (nth i desaturations)
             for lighten = (nth i lightens)
             collect (modify-color (nth i colors) desaturate lighten))))

(defun prism-shuffle (seq)
  "Destructively shuffle SEQ.
Copied from `elfeed-shuffle'."
  (let ((n (length seq)))
    (prog1 seq                  ; don't use dotimes result (bug#16206)
      (dotimes (i n)
        (cl-rotatef (elt seq i) (elt seq (+ i (cl-random (- n i)))))))))

(defun prism-expand-list (new-length list)
  "Return LIST expanded to NEW-LENGTH.
Each element of LIST is repeated an equal number of times, except
that the last element may be repeated an extra time when
necessary."
  (let* ((length (length list))
         (_longer-p (or (> new-length length)
                        (user-error "NEW-LENGTH must be longer than LIST")))
         (repeat-n (/ new-length (if (zerop (mod new-length length))
                                     length
                                   (1- length))))
         (final-element-p (not (zerop (mod new-length length))))
         (new-list (->> list
                        (--map (-repeat repeat-n it))
                        (-flatten))))
    (if final-element-p
        (-snoc new-list (-last-item list))
      new-list)))

;;;; Footer

(provide 'prism)

;;; prism.el ends here
