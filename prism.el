;;; prism.el --- Disperse lisp forms into a spectrum of color by depth  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/prism.el
;; Package-Requires: ((Emacs "26.1") (dash "2.14.1"))
;; Keywords: faces lisp

;;; License:

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
;; and allows flexible configuration of faces and colors.  Also
;; optionally colorizes strings and/or comments by code depth in a
;; similar, customizable way.

;; Usage:

;; 1.  Activate `prism-mode' in a buffer.
;; 2.  Enjoy.

;; To customize, see the `prism' customization group, e.g. by using
;; "M-x customize-group RET prism RET".  For example, by default,
;; comments and strings are colorized according to depth, similarly to
;; code, but this can be disabled.

;; Advanced:

;; More advanced customization of faces is done by calling
;; `prism-set-colors', which can override the default settings and
;; perform additional color manipulations.  The primary argument is
;; COLORS, which should be a list of colors, each of which may be a
;; name, a hex RGB string, or a face name (of which the foreground
;; color is used).  Note that the list of colors need not be as long
;; as the number of faces that's actually set (e.g. the default is 16
;; faces), because the colors are automatically repeated and adjusted
;; as necessary.

;; Here's an example that the author finds pleasant:

;;   (prism-set-colors :num 16
;;     :desaturations (cl-loop for i from 0 below 16
;;                             collect (* i 2.5))
;;     :lightens (cl-loop for i from 0 below 16
;;                        collect (* i 2.5))
;;     :colors (list "sandy brown" "dodgerblue" "medium sea green")
;;
;;     :comments-fn
;;     (lambda (color)
;;       (prism-blend color (face-attribute 'font-lock-comment-face :foreground) 0.25))
;;
;;     :strings-fn
;;     (lambda (color)
;;       (prism-blend color "white" 0.5)))

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'color)
(require 'thingatpt)

(require 'dash)

;;;; Variables

(defvar prism-num-faces nil
  "Number of `prism' faces.  Set automatically by `prism-set-colors'.")

(defvar prism-faces nil
  "Alist mapping depth levels to faces.")

(defvar prism-faces-comments nil
  "Alist mapping depth levels to string faces.")

(defvar prism-faces-strings nil
  "Alist mapping depth levels to string faces.")

(defvar prism-face nil
  "Set by `prism-match' during fontification.")

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

(defcustom prism-comments t
  "Whether to colorize comments.
Note that comments at depth 0 are not colorized, which preserves
e.g. commented Lisp headings."
  :type 'boolean)

(defcustom prism-comments-fn
  (lambda (color)
    (prism-blend color (face-attribute 'font-lock-comment-face :foreground) 0.25))
  "Function which adjusts colors for comments."
  :type 'function)

(defcustom prism-strings t
  "Whether to fontify strings."
  :type 'boolean)

(defcustom prism-strings-fn
  (lambda (color)
    (prism-blend color "white" 0.5))
  "Function which adjusts colors for strings."
  :type 'function)

;;;; Minor mode

(define-minor-mode prism-mode
  "Disperse lisp forms into a spectrum of colors according to depth."
  :global nil
  (let ((keywords '((prism-match 0 prism-face prepend))))
    (if prism-mode
        (progn
          (unless prism-faces
            (prism-set-colors))
          (setq prism-syntax-table (prism-syntax-table (syntax-table)))
          (font-lock-add-keywords nil keywords 'append)
          (add-hook 'font-lock-extend-region-functions #'prism-extend-region nil 'local)
          (font-lock-flush))
      (font-lock-remove-keywords nil keywords)
      (remove-hook 'font-lock-extend-region-functions #'prism-extend-region 'local)
      (prism-remove-faces))))

;;;; Commands

(defun prism-save-colors ()
  "Save current `prism' colors.
Function `prism-set-colors' does not save its argument values
permanently.  This command saves them using the customization
system so that `prism-set-colors' can then be called without
arguments to set the same faces."
  (interactive)
  (cl-letf (((symbol-function 'custom-save-all)
             (symbol-function 'ignore)))
    ;; Avoid saving the file for each variable, which is very slow.
    ;; Save it once at the end.
    (dolist (var (list 'prism-desaturations 'prism-lightens
                       'prism-comments-fn 'prism-strings-fn))
      (customize-save-variable var (symbol-value var))))
  (customize-save-variable 'prism-colors prism-colors))

;;;; Functions

;; Silence byte-compiler for these special variables that are bound
;; around `font-lock-extend-region-functions'.
(defvar font-lock-beg)
(defvar font-lock-end)

(defun prism-extend-region ()
  "Extend region to the current sexp.
For `font-lock-extend-region-functions'."
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

(defun prism-match (limit)
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
            ;; In a string: go back to its beginning (before its delimiter).
            ;; It would be nice to leave this out and rely on the check in
            ;; the `while' above, but if partial fontification starts inside
            ;; a string, we have to handle that.
            ;; NOTE: If a string contains a Lisp comment (e.g. in
            ;; `custom-save-variables'), `in-string-p' will be non-nil, but
            ;; `comment-or-string-start' will be nil.  I don't know if this
            ;; is a bug in `parse-partial-sexp', but we have to handle it.
            (when comment-or-string-start
              (goto-char comment-or-string-start)
              (unless prism-strings
                (forward-sexp))
              (parse-syntax)))
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
                          limit
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

  ;; Silence byte-compiler since this is used in the defun below.
(defvar prism-colors)

(cl-defun prism-set-colors (&key shuffle (colors prism-colors)
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
  ;; Save arguments for later saving as customized variables,
  ;; including the unmodified (but shuffled) colors.
  (setf prism-colors colors
        prism-desaturations desaturations
        prism-lightens lightens
        prism-comments-fn comments-fn
        prism-strings-fn strings-fn)
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
                        (prism-modify-colors :num num
                                             :desaturations desaturations
                                             :lightens lightens
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

(defun prism-blend (a b alpha)
  "Return color A blended with color B by amount ALPHA."
  (cl-flet ((blend (a b alpha)
                   (+ (* alpha a) (* b (- 1 alpha)))))
    (-let* (((ar ag ab) (color-name-to-rgb a))
            ((br bg bb) (color-name-to-rgb b)))
      (color-rgb-to-hex (blend ar br alpha)
                        (blend ag bg alpha)
                        (blend ab bb alpha)))))

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

;;;; Further customization

;; These are at the bottom because one of the setters calls one of the
;; functions above.

(defcustom prism-colors
  (list 'font-lock-comment-face 'font-lock-function-name-face
        'font-lock-keyword-face 'font-lock-constant-face 'font-lock-type-face)
  "List of colors used by default."
  :type '(repeat (choice (face :tag "Face (using its foreground color)")
                         color))
  :set (lambda (option value)
         (set-default option value)
         (prism-set-colors)))

;;;; Footer

(provide 'prism)

;;; prism.el ends here
