;;; cilk-keywords-hl.el --- Cilk keyword highlighting -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Supertech group, CSAIL, MIT
;;
;; Author: Alexandros-Stavros Iliopoulos <https://github.com/ailiop>
;; Maintainer: Alexandros-Stavros Iliopoulos <1577182+ailiop@users.noreply.github.com>
;; Created: January 26, 2021
;; Modified: November 12, 2021
;; Version: 0.2.0
;; Keywords: c, cilk, faces, languages
;; Homepage: https://github.com/ailiop/cilk-goodies
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;;  MIT License
;;
;;  Permission is hereby granted, free of charge, to any person obtaining a
;;  copy of this software and associated documentation files (the "Software"),
;;  to deal in the Software without restriction, including without limitation
;;  the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;  and/or sell copies of the Software, and to permit persons to whom the
;;  Software is furnished to do so, subject to the following conditions:
;;
;;  The above copyright notice and this permission notice shall be included in
;;  all copies or substantial portions of the Software.  THE SOFTWARE IS
;;  PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;
;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;  DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;;
;;  Provides a minor mode, `cilk-keywords-hl-mode' for highlighting Cilk
;;  keywords.
;;
;;  Highlighting is done by applying the `cilk-keyword' face to all symbols in
;;  the list `cilk-keywords-hl-list'.
;;
;;  To enable Cilk keyword highlighting in `c-mode', `c++-mode', and derived
;;  modes (assuming you are using the `cc-mode' package), add
;;  `cilk-keywords-hl-mode' to the revelant mode hooks:
;;
;;      (dolist (hook '(c-mode-hook c++-mode-hook))
;;        (add-hook hook #'cilk-keywords-hl-mode))
;;
;;  By default, the `cilk-keyword' face is the same as the standard face
;;  except that its foreground color is mustard (color hex: "#FDA900").
;;
;;  To customize the Cilk keyword face, use the `set-face-attribute' elisp
;;  function.  For more information on face attributes, see:
;;  <https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html>.
;;
;;  It is possible to change which symbols are highlighted as Cilk keywords by
;;  altering `cilk-keywords-hl-list'.  To apply such changes to a buffer where
;;  `cilk-keywords-hl-mode' is already enabled, call
;;  `cilk-keywords-hl-refresh' in said buffer.
;;
;;; Using with vanilla Emacs:
;;
;;  ;; ===== .emacs =====
;;  (add-to-list 'load-path "/path/to/parent/of/cilk-keywords-hl.el")
;;  ;; enable Cilk keyword highlighting in C/C++ and derived modes
;;  ;; (assuming cc-mode is already enabled)
;;  (dolist (hook '(c-mode-hook c++-mode-hook))
;;    (add-hook hook #'cilk-keywords-hl-mode))
;;  ;; optional customization:
;;  (require 'cilk-keywords-hl)
;;  (set-face-attribute 'cilk-keyword nil
;;                      :weight     'bold
;;                      :foreground "#582A72")  ; purple
;;
;;; Installing and using with Doom Emacs:
;;
;;  ;; ===== .doom.d/packages.el =====
;;  (package! cilk-keywords-hl
;;    :recipe (:host github :repo "ailiop/cilk-goodies"
;;             :files ("cilk-keywords-hl.el")))
;;
;;  ;; ===== .doom.d/config.el =====
;;  (add-hook! '(c-mode-hook c++-mode-hook) #'cilk-keywords-hl-mode)
;;
;;  ;; optional customization
;;  (use-package! cilk-keywords-hl
;;    :after cc-mode
;;    :config
;;    (set-face-attribute 'cilk-keyword nil
;;                        :weight     'bold
;;                        :foreground "#582A72"))  ; purple
;;
;;; Code:

(require 'cc-mode)


(defvar cilk-keywords-hl-list '("cilk_for"
                                "cilk_scope"
                                "cilk_spawn"
                                "cilk_sync")
  "List of Cilk keywords for highlighting.

Highlighting is done by applying the `cilk-keyword' face to each
keyword instance in buffers where `cilk-keywords-hl-mode' is
enabled.")


(make-face 'cilk-keyword)
(set-face-attribute 'cilk-keyword nil
                    :foreground "#FDA900") ; mustard color


(make-variable-buffer-local
 (defvar cilk-keywords-hl-list--enabled cilk-keywords-hl-list
   "Record of Cilk keywords list at the time highlighting was last enabled.

This is useful for making sure highlighted keywords are
un-highlighted when `cilk-keywords-hl-mode' is disabled."))


(defun cilk-keywords-hl--enabled-regexp ()
  "Return symbol-matching regexp for buffer-enabled Cilk keywords.

This function is called internally by `cilk-keywords-hl--set'."
  (regexp-opt cilk-keywords-hl-list--enabled 'symbols))


(defun cilk-keywords-hl--set (on-flag)
  "Toggle Cilk keyword fontification in current buffer based on ON-FLAG.

If ON-FLAG is non-nil, apply the `cilk-keyword' face to symbols
in `cilk-keywords-hl-list'.  If ON-FLAG is nil, remove the face."
  (let ((font-lock-kw-func (if on-flag
                               'font-lock-add-keywords
                             'font-lock-remove-keywords)))
    (when on-flag
      (setq cilk-keywords-hl-list--enabled cilk-keywords-hl-list))
    (funcall font-lock-kw-func
             nil  ; only apply to current buffer
             (list (list (cilk-keywords-hl--enabled-regexp)
                         'quote 'cilk-keyword)))))


(defun cilk-keywords-hl--refontify ()
  "Force re-fontification in current buffer.

See also: https://emacs.stackexchange.com/a/10169"
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))


;;;###autoload
(define-minor-mode cilk-keywords-hl-mode
  "Minor mode for highlighting Cilk keywords.

Highlights symbols in `cilk-keywords-hl-list' by applying the
`cilk-keyword' face to them."
  :lighter " Cilk"

  (cilk-keywords-hl--set cilk-keywords-hl-mode)
  (cilk-keywords-hl--refontify))


(defun cilk-keywords-hl-refresh ()
  "Refresh fontification of Cilk keywords in current buffer.

If `cilk-keywords-hl-mode' is enabled, this ensures that the
`cilk-keyword' face is applied to keywords currently in
`cilk-keywords-hl-list'.

If `cilk-keywords-hl-mode' is disabled, this function does
nothing."
  (interactive)
  (when cilk-keywords-hl-mode
    (cilk-keywords-hl--set nil)
    (cilk-keywords-hl--set t)
    (cilk-keywords-hl--refontify)))


(provide 'cilk-keywords-hl)

;;; cilk-keywords-hl.el ends here
