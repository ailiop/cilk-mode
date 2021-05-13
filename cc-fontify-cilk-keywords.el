;;; cc-fontify-cilk-keywords.el --- Cilk keyword fontification -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Supertech group, CSAIL, MIT
;;
;; Author: Alexandros-Stavros Iliopoulos <https://github.com/ailiop>
;; Maintainer: Alexandros-Stavros Iliopoulos <1577182+ailiop@users.noreply.github.com>
;; Created: January 26, 2021
;; Modified: March 31, 2021
;; Version: 0.1.0
;; Keywords: Cilk, fontification, cc-mode
;; Homepage: https://github.com/ailiop/cilk-goodies
;; Package-Requires: ((emacs "24.3") (cc-mode "5.33"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;;  MIT License
;;
;;  Permission is hereby granted, free of charge, to any person obtaining a copy
;;  of this software and associated documentation files (the "Software"), to
;;  deal in the Software without restriction, including without limitation the
;;  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;;  sell copies of the Software, and to permit persons to whom the Software is
;;  furnished to do so, subject to the following conditions:
;;
;;  The above copyright notice and this permission notice shall be included in
;;  all copies or substantial portions of the Software.  THE SOFTWARE IS
;;  PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;
;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;  IN THE SOFTWARE.
;;
;;; Commentary:
;;
;;  Custom fontification of Cilk keywords (`cilk_spawn', `cilk_sync',
;;  `cilk_for') in `c-mode', `c++-mode', and derived modes.
;;
;;  Cilk keywords are fontified with the `font-lock-cilk-keyword-face'.  By
;;  default, this is the same as the standard face except that its foreground
;;  color is mustard (color hex: "#FDA900").
;;
;;  To customize the Cilk keyword face, use the `set-face-attribute' elisp
;;  function.  For more information on face attributes, see:
;;  <https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html>.
;;
;;  Requires `cc-mode' package.
;;
;;; Using with vanilla Emacs:
;;
;;  ;; ===== .emacs =====
;;  ; (assume that `cc-mode.el' can be found via the `load-path')
;;  (add-to-list 'load-path "/path/to/parent/of/cc-fontify-cilk-keywords.el")
;;  (require 'cc-fontify-cilk-keywords)
;;  ; optional customization:
;;  (set-face-attribute 'font-lock-cilk-keyword-face nil
;;                      :weight     'bold
;;                      :foreground "#582A72")
;;
;;; Installing and using with Doom Emacs:
;;
;;  ;; ===== .doom.d/packages.el =====
;;  (package! cc-fontify-cilk-keywords
;;    :recipe (:host github :repo "ailiop/cilk-goodies"
;;             :files ("cc-fontify-cilk-keywords.el")))
;;
;;  ;; ===== .doom.d/config.el =====
;;  (use-package! cc-fontify-cilk-keywords
;;    :after cc-mode
;;    ;; optional customization
;;    :config
;;    (set-face-attribute 'font-lock-cilk-keyword-face nil
;;                        :weight     'bold
;;                        :foreground "#582A72"  ; purple
;;                        ))
;;
;;; Code:

(require 'cc-mode)

(make-face 'font-lock-cilk-keyword-face)
(set-face-attribute 'font-lock-cilk-keyword-face nil
                    :foreground "#FDA900" ; mustard color
                    )

;; To get the optimized regexp that matches Cilk keywords, evaluate:
;; (regexp-opt '("cilk_for" "cilk_spawn" "cilk_sync") 'symbols)

(defun cilk-keyword-fontification-hook ()
  "Apply custom Cilk font face to Cilk keywords."
  (font-lock-add-keywords
   nil                                  ; only apply to current buffer
   '(("\\_<\\(cilk_\\(?:for\\|s\\(?:pawn\\|ync\\)\\)\\)\\_>"
      . 'font-lock-cilk-keyword-face))))

(add-hook 'c-mode-hook   #'cilk-keyword-fontification-hook)
(add-hook 'c++-mode-hook #'cilk-keyword-fontification-hook)

(provide 'cc-fontify-cilk-keywords)

;;; cc-fontify-cilk-keywords.el ends here
