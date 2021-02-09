;;; cc-fontify-cilk-keywords.el --- Cilk keyword fontification -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Supertech group, CSAIL, MIT
;;
;; Author: Alexandros-Stavros Iliopoulos <https://github.com/ailiop>
;; Maintainer: Alexandros-Stavros Iliopoulos <1577182+ailiop@users.noreply.github.com>
;; Created: January 26, 2021
;; Modified: January 26, 2021
;; Version: 0.0.1
;; Keywords: Cilk, fontification, cc-mode
;; Homepage: https://github.com/ailiop/cilk-goodies
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
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
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html.
;;
;;  Requires `cc-mode' package.
;;
;;; For Doom Emacs users:
;;
;;  ;; in ~/.doom.d/packages.el
;;  (package! cc-fontify-cilk-keywords
;;    :recipe (:host github :repo "ailiop/cilk-goodies"
;;             :files ("cc-fontify-cilk-keywords.el")))
;;
;;  ;; in ~/.doom.d/config.el
;;  (use-package! cc-fontify-cilk-keywords
;;    :after cc-mode
;;    ;; optional customizations
;;    :config (set-face-attribute 'font-lock-cilk-keyword-face nil
;;                                :weight     'bold
;;                                :foreground "#582A72"  ; purple
;;                                ))
;;
;;; Code:

(require 'cc-mode)

(make-face 'font-lock-cilk-keyword-face)
(set-face-attribute 'font-lock-cilk-keyword-face nil
                    :foreground "#FDA900" ; mustard color
                    )

(defun cilk-keyword-fontification-hook ()
  "Apply custom font face to Cilk keywords."
  (font-lock-add-keywords nil
                          '(("\\_<\\(cilk_\\(?:for\\|s\\(?:pawn\\|ync\\)\\)\\)\\_>"
                             . 'font-lock-cilk-keyword-face))))

(add-hook 'c-mode-hook #'cilk-keyword-fontification-hook)
(add-hook 'c++-mode-hook #'cilk-keyword-fontification-hook)

(provide 'cc-fontify-cilk-keywords)

;;; cc-fontify-cilk-keywords.el ends here
