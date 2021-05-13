;;; flycheck-use-opencilk-clang.el --- OpenCilk for flycheck linting -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Supertech group, CSAIL, MIT
;;
;; Author: Alexandros-Stavros Iliopoulos <https://github.com/ailiop>
;; Maintainer: Alexandros-Stavros Iliopoulos <1577182+ailiop@users.noreply.github.com>
;; Created: April 07, 2021
;; Modified: April 07, 2021
;; Version: 0.0.1
;; Keywords: Cilk, OpenCilk, clang, flycheck, lint, cc-mode
;; Homepage: https://github.com/ailiop/cilk-goodies
;; Package-Requires: ((emacs "24.3"))
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
;;  Enable Cilk-aware flycheck linting with the OpenCilk clang compiler.
;;
;;  This package simply does the following:
;;  - set the flycheck C/C++ clang executable to the OpenCilk clang compiler;
;;  - set the `-fopencilk' flag; and
;;  - add C/C++ mode hooks to instruct flycheck to use clang.
;;
;;  By default, the OpenCilk clang compiler executable is assumed to be
;;  `/usr/local/opencilk/bin/clang'.  To change this, set the variable
;;  `flycheck-c/c++-clang-executable' to point to the OpenCilk clang executable.
;;
;;  (In the future, a custom `c/c++-opencilk' flycheck checker may be used,
;;  instead of the already-existing `c/c++-clang' checker.)
;;
;;  Requires packages: `flycheck', `cc-mode'.
;;
;;; Using with vanilla Emacs:
;;
;;  ;; ===== .emacs =====
;;  ;; (assume that `flycheck.el' and `cc-mode.el' can be found via the `load-path')
;;  (add-to-list 'load-path "/path/to/parent/of/flycheck-use-opencilk-clang.el")
;;  (require 'flycheck-use-opencilk-clang)
;;  ;; [optional] OpenCilk compiler (default: /usr/local/opencilk/bin/clang)
;;  (setq flycheck-c/c++-clang-executable "/my/path/to/opencilk/clang")
;;  ;; other, optional customizations to `flycheck-clang-xxxxxxxx' variables...
;;
;;; Installing and using with Doom Emacs:
;;
;;  ;; ===== .doom.d/packages.el =====
;;  (package! flycheck-use-opencilk-clang
;;    :recipe (:host github :repo "ailiop/cilk-goodies"
;;             :files ("flycheck-use-opencilk-clang.el")))
;;
;;  ;; ===== .doom.d/config.el =====
;;  (use-package! flycheck-use-opencilk-clang
;;    :after (flycheck cc-mode)
;;    ;; optional customization
;;    :config
;;    (setq flycheck-c/c++-clang-executable "/my/path/to/opencilk/clang")
;;    )
;;
;;; Code:

(require 'flycheck)
(require 'cc-mode)

(setq flycheck-c/c++-clang-executable "/usr/local/opencilk/bin/clang")
(add-to-list 'flycheck-clang-args "-fopencilk")

(defun flycheck-set-checker-to-c/c++-clang ()
  "Set the `flycheck-checker' variable to `c/c++-clang'."
  (setq flycheck-checker 'c/c++-clang))

(add-hook 'c-mode-hook   #'flycheck-set-checker-to-c/c++-clang)
(add-hook 'c++-mode-hook #'flycheck-set-checker-to-c/c++-clang)

(provide 'flycheck-use-opencilk-clang)

;;; flycheck-use-opencilk-clang.el ends here
