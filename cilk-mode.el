;;; cilk-mode.el --- Minor mode for Cilk code editing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Supertech Research Group, CSAIL, MIT
;;
;; Author: Alexandros-Stavros Iliopoulos <https://github.com/ailiop>
;; Maintainer: Alexandros-Stavros Iliopoulos <1577182+ailiop@users.noreply.github.com>
;; Created: November 30, 2021
;; Modified: November 30, 2021
;; Version: 0.1.0
;; Keywords: c convenience faces languages
;; Homepage: https://github.com/ailiop/cilk-mode
;; Package-Requires: ((emacs "25.1") (flycheck "32-cvs") (lsp-mode "8.0"))
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
;;  all copies or substantial portions of the Software.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;  IN THE SOFTWARE.
;;
;;; Commentary:
;;
;; Provides `cilk-mode', a minor mode that augments `c-mode' and `c++-mode'
;; for editing Cilk source code.
;;
;; This package simply groups together a few small customizations of other
;; modes to make Cilk C/C++ code editing more convenient.  Specifically, the
;; package includes the following features:
;;
;; 1. Correct indentation of code blocks with Cilk keywords.  This is done by
;; modifying buffer-local bindings of C/C++ keyword regexp variables of
;; `cc-mode'.
;;
;; 2. Custom fontification of Cilk keywords.  This is done via
;; `font-lock-mode' and the provided face `cilk-mode-parallel-keyword'.  By
;; default, the `cilk-mode-parallel-keyword' face is the same as
;; `font-lock-keyword-face' except with foreground color `#FDA900' (mustard).
;; Note that if this feature is disabled, Cilk keywords will not be fontified
;; at all; that is, there is no "fallback" to fontifying the keywords using
;; `cc-mode'.
;;
;; 3. Syntax checking with `flycheck' and the OpenCilk compiler
;; (https://opencilk.org).  This is done via buffer-local bindings of
;; `flycheck' options for the `c/c++-clang' checker.  If `flycheck' is not
;; installed, this feature is elided.  The OpenCilk compiler path is
;; found in `cilk-mode-flycheck-opencilk-executable'.
;;
;; 4. Easy setup for `lsp-mode' integration using the OpenCilk clangd language
;; server.  This is done simply via a buffer-local binding of
;; `lsp-clients-clangd-executable'.  It is up to the user to ensure that
;; `lsp-mode' is activated in the relevant buffer, as well as to ensure that
;; it uses clangd.  If `lsp-mode' is not installed, this feature is elided.
;;
;; Each of the above features is enabled/disabled via hook functions for
;; `cilk-mode'.  To disable any of the feature hook functions, set the value
;; of the corresponding variable below to `nil' BEFORE loading the package:
;;
;; 1. flag `cilk-mode-enable-cc-keywords'
;;      -> hook function `cilk-mode-cc-keywords'
;; 2. flag `cilk-mode-enable-font-lock' ->
;;      -> hook function `cilk-mode-font-lock'
;; 3. flag `cilk-mode-enable-flycheck-opencilk'
;;      -> hook function `cilk-mode-flycheck-opencilk'
;; 4. flag `cilk-mode-enable-lsp-opencilk'
;;      -> hook function `cilk-mode-lsp-opencilk'
;;
;; Each of the hook functions above can also be called interactively (in any
;; mode) to toggle the corresponding feature.
;;
;; The `cilk-mode' minor mode can only be enabled in buffers with major mode
;; `c-mode' or `c++-mode' (provided by `cc-mode').
;;
;;; Using with GNU Emacs:
;;
;; ;; ===== .emacs =====
;; ; (optional) customize init options
;; (setq cilk-mode-opencilk-dir "~/opencilk/build") ; OpenCilk base dir
;; (setq cilk-mode-enable-flycheck-opencilk nil)    ; disable flycheck options
;;
;; ; load package and enable Cilk mode in all C/C++ (or derived) buffers
;; (require 'cilk-mode)
;; (dolist (h '('c-mode-hook c++-mode-hook))
;;   (add-hook h #'cilk-mode))
;;
;; ; (optional) customize Cilk keyword face
;; (set-face-attribute 'cilk-mode-parallel-keyword nil
;;                     :weight 'bold
;;                     :foreground (face-attribute 'font-lock-keyword-face
;;                                                 :foreground))
;;
;;; Installing and using with Doom Emacs:
;;
;; ;; ===== .doom.d/packages.el =====
;; (package! cilk-mode
;;   :recipe (:host github :repo "ailiop/cilk-mode"))
;;
;; ;; ===== .doom.d/config.el =====
;; ; enable Cilk mode in all C/C++ mode (or derived) buffers
;; (add-hook! (c-mode-hook c++-mode-hook) #'cilk-mode) ; enable in all C/C++ buffers
;;
;; ; optional customization
;; (use-package! cilk-mode
;;   :command cilk-mode
;;   :init
;;   (setq cilk-mode-opencilk-dir "~/opencilk/build" ; OpenCilk base dir
;;         cilk-mode-enable-flycheck-opencilk nil)   ; disable flycheck options
;;   :config
;;   ; customize Cilk keyword face (e.g. same color as C/C++ keywords, but bold)
;;   (set-face-attribute 'cilk-mode-parallel-keyword nil
;;                       :weight 'bold
;;                       :foreground (face-attribute 'font-lock-keyword-face
;;                                                   :foreground)))
;;
;;; Code:


(require 'cc-mode)  ; `cilk-mode' is only applicable over `c-mode'/`c++-mode'



;; ==================================================
;;; PARAMETERS
;; ==================================================

(defgroup cilk nil
  "Customization group for `cilk-mode' options."
  :group 'c)


;; ---------- Cilk keywords

(defconst cilk-mode--block-stmt-1-kwds '("cilk_scope" "cilk_spawn")
  "Cilk keywords followed directly by a substatement.")

(defconst cilk-mode--block-stmt-2-kwds '("cilk_for")
  "Cilk keywords followed by a paren sexp and then by a substatment.")

(defconst cilk-mode--simple-stmt-kwds '("cilk_sync")
  "Cilk keywords followed by nothing.")

(defconst cilk-mode--all-kwds (append cilk-mode--block-stmt-1-kwds
                                      cilk-mode--block-stmt-2-kwds
                                      cilk-mode--simple-stmt-kwds)
  "All Cilk keywords.")


;; ---------- OpenCilk

(defcustom cilk-mode-opencilk-dir "/opt/opencilk"
  "Installation directory for the OpenCilk compiler.

This is used as the base path for finding the OpenCilk C/C++
compiler (`cilk-mode-flycheck-opencilk-executable') and language
server daemon (`cilk-mode-lsp-clients-opencilk-executable'), ONLY
IF the corresponding variables are not set before loading the
`cilk-mode' package."
  :type 'directory)


(defcustom cilk-mode-opencilk-cilk-flags '("-fopencilk")
  "OpenCilk compiler flags for building Cilk code.

To change non-Cilk-specific arguments to the OpenCilk compiler
for use with flycheck, use `flycheck-clang-args'."
  :type '(repeat string))



;; ==================================================
;;; CC MODE KEYWORD LIST AMENDMENT FOR CILK
;; ==================================================

(defcustom cilk-mode-enable-cc-keywords t
  "If non-nil, register Cilk keywords with CC Mode keyword regexps.

This variable is only checked when the `cilk-mode' package is
loaded, to control whether to add `cilk-mode-cc-keywords' to
`cilk-mode-hook'."
  :type 'boolean)


;; only required at compile time to get the CC Mode C/C++ language constants
(when cilk-mode-enable-cc-keywords
  (eval-when-compile (require 'cc-langs)))


(declare-function cilk-mode-cc-keywords "cilk-mode.el")
(let ((cilk-mode--cc-keywords-state nil))

  (defun cilk-mode-cc-keywords (&optional flag)
    "Toggle Cilk keywords in CC Mode keyword regexps.

If FLAG is nil (equivalently, if no FLAG is specified), update CC
Mode keyword-matching regexps to also match relevant Cilk
keywords.  Specifically, change the following CC Mode regexp
variables for `c-mode' and `c++-mode':

o `c-block-stmt-1-key' ==> also keywords in `cilk-mode--block-stmt-1-kwds'
o `c-block-stmt-2-key' ==> also keywords in `cilk-mode--block-stmt-2-kwds'
o `c-opt-block-stmt-key' ==> (update based on the 2 variables above)
o `c-simple-stmt-key' ==> also keywords in `cilk-mode-simple-stmt-kwds'

If FLAG is non-nil, undo the above changes and set the CC Mode
regexp variables to their original values.

When called interactively, `cilk-mode-cc-keywords' toggles the CC
Mode keyword regexp variables between the OpenCilk and original
settings.

This function should only be called from buffers where the major
mode is `c-mode' or `c++-mode'."
    (interactive)
    (if (called-interactively-p 'any)
        (cilk-mode-cc-keywords cilk-mode--cc-keywords-state)
      (setq cilk-mode--cc-keywords-state (not flag))
      (setq c-block-stmt-1-key
            (c-make-keywords-re t
              (append (c-lang-const c-block-stmt-1-kwds)
                      (unless flag cilk-mode--block-stmt-1-kwds))))
      (setq c-block-stmt-2-key
            (c-make-keywords-re t
              (append (c-lang-const c-block-stmt-2-kwds)
                      (unless flag cilk-mode--block-stmt-2-kwds))))
      (setq c-opt-block-stmt-key
            (c-make-keywords-re t
              (append (c-lang-const c-block-stmt-1-kwds)
                      (c-lang-const c-block-stmt-2-kwds)
                      (unless flag (append cilk-mode--block-stmt-1-kwds
                                           cilk-mode--block-stmt-2-kwds)))))
      (setq c-simple-stmt-key
            (c-make-keywords-re t
              (append (c-lang-const c-simple-stmt-kwds)
                      (unless flag cilk-mode--simple-stmt-kwds)))))))



;; ==================================================
;;; FONT LOCKING OF CILK PARALLEL KEYWORDS
;; ==================================================

(defcustom cilk-mode-enable-font-lock (require 'font-lock nil 'noerror)
  "If non-nil, enable automatic Cilk keyword font-locking.

This variable is only checked when the `cilk-mode' package is
loaded, to control whether to add `cilk-mode-font-lock' to
`cilk-mode-hook'."
  :type 'boolean)


;; in case `cilk-mode-enable-font-lock' was pre-set to non-nil value
(when cilk-mode-enable-font-lock (require 'font-lock))


(defface cilk-mode-parallel-keyword
  `((t :inherit 'font-lock-keyword-face
       :foreground "#FDA900"))
  "Cilk mode face used to highlight parallel Cilk keywords.")


(defconst cilk-mode--cilk-keywords-regexp
  (regexp-opt cilk-mode--all-kwds 'symbols)
  "Optimized regexp for matching Cilk keyword symbols.")


(declare-function cilk-mode-font-lock "cilk-mode.el")
(let ((cilk-mode--font-lock-state nil))

  (defun cilk-mode-font-lock (&optional flag)
    "Toggle Cilk keyword fontification in current buffer.

If FLAG is nil (equivalently, if no FLAG is specified), apply the
`cilk-mode-parallel-keyword' face to all Cilk keywords (found in
`cilk-mode--all-kwds').

If flag is non-nil, remove the face.

When called interactively, `cilk-mode-font-lock' toggles the
fontification of Cilk keywords in the current buffer."
    (interactive)
    (if (called-interactively-p 'any)
        (cilk-mode-font-lock cilk-mode--font-lock-state)
      (setq cilk-mode--font-lock-state (not flag))
      ;; add/remove custom face to/from Cilk keywords
      ;; (NOTE: Use `font-lock-add-keywords' and `font-lock-remove-keywords'
      ;; instead of the fontification mechanisms in `cc-mode', because the
      ;; latter would seem to require copying an entire matcher like
      ;; `c-cpp-matchers'.)
      (let ((font-lock-cilk-keywords-list
             `((,cilk-mode--cilk-keywords-regexp . 'cilk-mode-parallel-keyword))))
        (funcall (if cilk-mode--font-lock-state
                     #'font-lock-add-keywords
                   #'font-lock-remove-keywords)
                 nil font-lock-cilk-keywords-list))
      ;; re-fontify buffer (see also <https://emacs.stackexchange.com/a/10169>)
      (if (fboundp 'font-lock-flush)
          (font-lock-flush)
        (when font-lock-mode
          (with-no-warnings (font-lock-fontify-buffer)))))))



;; ==================================================
;;; FLYCHECK SYNTAX CHECKING
;; ==================================================

(defcustom cilk-mode-enable-flycheck-opencilk (require 'flycheck nil 'noerror)
  "If non-nil, enable Cilk syntax checking with `flycheck' in `cilk-mode'.

This variable is only checked when the `cilk-mode' package is
loaded, to control whether to add `cilk-mode-flycheck-opencilk'
to `cilk-mode-hook'."
  :type 'boolean)


;; in case `cilk-mode-enable-flycheck-opencilk' was pre-set to non-nil value
(when cilk-mode-enable-flycheck-opencilk (require 'flycheck))


(when (require 'flycheck nil 'noerror)


  (defvar flycheck-c/c++-clang-executable)
  (defvar flycheck-clang-args)
  (defvar flycheck-checker)
  (declare-function flycheck-buffer "flycheck.el" nil)


  (defcustom cilk-mode-flycheck-opencilk-executable
    (expand-file-name "clang" (expand-file-name "bin/" cilk-mode-opencilk-dir))
    "The executable of the OpenCilk syntax checker (i.e., compiler).

The value of `cilk-mode-flycheck-opencilk-executable' replaces
that of `flycheck-c/c++-clang-executable' in `cilk-mode' buffers."
    :type '(file :must-match t))


  (declare-function cilk-mode-flycheck-opencilk "cilk-mode.el")
  (let ((cilk-mode--flycheck-opencilk-state nil))

    (defun cilk-mode-flycheck-opencilk (&optional flag)
      "Toggle Cilk options for the `c/c++-clang' `flycheck' checker.

If FLAG is nil (equivalently, if no FLAG is specified), ensure
the buffer-local `flycheck-checker' value is `c/c++-clang'.
Also, create the following local bindings with values from the
corresponding `cilk-mode' constants:

o `flycheck-c/c++-clang-executable'  = `cilk-mode-flycheck-opencilk-executable'
o `flycheck-clang-args'             += `cilk-mode-opencilk-cilk-flags'

If FLAG is non-nil, delete the buffer-local bindings and undo the
above options to their original setting.

When called interactively, `cilk-mode-flycheck-opencilk' toggles
the above flycheck options between the OpenCilk and original settings."
      (interactive)
      (if (called-interactively-p 'any)
          (cilk-mode-flycheck-opencilk cilk-mode--flycheck-opencilk-state)
        (setq cilk-mode--flycheck-opencilk-state (not flag))
        (if cilk-mode--flycheck-opencilk-state
            (progn
              (setq-local flycheck-c/c++-clang-executable
                          cilk-mode-flycheck-opencilk-executable)
              (setq-local flycheck-clang-args
                          (append flycheck-clang-args
                                  cilk-mode-opencilk-cilk-flags))
              (setq-local flycheck-checker 'c/c++-clang))
          (kill-local-variable 'flycheck-c/c++-clang-executable)
          (kill-local-variable 'flycheck-clang-args)
          (kill-local-variable 'flycheck-checker))
        (when (bound-and-true-p flycheck-mode)
          (flycheck-buffer))))))



;; ==================================================
;;; OPENCILK WITH LSP-MODE
;; ==================================================

(defcustom cilk-mode-enable-lsp-opencilk (require 'lsp-mode nil 'noerror)
  "If non-nil, use the OpenCilk language server as the `lsp-mode' clangd client.

This variable is only checked when the `cilk-mode' package is
loaded, to control whether to add `cilk-mode-lsp-opencilk' to the
`cilk-mode' on/off hooks (partially evaluated with t/nil
input)."
  :type 'boolean)


;; in case `cilk-mode-enable-lsp-opencilk' was pre-set to non-nil value
(when cilk-mode-enable-lsp-opencilk (require 'lsp-mode))


(when (require 'lsp-mode nil 'noerror)


  (defvar lsp-mode)
  (defvar lsp-clients-clangd-executable)


  (defcustom cilk-mode-lsp-clients-opencilk-executable
    (expand-file-name "clangd" (expand-file-name "bin/" cilk-mode-opencilk-dir))
    "The executable of the OpenCilk language server.

The value of `cilk-mode-lsp-clients-opencilk-executable' replaces
that of `lsp-clients-clangd-executable' in `cilk-mode' buffers."
    :type '(file :must-match t))


  (declare-function cilk-mode-lsp-opencilk "cilk-mode.el")
  (let ((cilk-mode--lsp-opencilk-state nil))

    (defun cilk-mode-lsp-opencilk (&optional flag)
      "Toggle OpenCilk as the LSP clangd client.

If FLAG is nil (equivalently, if no FLAG is specified), set the
OpenCilk language server as the LSP clangd client executable.

If FLAG is non-nil, restore the LSP clangd client executable to
its original value (if `cilk-mode-lsp-opencilk' was run before).

When called interactively, `cilk-mode-lsp-opencilk' toggles the
LSP clangd client executable value between the OpenCilk and
original settings."
      (interactive)
      (if (called-interactively-p 'any)
          (cilk-mode-lsp-opencilk cilk-mode--lsp-opencilk-state)
        (setq cilk-mode--lsp-opencilk-state (not flag))
        (if cilk-mode--lsp-opencilk-state
            (setq-local lsp-clients-clangd-executable
                        cilk-mode-lsp-clients-opencilk-executable)
          (kill-local-variable 'lsp-clients-clangd-executable))))))



;; ==================================================
;;; CILK MODE
;; ==================================================

;;;###autoload
(define-minor-mode cilk-mode
  "Minor mode for editing Cilk source code.

Can only be enabled in buffers where `major-mode' is either
`c-mode' or `c++-mode' (derived from CC Mode).

The `cilk-mode' minor mode by itself does nothing but provide a
way to run Cilk-specific functions via `cilk-mode-hook'.  When
loading the `cilk-mode' package, the following functions are
added to `cilk-mode-hook' if the corresponding flag variables are
set (at package initialization time):

1. `cilk-mode-cc-keywords'       (flag: `cilk-mode-enable-cc-keywords')
2. `cilk-mode-font-lock'         (flag: `cilk-mode-enable-font-lock')
3. `cilk-mode-flycheck-opencilk' (flag: `cilk-mode-enable-flycheck-opencilk')
4. `cilk-mode-lsp-opencilk'      (flag: `cilk-mode-enable-lsp-opencilk')"
  :lighter " Cilk"
  (when (and (bound-and-true-p c-buffer-is-cc-mode)
             (member major-mode '('c-mode 'c++-mode)))
    (error "Unsupported major mode `%s' for minor mode `cilk-mode'" major-mode)))


(when cilk-mode-enable-cc-keywords
  (add-hook 'cilk-mode-hook
            (lambda () (cilk-mode-cc-keywords (not cilk-mode)))))

(when cilk-mode-enable-font-lock
  (add-hook 'cilk-mode-hook
            (lambda () (cilk-mode-font-lock (not cilk-mode)))))

(when (and cilk-mode-enable-flycheck-opencilk
           (fboundp 'cilk-mode-flycheck-opencilk))
  (add-hook 'cilk-mode-hook
            (lambda () (cilk-mode-flycheck-opencilk (not cilk-mode)))))

(when (and cilk-mode-enable-lsp-opencilk
           (fboundp 'cilk-mode-lsp-opencilk))
  (add-hook 'cilk-mode-hook
            (lambda () (cilk-mode-lsp-opencilk (not cilk-mode)))))


(provide 'cilk-mode)

;;; cilk-mode.el ends here
