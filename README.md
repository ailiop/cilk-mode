# cilk-mode

[![MELPA](https://melpa.org/packages/cilk-mode-badge.svg)](https://melpa.org/#/cilk-mode)
[![MELPA Stable](https://stable.melpa.org/packages/cilk-mode-badge.svg)](https://stable.melpa.org/#/cilk-mode)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Emacs minor mode for Cilk source code editing.


## Features

By default, all of the features below are activated/deactivated together with
`cilk-mode`, which in turn can only be activated in a buffer with major mode
derived from `c-mode` or `c++-mode`.

Each feature can also be toggled by calling its corresponding function, as
listed below.  To prevent a feature with corresponding function
`cilk-mode-<feature>` from being automatically enabled in `cilk-mode`, set the
variable `cilk-mode-enable-<feature>` to `nil`.


### Correct indentation with Cilk keywords in CC Mode 

Toggled with the function `cilk-mode-cc-keywords`.

| ![](screenshots/cilk-mode-example_cc-off_font-off_flycheck-on.png) | ![](screenshots/cilk-mode-example_cc-on_font-off_flycheck-on.png) |
|:------------------------------------------------------------------:|:-----------------------------------------------------------------:|
| in `c-mode`, before `cilk-mode-cc-keywords`                        | after `cilk-mode-cc-keywords`                                     |

### Custom font-lock face for Cilk keyword highlighting

Toggled with the function `cilk-mode-font-lock`.  Cilk keywords are fontified
with the `cilk-mode-parallel-keyword` face.  By default, the latter is the
same as `font-lock-keyword-face` but can be customized without affecting the
"regular" C/C++ keywords.

| ![](screenshots/cilk-mode-example_cc-on_font-off_flycheck-on.png) | ![](screenshots/cilk-mode-example_cc-on_font-on_flycheck-on.png) |
|:-----------------------------------------------------------------:|:----------------------------------------------------------------:|
| before `cilk-mode-font-lock`                                      | after `cilk-mode-font-lock`                                      |

### Flycheck syntax checking with OpenCilk

Toggled with the function `cilk-mode-flycheck-opencilk`.  The path to the
[OpenCilk](https://opencilk.org) compiler can be set via
`cilk-mode-flycheck-opencilk-executable`.  This feature requires the
`flycheck` package and works with the `flycheck-mode` minor mode.

| ![](screenshots/cilk-mode-example_cc-on_font-on_flycheck-off.png) | ![](screenshots/cilk-mode-example_cc-on_font-on_flycheck-on.png) |
|:-----------------------------------------------------------------:|:----------------------------------------------------------------:|
| before `cilk-mode-flycheck-opencilk`                                     | after `cilk-mode-flycheck-opencilk`                              |


## Installation

### Manual

Add the following to your `.emacs` file:

``` emacs-lisp
(add-to-list 'load-path "/path/to/cilk-mode-parent-dir")
(require 'cilk-mode)
```

### MELPA

Users of GNU Emacs (version 24.1 or greater) can install `cilk-mode` via
[MELPA](https://melpa.org/#/getting-started).  To use the MELPA repository,
make sure you have the following in your `.emacs` file:

``` emacs-lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

Then, run `M-x package-list-packages` to find and install `cilk-mode`.

### Doom Emacs

In `.doom.d/packages.el`:

``` emacs-lisp
(package! cilk-mode
  :recipe (:host github :repo "ailiop/cilk-mode"))
```


## Configuration example

### GNU Emacs

In your `.emacs` file:

``` emacs-lisp
;; load package
(add-to-list 'load-path "/path/to/cilk-mode-parent-dir") ; if necessary
(require 'cilk-mode)

;; automatically enable cilk-mode within c-mode and c++-mode
(dolist (h '('c-mode-hook 'c++-mode-hook))
  (add-hook h #'cilk-mode))
  
;; customize Cilk keyword face
(set-face-attribute 'cilk-mode-parallel-keyword nil
                    :foreground "#FDA900") ; mustard color

;; customize path to the OpenCilk compiler (for use with flycheck)
(setq cilk-mode-opencilk-executable "/opt/opencilk/bin/clang")
                                                
;; do not enable flycheck with OpenCilk by default in cilk-mode
(setq cilk-mode-enable-flycheck-opencilk nil)
```

### Doom Emacs

In `.doom.d/config.el`:

``` emacs-lisp
(use-package! cilk-mode
  :command cilk-mode
  :config
  (set-face-attribute 'cilk-mode-parallel-keyword nil
                      :foreground "#FDA900") ; mustard color
  (setq cilk-mode-opencilk-executable "/opt/opencilk/bin/clang"))
  (setq cilk-mode-enable-flycheck-opencilk nil)

(add-hook! (c-mode c++-mode) #'cilk-mode)
```
