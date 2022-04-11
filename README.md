# cilk-mode

[![MELPA](https://melpa.org/packages/cilk-mode-badge.svg)](https://melpa.org/#/cilk-mode)
[![MELPA Stable](https://stable.melpa.org/packages/cilk-mode-badge.svg)](https://stable.melpa.org/#/cilk-mode)
[![MIT License](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Emacs minor mode for Cilk source code editing.

### Contents

  - [Features](#features)
  - [Installation](#installation)
  - [Configuration example](#configuration-example)


## Features

This package provides `cilk-mode`, a minor mode that can be activated in
buffers with major mode `c-mode`, `c++-mode`, or derived modes. The
`cilk-mode` minor mode enables the features listed in the following
table and described below.

| Feature                            | Function                      | Auto-enable variable                 |
|:-----------------------------------|:------------------------------|:-------------------------------------|
| Cilk keyword indentation           | `cilk-mode-cc-keywords`       | `cilk-mode-enable-cc-keywords`       |
| Cilk keyword highlighting          | `cilk-mode-font-lock`         | `cilk-mode-enable-font-lock`         |
| Flycheck with [OpenCilk][opencilk] | `cilk-mode-flycheck-opencilk` | `cilk-mode-enable-flycheck-opencilk` |

[opencilk]: https://opencilk.org

By default, all features are activated/deactivated together with
`cilk-mode`.

  - To explicitly enable, disable, or toggle a feature, use the
    corresponding function in the table above. The functions do not
    require `cilk-mode` to be active (but it must be loaded, of course).
  - To prevent a feature from being automatically enabled in
    `cilk-mode`, set the corresponding auto-enable variable to `nil`.

### Correct indentation with Cilk keywords in CC Mode

| ![][png-cilk-kwd-indent-off]                | ![][png-cilk-kwd-indent-on]   |
|:-------------------------------------------:|:-----------------------------:|
| in `c-mode`, before `cilk-mode-cc-keywords` | after `cilk-mode-cc-keywords` |

[png-cilk-kwd-indent-off]: screenshots/cilk-mode-example_cc-off_font-off_flycheck-on.png
[png-cilk-kwd-indent-on]: screenshots/cilk-mode-example_cc-on_font-off_flycheck-on.png

### Custom font-lock face for Cilk keyword highlighting

Cilk keywords are fontified with the `cilk-mode-parallel-keyword` face.
By default, the latter is the same as `font-lock-keyword-face` but can
be customized without affecting the "regular" C/C++ keywords.

| ![][png-cilk-kwd-highlight-off] | ![][png-cilk-kwd-highlight-on]                |
|:-------------------------------:|:---------------------------------------------:|
| before `cilk-mode-font-lock`    | after `cilk-mode-font-lock` (customized face) |

[png-cilk-kwd-highlight-off]: screenshots/cilk-mode-example_cc-on_font-off_flycheck-on.png
[png-cilk-kwd-highlight-on]: screenshots/cilk-mode-example_cc-on_font-on_flycheck-on.png

### Flycheck syntax checking with OpenCilk

The path to the [OpenCilk][opencilk] compiler can be customized via the
variable `cilk-mode-flycheck-opencilk-executable`; its default value is
`"/opt/opencilk/bin/clang"`. This feature requires the `flycheck` package and
works with the `flycheck-mode` minor mode.

| ![][png-cilk-flycheck-off]           | ![][png-cilk-flycheck-on]           |
|:------------------------------------:|:-----------------------------------:|
| before `cilk-mode-flycheck-opencilk` | after `cilk-mode-flycheck-opencilk` |

[png-cilk-flycheck-off]: screenshots/cilk-mode-example_cc-on_font-on_flycheck-off.png
[png-cilk-flycheck-on]: screenshots/cilk-mode-example_cc-on_font-on_flycheck-on.png


## Installation

### Manual

Download `cilk-mode.el` and add the following to your `.emacs` file:

``` emacs-lisp
(add-to-list 'load-path "/path/to/cilk-mode-parent-dir")
(require 'cilk-mode)
```

### MELPA

Users of GNU Emacs (version 24.1 or greater) can install `cilk-mode` via
[MELPA](https://melpa.org/#/cilk-mode). To use the MELPA repository, make sure
you have the following in your `.emacs` file:

``` emacs-lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

Then, run `M-x package-list-packages` to find and install `cilk-mode`.

For more information, see the [MELPA Getting Started
Guide](https://melpa.org/#/getting-started).

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
  (setq cilk-mode-opencilk-executable "/opt/opencilk/bin/clang")
  (setq cilk-mode-enable-flycheck-opencilk nil))

(add-hook! '(c-mode c++-mode) #'cilk-mode)
```
