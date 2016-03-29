# helm-ag.el

[![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]


## Introduction

`helm-ag.el` provides interfaces of [The Silver Searcher](https://github.com/ggreer/the_silver_searcher) with helm.


## Features

- Support multiple search tools(ag, [the platinum searcher][pt-link], [ack][ack-link] etc)
- Edit search result like [wgrep](https://github.com/mhayashi1120/Emacs-wgrep)


## Screenshot

![helm-ag](image/helm-ag.png)


## Requirements

* Emacs 24 or higher
* helm 1.7.7 or higher
* [The Silver Searcher](https://github.com/ggreer/the_silver_searcher) 0.29.0 or higher.

## Installation

`helm-ag` is available on [MELPA](https://melpa.org/) and [MELPA stable](https://stable.melpa.org/)

You can install `helm-ag` with the following command.

<kbd>M-x package-install [RET] helm-ag [RET]</kbd>


## Basic Usage

#### `helm-ag`

Input search word with `ag` command. You can change search directory
with `C-u` prefix.

#### `helm-ag-this-file`

Same as `helm-ag` except to search only current file

#### `helm-do-ag`

Search with `ag` like `helm-do-grep-ag `, `helm-grep-do-git-grep`.
You can specify extra command line option of `ag` with minus prefix(`M--` or `C--`).

![helm-do-ag](image/helm-do-ag.gif)

#### `helm-do-ag-this-file`

Same as `helm-do-ag` except to search only current file

#### `helm-ag-project-root`

Call `helm-ag` at project root. `helm-ag` seems directory as project root where
there is `.git` or `.hg` or `.svn`.


#### `helm-do-ag-project-root`

Call `helm-do-ag` at project root.


#### `helm-ag-buffers`

Search buffers by `helm-ag`


#### `helm-do-ag-buffers`

Search buffers by `helm-do-ag`


#### `helm-ag-pop-stack`

Move to point before jump

#### `helm-ag-clear-stack`

Clear context stack


## Enable helm-follow-mode by default

Please set `helm-follow-mode-persistent` to non-nil if you want to use `helm-follow-mode` by default. You must set it before loading `helm-ag.el`.

``` lisp
(custom-set-variables
 '(helm-follow-mode-persistent t))
```


## Persistent action

You can see file content temporarily by persistent action(`C-j`).


## Search Tips of `helm-ag`

#### Passing command line options and pattern

```
Pattern: -Gmd$ search_pattern
```

Command line options is `-Gmd$` and search pattern is `search_pattern`.
`helm-ag` treats words which starts with `-` as command line option.

#### Pattern contains space(`helm-do-ag`)

```
Pattern: foo\ bar\ baz
```

Search pattern is `foo\ bar\ baz`. You need to escape spaces with backslash.

In `helm-ag`, you need not to escape spaces.


#### Pattern starts with `-`

```
Pattern: -- --count
```

Search pattern is `--count`.
`helm-ag` treats words after `--` as search pattern.

#### Search meta characters as literal

`ag`(`ack`, `pt`) takes Perl compatible PCRE so that you need to escape meta characters
likes brackets, braces, asterisk, when you search them as literals.

## Customize

#### `helm-ag-base-command`(Default: `ag --nocolor --nogroup`)

Base command of `ag`. Windows users should set `--vimgrep` option for using `helm-do-ag`.

#### `helm-ag-command-option`(Default: `nil`)

Command line option of base command.

#### `helm-ag-insert-at-point`(Default: `nil`)

Insert thing at point as default search pattern, if this value is `non nil`.
You can set the parameter same as `thing-at-point`(Such as `'word`, `symbol` etc).

#### `helm-ag-fuzzy-match`(Default: `nil`)

Enable fuzzy matching.

#### `helm-ag-use-grep-ignore-list`(Default: `nil`)

Use `grep-find-ignored-files` and `grep-find-ignored-directories` as ignore pattern.
They are specified to `--ignore' options."

#### `helm-ag-always-set-extra-option`(Default: `nil`)

Always set extra command line option of `ag` in `helm-do-ag`
if this value is non-nil.

#### `helm-ag-edit-save`(Default: `t`)

Save buffers you edit at editing completed.

#### `helm-ag-use-emacs-lisp-regexp`(Default: `nil`)

Use Emacs Lisp regexp instead of PCRE as pattern.
NOTE: this is very simple conversion.

#### `helm-ag-use-agignore`(Default: `nil`)

Use `.agignore` file at project root if this variable is non nil.

#### `helm-ag-use-temp-buffer`(Default: `nil`)

Use temporary buffer and not open file for persistent action.

#### NOTE

`helm` removes `file-line` type feature from 1.6.9. So `helm-ag-source-type` is no longer available.


## Keymap

`helm-ag-map` and `helm-do-ag-map` are inherited by `helm-map`.

| Key              | Action                                                                     |
|:-----------------|:---------------------------------------------------------------------------|
| `C-c o`          | Open other window                                                          |
| `C-l`            | Search in parent directory                                                 |
| `C-c C-e`        | Switch to edit mode                                                        |
| `C-x C-s`        | Save ag results to buffer(Ask save buffer name if prefix key is specified) |
| `C-c C-f`        | Enable helm-follow-mode                                                    |
| `C-c >`, `right` | Move to next file                                                          |
| `C-c <`, `left`  | Move to previous file                                                      |
| `C-c ?`          | Show help message                                                          |


### Edit mode keymap

| Key       | Action           |
|:----------|:-----------------|
| `C-c C-c` | Commit changes   |
| `C-c C-k` | Abort            |
| `C-c C-d` | Mark delete line |
| `C-c C-u` | Unmark           |

You can use `next-error` and `previous-error` for seeing file content which
current line indicates.

### Saved buffer keymap

| Key   | Action                                      |
|:------|:--------------------------------------------|
| `RET` | Jump to current line posion                 |
| `C-o` | Jump to current line posion in other window |
| `g`   | Update result                               |


## Sample Configuration

```lisp
(custom-set-variables
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point 'symbol))
```

## helm-ag.el with [the platinum searcher][pt-link] or [ack][ack-link] or [sift](https://github.com/svent/sift)

`helm-ag.el` can work the platinum searcher or ack instead of the silver searcher.
It is difficult to install the silver search on some platform such as Windows,
while it is easy to install the platinum searcher because it is written in
go language and its author provides binaries for some platforms. And ack is written in Perl.

NOTE: Please use pt 1.7.7 or higher version if you use the platinum searcher.

Please add following configuration if you use `helm-ag` with the platinum searcher. I recommend you to set `-e` option which enables regexp searching.

```lisp
(custom-set-variables
 '(helm-ag-base-command "pt -e --nocolor --nogroup"))
```

or using `ack`

```lisp
(custom-set-variables
 '(helm-ag-base-command "ack --nocolor --nogroup"))
```

or using `shift`

```lisp
(custom-set-variables
 '(helm-ag-base-command "sift --no-color -n"))
```

#### NOTE: For pt users

Case using `ag` or `ack`, `helm-do-ag` convert pattern from `foo bar` to `"(?=.*" foo ".*)(?=.*" bar ".*)"`
which patterns matches line which contains both `foo` and `bar`. But case using `pt`, `helm-do-ag`
does not convert because Golang `regexp`(`pt` is written in Golang) does not support look-a-head pattern.
So using `pt` behaviors is different from `ag` when you use such pattern. I suppose there is same problem
if you use search programs which are written in Golang.

## Alternatives

[ag.el](https://github.com/Wilfred/ag.el) provides `M-x grep` interface.
Also it can work without helm.

[travis-badge]: https://travis-ci.org/syohex/emacs-helm-ag.svg
[travis-link]: https://travis-ci.org/syohex/emacs-helm-ag
[melpa-link]: https://melpa.org/#/helm-ag
[melpa-stable-link]: https://stable.melpa.org/#/helm-ag
[melpa-badge]: https://melpa.org/packages/helm-ag-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/helm-ag-badge.svg
[ack-link]: http://beyondgrep.com/
[pt-link]: https://github.com/monochromegane/the_platinum_searcher
