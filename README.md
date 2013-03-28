# helm-ag.el

## Introduction
`helm-ag.el` provides interfaces of [The Silver Searcher](https://github.com/ggreer/the_silver_searcher) with helm.


## Screenshot

![helm-ag](image/helm-ag.png)


## Requirements

* Emacs 23 or higher
* helm 1.0 or higher
* [The Silver Searcher](https://github.com/ggreer/the_silver_searcher).


## Installation

You can install `helm-ag.el` from [MELPA](https://github.com/milkypostman/melpa.git) with package.el (`M-x package-install git-gutter`).


## Basic Usage

#### helm-ag

Input search word with `ag` command. You can change search directory
with `C-u` prefix.

#### helm-ag-this-file

Same as `helm-ag` except to search only current file

#### helm-ag-pop-stack

Move to point before jump

#### helm-ag-clear-stack

Clear context stack


## Persistent action

You can see file content temporarily by persistent action(`C-z`)
at `helm-ag` and `helm-ag-this-file`.


## Customize

#### helm-ag-base-command(Default: `ag --nocolor --nogroup`)

Base command of `ag`.

#### helm-ag-command-option(Default: `nil`)

Command line option of base command.

#### helm-ag-thing-at-point(Default: `'nil`)

Insert thing at point as default search pattern, if this value is `non nil`.
You can set the parameter same as `thing-at-point`(Such as `'word`, `symbol` etc).

### Sample Configuration

```elisp
(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
(setq helm-ag-command-option "--all-text")
(setq helm-ag-thing-at-point 'symbol)
```
