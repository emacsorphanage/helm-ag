# helm-ag.el

## Introduction
`helm-ag.el` provides interfaces of [the silver searcher](https://github.com/ggreer/the_silver_searcher) with helm.


## Screenshot

![helm-ag](image/helm-ag.png)


## Requirements

* Emacs 24 or higher
* helm 1.0 or higher
* [the silver searcher](https://github.com/ggreer/the_silver_searcher).


## Basic Usage

Input search word with `ag` command.

    M-x helm-ag

Same as `helm-ag` except to search only current file

    M-x helm-ag-this-file

Move to point before jump

    M-x helm-ag-pop-stack

Clear context stack

    M-x helm-ag-pop-stack


## Customize

Base ag command, default is `ag --nocolor --nogroup`

    (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")

Command line option of base command. Default is `nil`.

    (setq helm-ag-command-option "--all-text")
