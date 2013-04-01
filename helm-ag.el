;;; helm-ag.el --- the silver search with helm interface

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-ag
;; Version: 0.04
;; Package-Requires: ((helm "1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'helm)
(require 'eshell)

(defgroup helm-ag nil
  "the silver searcher with helm interface"
  :group 'helm)

(defcustom helm-ag-base-command "ag --nocolor --nogroup"
  "Base command of `ag'"
  :type 'string
  :group 'helm-ag)

(defcustom helm-ag-command-option nil
  "Command line option of `ag'. This is appended after `helm-ag-base-command'"
  :type 'string
  :group 'helm-ag)

(defcustom helm-ag-insert-at-point nil
  "Insert thing at point as search pattern.
   You can set value same as `thing-at-point'"
  :type 'symbol
  :group 'helm-ag)

(defvar helm-ag-command-history '())
(defvar helm-ag-context-stack nil)
(defvar helm-ag-default-directory nil)
(defvar helm-ag-last-default-directory nil)

(defun helm-ag-save-current-context ()
  (helm-aif (buffer-file-name helm-current-buffer)
      (let ((curpoint (with-helm-current-buffer
                        (point))))
        (push (list :file it :point curpoint) helm-ag-context-stack))))

(defun helm-ag-initial-command ()
  (substring-no-properties
   (format "%s%s%s"
           helm-ag-base-command
           (if helm-ag-command-option
               (format " %s" helm-ag-command-option)
             "")
           (if helm-ag-insert-at-point
               (format " %s"
                       (with-helm-current-buffer
                         (or (thing-at-point helm-ag-insert-at-point) " ")))
             " "))))

(defun helm-ag-init ()
  (let* ((cmd (read-string "Ag: "
                           (helm-ag-initial-command)
                           'helm-ag-command-history)))
    (helm-attrset 'recenter t)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let ((default-directory (or helm-ag-default-directory
                                   default-directory)))
        (eshell-command (helm-aif (helm-attr 'search-this-file)
                            (format "%s %s" cmd it)
                          cmd) t)
        (helm-ag-save-current-context))
      (when (zerop (length (buffer-string)))
        (error "No output: '%s'" cmd))
      (unless (zerop eshell-last-command-status)
        (error "Failed: '%s'" cmd)))))

(defun helm-ag-find-file-action (candidate find-func)
  (let* ((elems (split-string candidate ":"))
         (search-this-file (helm-attr 'search-this-file))
         (filename (or search-this-file (first elems)))
         (line (string-to-number (if search-this-file
                                     (first elems)
                                   (second elems))))
         (default-directory (or helm-ag-default-directory
                                helm-ag-last-default-directory)))
    (setq helm-ag-last-default-directory default-directory)
    (funcall find-func filename)
    (goto-char (point-min))
    (forward-line (1- line))))

(defvar helm-ag-source
  '((name . "the silver searcher")
    (init . helm-ag-init)
    (candidates-in-buffer)
    (action . (("Open File" . (lambda (c)
                                (helm-ag-find-file-action c 'find-file)))
               ("Open File Other Window" .
                (lambda (c)
                  (helm-ag-find-file-action c 'find-file-other-window)))))
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-ag-pop-stack ()
  (interactive)
  (let ((context (pop helm-ag-context-stack)))
    (unless context
      (error "Context stack is empty!!"))
    (let ((file (plist-get context :file))
          (curpoint (plist-get context :point)))
      (find-file file)
      (goto-char curpoint))))

;;;###autoload
(defun helm-ag-clear-stack ()
  (interactive)
  (setq helm-ag-context-stack nil))

;;;###autoload
(defun helm-ag-this-file ()
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (helm-attrset 'search-this-file (buffer-file-name) helm-ag-source)
    (helm-attrset 'name (format "Search at %s" filename) helm-ag-source)
    (helm :sources '(helm-ag-source) :buffer "*helm-ag*")))

;;;###autoload
(defun helm-ag ()
  (interactive)
  (let* ((helm-ag-default-directory (if current-prefix-arg
                                        (read-directory-name "Search Directory: ")
                                      default-directory))
         (header-name (format "Search at %s" helm-ag-default-directory)))
    (helm-attrset 'name header-name helm-ag-source)
    (helm :sources '(helm-ag-source) :buffer "*helm-ag*")))

(provide 'helm-ag)

;;; helm-ag.el ends here
