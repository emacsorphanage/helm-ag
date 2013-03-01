;;; helm-ag.el --- the silver search with helm interface

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-ag
;; Version: 0.01

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

(eval-when-compile
  (require 'cl))

(require 'helm)
(require 'eshell)

(defgroup helm-ag nil
  "the silver searcher with helm interface"
  :group 'helm)

(defcustom helm-ag-base-command "ag --nocolor --nogroup "
  "Base command of `ag'"
  :type 'string
  :group 'helm-ag)

(defvar helm-ag-command-history '())
(defvar helm-ag-context-stack nil)

(defun helm-ag-save-current-context ()
  (let ((file (buffer-file-name helm-current-buffer))
        (curpoint (with-helm-current-buffer
                    (point))))
    (push (list :file file :point curpoint) helm-ag-context-stack)))

(defun helm-ag-init ()
  (let* ((cmd (read-string "Ag: "
                           helm-ag-base-command 'helm-ag-command-history)))
    (helm-attrset 'recenter t)
    (helm-attrset 'before-jump-hook 'helm-ag-save-current-context)
    (with-current-buffer (helm-candidate-buffer 'global)
      (eshell-command cmd t)
      (when (zerop (length (buffer-string)))
        (error "No output: '%s'" cmd)))))

(defvar helm-ag-source
  '((name . "the silver searcher")
    (init . helm-ag-init)
    (candidates-in-buffer)
    (type . file-line)
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
(defun helm-ag ()
  (interactive)
  (helm :sources '(helm-ag-source) :buffer "*helm-ag*"))

(provide 'helm-ag)

;;; helm-ag.el ends here
