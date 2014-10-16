;;; test-util.el --- test helm-ag utility functions

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

(require 'ert)
(require 'helm-ag)

(ert-deftest construct-command ()
  "helm-ag--construct-command"
  (let ((helm-ag-base-command "ag --nocolor --nogroup"))
    (let ((got (helm-ag--construct-command "somepattern"))
          (expected '("ag" "--nocolor" "--nogroup" "--" "somepattern")))
      (should (equal got expected)))

    (let* ((helm-ag-command-option "--ignore-case --all-text")
           (got (helm-ag--construct-command "somepattern"))
           (expected '("ag" "--nocolor" "--nogroup" "--ignore-case" "--all-text"
                       "--" "somepattern")))
      (should (equal got expected)))))

;;; test-util.el ends here
