;;; ccrypt.el --- set up ccrypt wrapper -*-coding:utf-8-*-

;; Copyright (c) 2019 Søren Lund <soren@lund.org>

;; This file is part of dot-emacs.

;; dot-emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; dot-emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with dot-emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; If ccrypt is installed, then download and use the ps-ccrypt module.

;; See http://ccrypt.sourceforge.net/

;;; Code:

(if (locate-file "ccrypt" exec-path exec-suffixes 'file-executable-p)
    (let ((url "http://ccrypt.sourceforge.net/download/ps-ccrypt.el"))
      (let ((target (concat slu-dot-emacs-lisp-dir (file-name-nondirectory url))))
        (if (not (file-exists-p target))
            (url-copy-file url target 't))
        (load-file target)))

  (require 'ps-ccrypt))

;;; ccrypt.el ends here
