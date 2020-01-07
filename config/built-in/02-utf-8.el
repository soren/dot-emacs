;;; 02-utf-8.el --- set utf-8 settings -*-coding:utf-8-*-

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

;; This file makes Emacs default to UTF-8 encoding.

;; Inspiration from
;; http://blog.jonnay.net/archives/820-Emacs-and-UTF-8-Encoding.html

;;; Code:

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(if (eq window-system 'x)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(if (eq system-type 'windows-nt)
    (set-clipboard-coding-system 'utf-16le-dos))

;;; 02-utf-8.el ends here
