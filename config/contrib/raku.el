;;; raku.el --- raku mode -*-coding:utf-8-*-

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

;; Mode and more for writing Raku code. Note: Raku was once known as
;; Perl 6.

;; See https://github.com/hinrik/perl6-mode
;; and https://github.com/hinrik/flycheck-perl6

;;; Code:

(unless (package-installed-p 'perl6-mode)
  (package-install 'perl6-mode))


(if (and (package-installed-p 'flycheck)
         (not (package-installed-p 'flycheck-perl6)))
    (progn
      (package-install 'flycheck-perl6)))

;;; raku.el ends here
