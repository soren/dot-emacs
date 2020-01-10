;;; 01-flycheck.el --- flycheck set up -*-coding:utf-8-*-

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

;; On-the-fly syntax checking. Replacement for the built-in Flymake.

;; See https://www.flycheck.org/en/latest/
;; and https://www.masteringemacs.org/article/spotlight-flycheck-a-flymake-replacement

;;; Code:

(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

(require 'flycheck)

(if window-system
    ;; Redefine bitmap, as I'm using a 16 bit wide fringe
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [#b0000000000000000
       #b1110001110000000
       #b0111000111000000
       #b0011100011100000
       #b0001110001110000
       #b0000111000111000
       #b0000011100011100
       #b0000001110001110
       #b0000000111000111
       #b0000001110001110
       #b0000011100011100
       #b0000111000111000
       #b0001110001110000
       #b0011100011100000
       #b0111000111000000
       #b1110001110000000] 16 16))

;;; 01-flycheck.el ends here
