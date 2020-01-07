;;; 01-fringe.el --- fringe set up -*-coding:utf-8-*-

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

;; Setting up a wider fringe and icons

;;; Code:

(if window-system
    (progn
      (fringe-mode 16)

      (define-fringe-bitmap 'left-curly-arrow
        [#b0000000111111100
         #b0000001111111100
         #b0000011111100000
         #b0000111110000000
         #b0001111000000000
         #b0011110000000000
         #b0011110000000100
         #b0011111000001100
         #b0011111110011100
         #b0001111111111100
         #b0000111111111100
         #b0000001111111100
         #b0000000011111100
         #b0000000111111100
         #b0000001111111100
         #b0000011111111100] 16 16)

      (define-fringe-bitmap 'right-curly-arrow
        [#b0011111110000000
         #b0011111111000000
         #b0000011111100000
         #b0000000111110000
         #b0000000001111000
         #b0000000000111100
         #b0010000000111100
         #b0011000001111100
         #b0011100111111100
         #b0011111111111000
         #b0011111111110000
         #b0011111111000000
         #b0011111100000000
         #b0011111110000000
         #b0011111111000000
         #b0011111111100000] 16 16)))

;;; 00-fringe.el ends here
