;;; magit.el --- magit set up -*-coding:utf-8-*-

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

;; Set up magit. Git needs to be installed and in the PATH.

;;; Code:

(if (locate-file "git" exec-path exec-suffixes 'file-executable-p)
    (progn
      (unless (package-installed-p 'magit)
        (package-install 'magit))

      (require 'magit)

      (if window-system
          ;; Redefine bitmaps, as I'm using a 16 bit wide fringe
          (progn
            (define-fringe-bitmap 'magit-fringe-bitmap>
              [#b0000000000000000
               #b0011111000000000
               #b0001111100000000
               #b0000111110000000
               #b0000011111000000
               #b0000001111100000
               #b0000000111110000
               #b0000000011111000
               #b0000000001111100
               #b0000000011111000
               #b0000000111110000
               #b0000001111100000
               #b0000011111000000
               #b0000111110000000
               #b0001111100000000
               #b0011111000000000] 16 16)

            (define-fringe-bitmap 'magit-fringe-bitmapv
              [#b0000000000000000
               #b0000000000000000
               #b0000000000000000
               #b0000000000000000
               #b1000000000000001
               #b1100000000000011
               #b1110000000000111
               #b1111000000001111
               #b1111100000011111
               #b0111110000111110
               #b0011111001111100
               #b0001111111111000
               #b0000111111110000
               #b0000011111100000
               #b0000001111000000
               #b0000000110000000] 16 16))))

  (message "[dot-emacs] Cannot find git executable, skipping magit install"))

;;; magit.el ends here
