;;; rainbow-mode.el --- show color of color codes -*-coding:utf-8-*-

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

;; Show colors of color codes inline.

;;; Code:

(unless (package-installed-p 'rainbow-mode)
  (package-install 'rainbow-mode))

(add-to-list 'auto-mode-alist '("\\.html\\'" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . rainbow-mode))

;;; rainbow-mode.el ends here
