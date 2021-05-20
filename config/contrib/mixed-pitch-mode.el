;;; mixed-pitch-mode.el --- mixed pitch set up -*-coding:utf-8-*-

;; Copyright (c) 2021 Søren Lund <soren@lund.org>

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

;; See https://gitlab.com/jabranham/mixed-pitch

;;; Code:

(unless (package-installed-p 'mixed-pitch)
  (package-install 'mixed-pitch))

(add-hook 'org-mode-hook 'mixed-pitch-mode)

;;; mixed-pitch.el ends here
