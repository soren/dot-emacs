;;; sh-mode.el --- shell mode set up -*-coding:utf-8-*-

;; Copyright (c) 2020 Søren Lund <soren@lund.org>

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

;; Set up shellcheck integration using flycheck if both are installed.

;; See https://www.shellcheck.net/

;;; Code:

(when (and (featurep 'flycheck)
           (locate-file "shellcheck" exec-path exec-suffixes 'file-executable-p))
  (add-hook 'sh-mode-hook 'flycheck-mode))

;;; sh-mode.el ends here
