;;; color-theme.el --- set up color theme -*-coding:utf-8-*-

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

;; Set up dark solarized color theme.

;;; Code:

(if window-system
    (let ((git-url "https://github.com/sellout/emacs-color-theme-solarized"))      
      (slu-dot-emacs-clone git-url)
      (add-to-list 'custom-theme-load-path
                   (concat slu-dot-emacs-lisp-dir (file-name-nondirectory git-url)))
      (setq frame-background-mode 'dark)
      (set-frame-parameter nil 'background-mode 'dark)
      (load-theme 'solarized t)))

;;; color-theme.el ends here
