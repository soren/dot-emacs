;;; 01-yaml-mode.el --- yaml-mode set up -*-coding:utf-8-*-

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

;; Set up mode for editing YAML files.

;;; Code:

(if (slu-dot-emacs-clone-and-add
     "https://github.com/yoshiki/yaml-mode")
    (progn
      (require 'yaml-mode)
      (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))))

;;; 01-yaml-mode.el ends here
