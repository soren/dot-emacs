;;; typescript.el --- typescript mode -*-coding:utf-8-*-

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

;; Mode and more for writing TypeScript code.

;; See https://github.com/emacs-typescript/typescript.el
;; andhttps://github.com/ananthakumaran/tide/

;;; Code:

(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

(if (locate-file "node" exec-path exec-suffixes 'file-executable-p)
    (progn
      (unless (package-installed-p 'tide)
        (package-install 'tide))

      (defun setup-tide-mode ()
        (interactive)
        (tide-setup)
        (when (featurep 'flycheck)          
          (flycheck-mode +1)
          (setq flycheck-check-syntax-automatically '(save mode-enabled)))
        (eldoc-mode +1)
        (tide-hl-identifier-mode +1)
        (when (featurep 'company)
          (company-mode +1)))

      ;; formats the buffer before saving
      (add-hook 'before-save-hook 'tide-format-before-save)
      (add-hook 'typescript-mode-hook #'setup-tide-mode)))

;;; typescript.el ends here
