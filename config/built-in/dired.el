;;; dired.el --- dired set up -*-coding:utf-8-*-

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

;; Set up dired.

;;; Code:

;; Confirm recursive deletes and copies once (at top) and Do What I Mean for targets
(setq dired-recursive-deletes 'top
      dired-recursive-copies 'top
      dired-dwim-target t)


;; Extra stuff, e.g. omitting files and dired-jump (C-x C-j)
;; See https://www.emacswiki.org/emacs/DiredExtra
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))


;; Renaming files directly in dired
;; See https://www.emacswiki.org/emacs/WDired
(require 'wdired)


;; Bind C-PgUp to go up a directory level (like mc and other file managers)
(define-key dired-mode-map (kbd "C-<prior>") 'dired-up-directory)


;; Just press "r" to rename files (using wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;; dired.el ends here
