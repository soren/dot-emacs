;;; diff-hl.el --- highlight uncommitted changes -*-coding:utf-8-*-

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

;; Highlights uncommitted changes on the left side of the window,
;; allows you to jump between and revert them selectively.

;;; Code:

(if (slu-dot-emacs-clone-and-add
     "https://github.com/dgutov/diff-hl")
    (progn
      (require 'diff-hl)
      (global-diff-hl-mode)
      (if (featurep 'psvn)
          (advice-add 'svn-status-update-modeline :after #'diff-hl-update)
      (if (featurep 'magit-mode)
          (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))))

;;; diff-hl.el ends here
