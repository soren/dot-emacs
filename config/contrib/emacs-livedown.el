;;; emacs-livedown.el --- live preview of markdown -*-coding:utf-8-*-

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

;; Display live preview of rendered markdown in browser.

;; See https://github.com/shime/emacs-livedown

;;; Code:

(when (package-installed-p 'markdown-mode)
  (if (and
       (locate-file "livedown" exec-path exec-suffixes 'file-executable-p)
       (slu-dot-emacs-clone-and-add "https://github.com/shime/emacs-livedown"))
      (progn
        (require 'livedown)
        (setq livedown-browser my-alternate-browser)
        (global-set-key (kbd "C-M-m") 'livedown-preview))))

;;; emacs-livedown.el ends here
