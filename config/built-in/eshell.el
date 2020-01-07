;;; eshell.el --- eshell set up -*-coding:utf-8-*-

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

;; Setting up the eshell

;; Inspiration from
;; http://www.emacswiki.org/alex/2008-08-19_Emacs_on_Windows

;;; Code:

(global-set-key (kbd "C-z") 'eshell)
(setq eshell-save-history-on-exit t)
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-z") 'bury-buffer)
	    (local-set-key (kbd "C-a") 'eshell-bol)
	    (local-set-key (kbd "<up>") 'previous-line)
	    (local-set-key (kbd "<down>") 'next-line)))
(defalias 'eshell/emacs 'find-file)
(defun eshell/dired () (dired (eshell/pwd)))

;; From https://gist.github.com/ralt/a36288cd748ce185b26237e6b85b27bb

(require 'em-term)
(add-to-list 'eshell-visual-commands "htop")

(setq eshell-destroy-buffer-when-process-dies t)

(setq eshell-history-size 1000000)

;; Tramp...

;; cache file-name forever
(setq remote-file-name-inhibit-cache nil)

;; make sure vc stuff is not making tramp slower
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))

;; not sure why we have this? just cargo-culting from an answer I saw
;; online.
(setq tramp-verbose 1)

;; projectile has the fun side-effect of wanting to calculate the
;; project name, which makes tramp oh-so-much-slower.
(setq projectile-mode-line "Projectile")

;;; eshell.el ends here
