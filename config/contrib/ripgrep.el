;;; ripgrep.el --- ripgrep set up -*-coding:utf-8-*-

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

;; Use external program `rg` to search in files.

;; See https://github.com/BurntSushi/ripgrep

;;; Code:

(let ((rg-executable (locate-file "rg" exec-path exec-suffixes 1)))
  (if rg-executable
      (progn
        
        (setq ripgrep-command rg-executable)

        (defvar ripgrep-history nil
          "History for the `rg' command.")

        (defun ripgrep (command-args)
          (interactive
           (let ((ripgrep-command (concat ripgrep-command " --no-heading --with-filename ")))
             (list (read-shell-command "Run ripgrep (like this): "
                                       ripgrep-command
                                       'ripgrap-history))))
          
          (let ((compilation-disable-input t))
            (compilation-start (concat command-args " < /dev/null") ;; shouldn't use null-device
                               'grep-mode)))
        
        (global-set-key [f7] 'ripgrep)))
  (message "[dot-emacs] Cannot find rp executable, skipping ripgrep install"))


;;; ripgrep.el ends here
