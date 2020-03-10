;;; python-mode.el --- python mode set up -*-coding:utf-8-*-

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

;; Set up python-mode with auto-complete and linting.

;; See https://www.pylint.org/
;; and https://virtualenv.pypa.io/en/stable/

;;; Code:

;; Set up pylint integration using flycheck if both are installed
(when (and (featurep 'flycheck)
           (locate-file "pylint" exec-path exec-suffixes 'file-executable-p))
  (add-hook 'python-mode-hook 'flycheck-mode))

;; Set up jedi integration using company. This requires virtualenv to be installed
(when (and (featurep 'company)
           (locate-file "virtualenv" exec-path exec-suffixes 'file-executable-p))
  (unless (package-installed-p 'company-jedi)
    (package-install 'company-jedi))
  (jedi:install-server)
  (add-to-list 'company-backends 'company-jedi))

;;; python-mode.el ends here
