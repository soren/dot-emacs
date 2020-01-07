;;; ispell.el --- spell check set up -*-coding:utf-8-*-

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

;; Set up spell checking. I'm using aspell, except on Windows, where
;; I'm using hunspell from
;; http://sourceforge.net/projects/ezwinports/files

;;; Code:

;; Set default dictionary
(ispell-change-dictionary my-ispell-change-dictionary)

;; Use hunspell on Windows
(if (eq system-type 'windows-nt)
    (let ((hunspell-root "C:/Opt/hunspell"))
      (add-to-list 'exec-path (concat hunspell-root "/bin"))
      (setq ispell-program-name (locate-file "hunspell"
                                             exec-path exec-suffixes 'file-executable-p))
      (setq ispell-local-dictionary-alist '(                                             
                                            (nil
                                             "[[:alpha:]]"
                                             "[^[:alpha:]]"
                                             "[']"
                                             t
                                             ("-d" "en_US" "-p" "C:\\Opt\\hunspell\\share\\hunspell\\personal.en")
                                             nil
                                             utf-8)                                      
                                            ("american"
                                             "[[:alpha:]]"
                                             "[^[:alpha:]]"
                                             "[']"
                                             t
                                             ("-d" "en_US" "-p" "C:\\Opt\\hunspell\\share\\hunspell\\personal.en")
                                             nil
                                             utf-8)
                                            ("british"
                                             "[[:alpha:]]"
                                             "[^[:alpha:]]"
                                             "[']"
                                             t
                                             ("-d" "en_GB" "-p" "C:\\Opt\\hunspell\\share\\hunspell\\personal.gb")
                                             nil
                                             utf-8)
                                            )))
  (setq ispell-program-name "aspell"))

(require 'ispell)

;; Skip pars of org-mode documents
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
(add-to-list 'ispell-skip-region-alist '("#\\+\\(OPTIONS\\|INFOJS_OPT\\|HTML_HEAD\\):" . "$"))

;;; ispell.el ends here
