;;; todo.el --- font-lock of todo comments -*-coding:utf-8-*-

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

;; Set up simple font-lock hightlighting of TODO comments.

;; FIXME This is a of FIXME.
;; FIXME: Please!

;; TODO: This is a test of TODO
;; TODO Without the colon
;; ToDo: Another test
;; todo: Wouldn't do it like this
;; @todo JavaDoc style

;; XXX Another task tag.
;; XXX: With the colon.
;; xxx is lower case a good idea?

;;; Code:

;; http://c2.com/cgi/wiki?FixmeComment

(defface slu-dot-emacs-fixme-face
  '((t :foreground "White" :background "Red"))
  "FIXME face"
  :group 'slu-dot-emacs-todo-comments)

(defface slu-dot-emacs-todo-face
  '((t :foreground "Black" :background "Yellow"))
  "TODO face" :group 'slu-dot-emacs-todo-comments)

(defface slu-dot-emacs-xxx-face
  '((t :foreground "White" :background "Blue"))
  "XXX face" :group 'slu-dot-emacs-todo-comments)

(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\<\\(FIXME\\|FixMe\\|fixme\\):? " 1 'slu-dot-emacs-fixme-face t)
      ("\\<\\(\\(TODO\\|ToDo\\|todo\\):?\\|@todo\\) " 1 'slu-dot-emacs-todo-face t)
      ("\\<\\(XXX\\|xxx\\):? " 1 'slu-dot-emacs-xxx-face t)))))

;;; todo.el ends here
