;;; calendar.el --- calendar set up -*-coding:utf-8-*-

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

;; Set up calendar.

;; Inspiration from
;; http://www.emacswiki.org/emacs/CalendarLocalization

;;; Code:

(require 'calendar)

(setq calendar-latitude my-calendar-latitude)
(setq calendar-longitude my-calendar-longitude)
(setq calendar-location-name my-calendar-location-name)

(setq mark-diary-entries-in-calendar t)
(setq mark-holidays-in-calendar t)

(calendar-set-date-style 'iso)

(if (not (file-exists-p my-diary-file))
    (write-region "" nil my-diary-file))
(setq diary-file my-diary-file)

(setq calendar-view-diary-initially-flag t)
(setq number-of-diary-entries 7)

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; From http://bigwalter.net/daniel/elisp/snippets.html

(defface diary-birthday-face
  '((t :inherit diary-face :weight bold))
  "Diary birthday face."
  :group 'diary)

(defun diary-birthday (month day &optional year)
  "Birthday diary entry with face `diary-birtday-face'.
Use %%(diary-birthday month day year) in your diary file."
  (diary-anniversary month day year 'diary-birthday-face))

;;; calendar.el ends here
