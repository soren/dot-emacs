;; dot-emacs.el --- master dot-emacs initialization file

;; Copyright (c) 2006-2019 Søren Lund <soren@lund.org>

;; Author: Søren Lund <soren@lund.org>
;; Created: 1 Nov 2006
;; Version: 0.1

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

;; This actually replaces most of my GNU/Emacs configuration, i.e. the
;; infamous .emacs file.


;;; Code:

(defvar slu-dot-emacs-dryrun nil
  "*Set dryrun if no files should be loaded.")

;; Find the root-directory, i.e. the absolute path to the directory
;; where this script lives

(setq slu-dot-emacs-root-dir
      (file-name-directory
       (if load-in-progress load-file-name buffer-file-name)))

;; Setup the root-directory for the configuration files

(setq slu-dot-emacs-config-root-dir
      (concat slu-dot-emacs-root-dir "config"))

;; Setup subdirectories to load configurations from.
;; built-in - contains simple configuration. Standard Emacs stuff, i.e. no need to download/install anything.
;; contrib - contains contributed modes and packages.

(setq slu-dot-emacs-config-subdirs '("built-in" "contrib"))

;; Setup the root-directory for the lisp files

(setq slu-dot-emacs-lisp-root-dir (concat slu-dot-emacs-root-dir "my-lisp"))

;; Setup my local lisp directory

(defvar slu-dot-emacs-my-lisp-dir (expand-file-name "~/.emacs.d/lisp/")
  "*Local lisp directory. Will look for packages and lisp files here.")

;; This macro is used to "load" libraries, it will check whether the
;; library is present (i.e. found in load-path). If found
;; initialization code is run, else an error message containing
;; download URL is displayed.

(defmacro slu-dot-emacs-for-lib (lib url init)
  `(if (locate-library (format "%s" ,lib))
       ,init
     (message "[dot-emacs] Cannot find %s - download it from %s" ,lib ,url)))

(defmacro slu-dot-emacs-load-file (file url init)
  `(let ((full-path (concat slu-dot-emacs-my-lisp-dir ,file)))
     (if (file-exists-p full-path)
	 (progn
	   (load-file full-path)
	   ,init)
     (message "[dot-emacs] Cannot find %s - download it from %s" ,file ,url))))

(defun slu-dot-emacs-add-to-load-path (dir url)
  "If given dir exist, then add it to load-path, else print message with download URL."
  (let ((full-path (concat slu-dot-emacs-my-lisp-dir dir)))
    (if (file-exists-p full-path)
	(add-to-list 'load-path (expand-file-name full-path))
      (message "[dot-emacs] Cannot find %s - download it from %s" dir url)
      nil)))

(defun slu-dot-emacs-compile-lisp-files ()
  "Compiles the elisp files (.el files)"
  (interactive)
  (mapcar (lambda (x) (byte-recompile-directory x 0))
	  (list slu-dot-emacs-config-root-dir
		slu-dot-emacs-lisp-root-dir
		slu-dot-emacs-my-lisp-dir)))

(let ((start-time (current-time)))
  (message "[dot-emacs] dot-emacs %s starting to load configuration..." "1.0")

  (unless slu-dot-emacs-dryrun
    ;; Set some variables used to configure modes etc.
    (setq my-calendar-latitude 55.675876)
    (setq my-calendar-longitude 12.569066)
    (setq my-calendar-location-name "København")
    (setq my-timeclock-file "~/Projects/My/timelogs/timelog")
    (setq my-timeclock-workday 26640) ; 7.4 hour/day, e.g. 37 hours work week
    (setq my-diary-file "~/diary")
    (setq my-ispell-change-dictionary "dansk")

    ;; Load all lisp and configuration files
    (while slu-dot-emacs-config-subdirs
      (setq files (directory-files (concat slu-dot-emacs-config-root-dir "/"
					   (car slu-dot-emacs-config-subdirs)) t "\\.el$"))
      (while files
        (load (car files))
        (setq files (cdr files)))
      (setq slu-dot-emacs-config-subdirs (cdr slu-dot-emacs-config-subdirs)))

    ;; Load my own lisp code
    (setq files (directory-files slu-dot-emacs-lisp-root-dir t "\\.el$"))
    (while files
      (load (car files))
      (setq files (cdr files))))

  (message "[dot-emacs] Done in %.06f seconds" (float-time (time-since start-time))))

;;; dot-emacs.el ends here
