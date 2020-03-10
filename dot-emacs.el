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

;; Set some variables used to configure modes etc. You would want to
;; override some of these.

(defvar my-calendar-latitude 55.675876
  "*Latitude of Copenhagen, used by e.g. M-x sunrise-sunset")
(defvar my-calendar-longitude 12.569066
  "*Longitude of Copenhagen, used by e.g. M-x sunrise-sunset")
(defvar my-calendar-location-name "København"
  "*Copenhagen in danish, used by e.g. M-x sunrise-sunset")

(defvar my-timeclock-file "~/Projects/My/timelogs/timelog"
  "*")
(defvar my-timeclock-workday 26640
  "*A workday is 7.4 hours, e.g. a 37 hours work week")

(defvar my-diary-file "~/diary"
  "*")
(defvar my-ispell-change-dictionary "dansk"
  "*")
(defvar my-alternate-browser "/usr/bin/chromium-browser"
  "*I'm using this for livedown")

(defvar slu-dot-emacs-dryrun nil
  "*Set dryrun if no files should be loaded.")

(defvar slu-dot-emacs-lisp-dir
  (expand-file-name (concat user-emacs-directory "slu-dot-emacs/"))
  "*Local lisp directory. Will download and/or look for packages
  and lisp files here.")

(defvar slu-dot-emacs-git-executable
  (executable-find "git")
  "*Path to Git executable.")


;; Setup the root-directory for the configuration files. It will be an
;; absolute path, found by finding the directory where this script
;; lives

(setq slu-dot-emacs-config-root-dir
      (concat (file-name-directory
               (if load-in-progress load-file-name buffer-file-name))
              "config"))

;; Setup subdirectories to load configurations from:

;; built-in - contains simple configuration. Standard Emacs stuff,
;;            i.e. no need to download/install anything.

;; contrib - contains contributed modes and packages.

(setq slu-dot-emacs-config-subdirs '("built-in" "contrib"))


(defun slu-dot-emacs-clone (git-url)
  "Clone git repository from GIT-URL"
  (if slu-dot-emacs-git-executable
      (let ((default-directory slu-dot-emacs-lisp-dir)
            (dir-name (file-name-nondirectory git-url)))
        (if (not (file-exists-p dir-name))
            (with-temp-buffer
              (let ((exit-code
                     (apply 'call-process
                            (append
                             (list slu-dot-emacs-git-executable nil t nil)
                             (list "--no-pager" "clone" git-url)))))
                (if (zerop exit-code)
                    (progn
                      (message "%s" (buffer-string))
                      dir-name)
                  (progn
                   (message "[dot-emacs] Unable to clone git repository: %s\n%s"
                            git-url
                            (buffer-string))
                   nil)))))
          (progn
            (message "[dot-emacs] Git repository already cloned, skipping")
            (concat slu-dot-emacs-lisp-dir dir-name)))
    (progn
      (message "[dot-emacs] No git, unable to clone")
      nil)))


(defun slu-dot-emacs-clone-and-add (git-url)
  "Clone git repository and add the cloned directory to
 load-path."
  (progn
    (slu-dot-emacs-clone git-url)
    (let ((full-path (concat slu-dot-emacs-lisp-dir (file-name-nondirectory git-url))))
      (if (file-exists-p full-path)
	  (add-to-list 'load-path (expand-file-name full-path))
        (message "[dot-emacs] Unable to install from %s" git-url)
        nil))))


(defun slu-dot-emacs-compile-lisp-files ()
  "Compiles the elisp files (.el files)"
  (interactive)
  (mapcar (lambda (x) (byte-recompile-directory x 0))
	  (list slu-dot-emacs-config-root-dir
		slu-dot-emacs-lisp-dir)))


(let ((start-time (current-time)))
  (message "[dot-emacs] dot-emacs %s starting to load configuration..." "1.0")

  (unless slu-dot-emacs-dryrun

    (unless (file-exists-p slu-dot-emacs-lisp-dir)
      (make-directory slu-dot-emacs-lisp-dir t))

    ;; Load all lisp and configuration files
    (while slu-dot-emacs-config-subdirs
      (setq files (directory-files (concat slu-dot-emacs-config-root-dir "/"
					   (car slu-dot-emacs-config-subdirs)) t "\\.el$"))
      (while files
        (load (car files))
        (setq files (cdr files)))
      (setq slu-dot-emacs-config-subdirs (cdr slu-dot-emacs-config-subdirs))))

  (message "[dot-emacs] Done in %.06f seconds" (float-time (time-since start-time))))

;;; dot-emacs.el ends here
