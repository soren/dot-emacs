;;; 01-frames.el --- set up frames -*-coding:utf-8-*-

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

;; Setting up frame dimensions and fonts.

;;; Code:

(defun reset-frame-size-and-position()
  "Resets the currect frame to my defaults."
  (interactive)
  (progn
    (set-default-frame-size-and-position)
    (apply-frame-size-and-position)))

(defun get-screen-size ()
  "Returns the screens size as a list of widht and heigth"
  (if (eq system-type 'windows-nt)
      (get-win32-screen-size)
    (get-x11-screen-size)))

(defun get-win32-screen-size ()
  "Returns the screen size on a Windows system."
  (list (x-display-pixel-width) (x-display-pixel-height)))

(defun get-x11-screen-size ()
  "Returns the screen size on a X11 system."
  (let (cmd-name sh-output widht height)
    (setq cmd-name "xdpyinfo")
    (setq sh-output (shell-command-to-string cmd-name))
    ;;   dimensions:    1024x768 pixels (271x203 millimeters)
    (string-match "dimensions: +\\([0-9]+\\)x\\([0-9]+\\)" sh-output)
    (setq width (match-string 1 sh-output))
    (setq height (match-string 2 sh-output))
    (list (string-to-number width) (string-to-number height))))

(defun set-default-frame-size-and-position()
  "Set default size (widht, height), top left corner and font of frames."
  (interactive)
  (progn
    (setq initial-frame-alist
	  '((top . 16) (left . 80)
	    (width . 130) (height . 50)
	    (cursor-type . box)
            (font . "-outline-DejaVu Sans Mono-normal-normal-normal-mono-20-*-*-*-c-*-iso8859-1")))))

(defun reset-frame-size-and-position-huge()
  "Resets the currect frame to much larger defaults."
  (interactive)
  (progn
    (setq initial-frame-alist
          '((top . 14) (left . 80)
            (width . 120) (height . 38)
            (cursor-type . box)
            (font . "-outline-DejaVu Sans Mono-normal-normal-normal-mono-36-*-*-*-c-*-iso8859-1")))
    (apply-frame-size-and-position)))

(defun reset-frame-size-and-position-large()
  "Resets the currect frame to larger defaults."
  (interactive)
  (progn
    (setq initial-frame-alist
          '((top . 14) (left . 80)
            (width . 120) (height . 38)
            (cursor-type . box)
            (font . "-outline-DejaVu Sans Mono-normal-normal-normal-mono-24-*-*-*-c-*-iso8859-1")))
    (apply-frame-size-and-position)))

(defun reset-frame-size-and-position-small()
  "Resets the currect frame to my smaller defaults."
  (interactive)
  (progn
    (setq initial-frame-alist
          '((top . 16) (left . 64)
            (width . 150) (height . 58)
            (cursor-type . box)
            (font . "-outline-DejaVu Sans Mono-normal-normal-normal-mono-18-*-*-*-c-*-iso8859-1")))
    (apply-frame-size-and-position)))

(defun apply-frame-size-and-position()
  "Applies the frame settings in initial-frame-alist which is also copied to defaul-frame-alist. This is done for all open frames."
  (progn
    (setq default-frame-alist (append (copy-alist initial-frame-alist)
                                      '((foreground-color . "white")
                                        (background-color . "black")
                                        (cursor-color . "yellow"))))
    (mapcar (lambda (x)
	      (select-frame x)
	      (set-frame-font (cdr (assoc 'font initial-frame-alist)))
	      (set-frame-height x (cdr (assoc 'height initial-frame-alist)))
	      (set-frame-width x (cdr (assoc 'width initial-frame-alist)))
	      (set-frame-position x
				  (cdr (assoc 'left initial-frame-alist))
				  (cdr (assoc 'top initial-frame-alist))))
	    (frame-list))))

(defun persist-display-settings()
  "For faster (and less flickering) startup duplicate the settings in the registry or .Xdefaults"
  (if (eq system-type 'windows-nt)
      (progn
	(call-process "reg" nil nil nil
		      "add" "HKEY_LOCAL_MACHINE\\SOFTWARE\\GNU\\Emacs" "/f"
		      "/v" "Emacs.font" "/t" "REG_SZ"
		      "/d" (cdr (assoc 'font initial-frame-alist)))

	(call-process "reg" nil nil nil
		      "add" "HKEY_LOCAL_MACHINE\\SOFTWARE\\GNU\\Emacs" "/f"
		      "/v" "Emacs.geometry" "/t" "REG_SZ"
		      "/d" (format "%dx%d+%d+%d"
				   (cdr (assoc 'width initial-frame-alist))
				   (cdr (assoc 'height initial-frame-alist))
				   (cdr (assoc 'left initial-frame-alist))
				   (cdr (assoc 'top initial-frame-alist)))))
    (if window-system ; asuming this will be some variant of X11
        (progn
          (message "[dot-emacs] You should have the following in your .Xresources file:")
          (message (concat "Emacs.font:	" (cdr (assoc 'font initial-frame-alist))))
          (message (concat "Emacs.geometry:	" (format "%dx%d+%d+%d"
                                                          (cdr (assoc 'width initial-frame-alist))
                                                          (cdr (assoc 'height initial-frame-alist))
                                                          (cdr (assoc 'left initial-frame-alist))
                                                          (cdr (assoc 'top initial-frame-alist)))))))))

;; If on a window system (i.e. X11 or Windows), then set default font
;; settings (for this and future frames)
(if window-system
    (progn
      (reset-frame-size-and-position-huge)
      (persist-display-settings)))

;;; 00-frames.el ends here
