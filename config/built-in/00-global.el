;;; 00-global.el --- set global settings to my likings -*-coding:utf-8-*-

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

;; This file sets some global settings in Emacs, stuff like turning
;; off the menu, tool and scroll bars.

;; Inspiration from
;; http://www.emacswiki.org/cgi-bin/wiki/EmacsCrashCode
;; http://homepages.inf.ed.ac.uk/s0243221/emacs/index.html

;;; Code:

;; ---------------------------------------------------------------------
;; How to and what to display:

;; Don't display startup screen
(setq inhibit-startup-message t)

;; Set window title to <buffer name> - <full path> <user login name>@<system name>
(setq frame-title-format (concat  "%b - %f - " user-login-name "@" system-name))

;; Display time and date in mode line. Format is dayname date. month HH:MM
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-format "%A %e. %B %H:%M")
(display-time)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Turn off menu, toolbar and scrollbar
;;
;; Note: Setting the following in .Xresources would give a faster start-up:
;;
;; Emacs.menuBar: off
;; Emacs.verticalScrollBars: off
;; Emacs.toolBar: off
;;
;; Remember to run xrdb .Xresources
;;
;; On Windows the following regitry entries have the same effect:
;;
;; [HKEY_LOCAL_MACHINE\SOFTWARE\GNU\Emacs]
;; "Emacs.menuBar"="off"
;; "Emacs.toolBar"="off"
;; "Emacs.verticalScrollBars"="off"
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Show trailing whitespace - http://www.emacswiki.org/emacs/ShowWhiteSpace
;; and possibly enhance with http://bsdaemon.blogspot.com/2006/01/handling-eol-whitespace-in-emacs.html
(setq show-trailing-whitespace t)

;; In every buffer, the line which contains the cursor will be fully
;; highlighted.
(global-hl-line-mode 1)

;; Flash frame instead of beeping
(setq visible-bell t)


;; ---------------------------------------------------------------------
;; Change/set up some default behavior:

;; never open a new ediff window
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Enable narrow-to-region c-x n n
(put 'narrow-to-region 'disabled nil)

;; Alwaus split vertically
(setq split-width-threshold most-positive-fixnum)

;; Let me see a little more lines in the *Messages* buffer
(setq message-log-max 255)


;; ---------------------------------------------------------------------
;; Tabs:

;; Use tab to indent or complete - http://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
(setq tab-always-indent 'complete)

;; Turn off use of tabs for indentation in many modes
(setq-default indent-tabs-mode nil)

;; Most tools expetcs a tab to be equal to 8 spaces
(setq default-tab-width 8)


;; ---------------------------------------------------------------------
;; Fixing stuff:

;; Fix "Variable binding depth exceeds max-specpdl-size"
(setq max-specpdl-size 50000)

;; Fix "Lisp nesting exceeds ‘max-lisp-eval-depth"
(setq max-lisp-eval-depth 50000)


;; ---------------------------------------------------------------------
;; Enable global modes:

;; Use C-c <left> and C-c <right> to undo and re-do window changes.
(winner-mode 1)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)
(show-paren-mode t)


;; ---------------------------------------------------------------------
;; Backup:

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backup")) ; all bu in one dir
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups


;; ---------------------------------------------------------------------
;; Protect the *scratch* buffer from being killed:

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;; ---------------------------------------------------------------------
;; Hooks:

;; Enable automatic time stamping
(add-hook 'before-save-hook 'time-stamp)


;; ---------------------------------------------------------------------
;; Configuration of external tools:

;; default to unified diffs
(setq diff-switches "-u")

;; Use unzip instead of pkunzip
(setq archive-zip-use-pkzip nil)


;; ---------------------------------------------------------------------
;; Global keys:

;; Inspired by http://steve.yegge.googlepages.com/effective-emacs
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;;; 00-global.el ends here
