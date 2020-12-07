;;; timeclock.el --- configuration of timeclock

(setq timeclock-file my-timeclock-file)
(setq timeclock-workday my-timeclock-workday)

(setq timeclock-script-name
      (if (eq system-type 'windows-nt)
          "C:/Opt/Git/usr/bin/site_perl/timeclock.pl"
        "/usr/local/bin/timeclock.pl"))

(setq timeclock-perl-cmd
      (if (eq system-type 'windows-nt)
          "C:/Opt/Git/usr/bin/perl.exe"
        "/usr/bin/perl"))

(when (and
       (file-exists-p timeclock-script-name)                     
       (file-exists-p timeclock-perl-cmd))
  (defun timeclock-show-daily-report()
    "Creates and displays a daily report of timeclock entries."
    (interactive)
    (let ((process-connection-type nil)   ; Use a pipe.
          (command-name "timeclock")
          (buffer-name "*timeclock daily report*"))
      (when (get-buffer buffer-name)
        (progn
          (set-buffer buffer-name)
          (set-buffer-modified-p nil)
          (erase-buffer)))
      (set-buffer (get-buffer-create buffer-name))
      (start-process command-name buffer-name timeclock-perl-cmd timeclock-script-name (expand-file-name timeclock-file))
      (switch-to-buffer buffer-name))))
