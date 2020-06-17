(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutils-available-p))))
       (protocol (if no-ssl "http" "https")))
  (setq package-archives
	'(("melpa stable" . (concat protocol "://stable.melpa.org/packages"))
	  ("melpa" . (concat protocol "://melpa.org/packages"))
	  ("elpa" . (concat protocol "://elpa.gnu.org/packages")))
	package-archive-priorities
	'(("melpa stable" . 100)
	  ("melpa" . 50)
	  ("elpa" . 25))))

(package-initialize)

;; hide menubars. toolbars, and scrollbar

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; start Emacs with maximised screen

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; no backups allowed

(setq make-backup-files nil)


	       
