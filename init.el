(require 'package)

(setq package-archives
      '(("melpa stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa stable" . 100)
	("melpa" . 50)))

;;; setup the required packages
;;; and install them if not found
(setq packages-list
      '(slime
	exec-path-from-shell))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package packages-list)
  (unless (package-installed-p package)
    (package-install package)))

;; hide menubars. toolbars, and scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; start Emacs with maximised screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start splits vertically
(setq
 split-width-threshold 0
 split-height-threshold nil)

;; no backups allowed
(setq make-backup-files nil)

;; deal with PATH hell on macOS in a sane way
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; SBCL setup
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (exec-path-from-shell slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
