;;; init.el --- My Emacs configuration -*- lexical-binding: t -*-

;; Author: Timmy Jose <zoltan.jose@gmail.com>
;; Maintainer: Timmy Jose <zoltan.jose@gmail.com>
;; Version: 1.0
;; Homepage: https://github.com/timmyjose-config/.emacs.d
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;;   Configuring Emacs from scratch.  The idea is to keep vanilla as much as possible.

(require 'package)

;;; Code:

(setq package-archives
      '(("melpa stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa" . 100)
	("melpa stable" . 50)
        ("elpa" . 25)))


;;; setup the required packages
;;; and install them if not found
(defvar packages-list
      '(company
        exec-path-from-shell
        flycheck
        flycheck-package
        haskell-mode
        helm
        helm-ag
        helm-company
        helm-projectile
        lox-mode
        magit
        markdown-mode
        package-lint
	paredit
        projectile
        rust-mode
	slime
        zig-mode))

(package-initialize)

;; only used during initial setup

(unless package-archive-contents
  (package-refresh-contents))

;; this will check whether a package list refresh
;; is required or not, and then install the package
;; if not already installed.

(let ((packages-refreshed nil))
  (dolist (package packages-list)
    (unless (package-installed-p package)
      (unless packages-refreshed
        (package-refresh-contents)
        (setq packages-refreshed t))
      (package-install package))))

;; flycheck-package config
(eval-after-load 'flycheck
  '(flycheck-package-setup))


;;; Common Setup

;; load up cl.el
(eval-when-compile (require 'cl-lib))

;; hide menubars. toolbars, and scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; start emacs with maximised screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start splits vertically
(setq
 split-width-threshold 0
 split-height-threshold nil)

;; use windmove everywhere

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun ignore-windmove-error (fn)
  "Mute errors when no other window exists while invoking FN."
  (lambda ()
    (interactive)
    (ignore-errors
      (funcall fn))))

(global-set-key [s-left] (ignore-windmove-error 'windmove-left))
(global-set-key [s-right] (ignore-windmove-error 'windmove-right))
(global-set-key [s-up] (ignore-windmove-error 'windmove-up))
(global-set-key [s-down] (ignore-windmove-error 'windmove-down))

;; set preferred font

(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14"))

;; no backups allowed
(setq make-backup-files nil)

;; deal with PATH hell on macOS in a sane way
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; auto-focus on help window
(setq help-window-select t)


;;; Language-specific Setup

;; SBCL setup
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")


;;; Global Hooks

;; enable helm-mode and bind M-x to helm-M-x
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
; fix annoying quirky TAB behaviour
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; use company-mode everywhere
(add-hook 'after-init-hook 'global-company-mode)

;; enable projectile mode
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(define-key projectile-mode-map (kbd "M-t") 'helm-projectile)

;; use helm with Silver Searcher
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lox-bin "clox")
 '(package-selected-packages
   (quote
    (lox-mode package-lint magit markdown-mode markdown helm-ag helm-projectile projectile projectile-mode helm-company helm company company-mode exec-path-from-shell slime))))

(global-set-key (kbd "M-g") 'helm-ag)

;; use electric-pair mode everywhere (unless overridden
;; by specific modes)
(electric-pair-mode 1)

;; highlight matching parens
(show-paren-mode 1)

;; enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)


;;; Language-specific Hooks

;; practically every language major mode extends prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (electric-pair-mode 1)))

;; elisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'pp-eval-last-sexp)))

;; Rust
(add-hook 'rust-mode-hook
          (lambda ()
            (setq rust-format-on-save t)
            (define-key rust-mode-map (kbd "C-c C-c") 'rust-compile)
            (define-key rust-mode-map (kbd "C-c C-r") 'rust-run)
            (define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
            (define-key rust-mode-map (kbd "C-c C-k") 'rust-run-clippy)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
