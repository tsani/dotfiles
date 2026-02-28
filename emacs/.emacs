;;;;; PACKAGE MANAGEMENT ;;;;;

(require 'package)


(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package evil
  :init

  ;; so that C-u will scroll up, as in Vim
  (setq evil-want-C-u-scroll t)
  :ensure t)

(use-package highlight-parentheses
  :config
  (setq hl-paren-colors '("black"))
  (setq hl-paren-background-colors '("red" "orange" "yellow" "green" "blue" "violet"))
  (setq hl-paren-delay 0.05)
  :ensure t)

;;;;; LOADING PACKAGES ;;;;;

; (add-hook 'csharp-mode-hook 'omnisharp-mode)
; (add-hook 'csharp-mode-hook #'flycheck-mode)
(add-hook 'agda2-mode-hook
          (lambda ()
            (evil-define-key 'normal agda2-mode-map
              "gd" 'agda2-goto-definition-keyboard)))

(evil-mode t)

(define-key evil-normal-state-map (kbd ", w") 'save-buffer)
(define-key evil-normal-state-map (kbd ", q") 'evil-quit)

(add-hook 'text-mode-hook
	  '(lambda ()
	     (set-fill-column 100)
	     (turn-on-auto-fill)))

(add-to-list 'load-path "/Users/jerrin/.emacs.d/lisp")

(setq focus-follows-mouse t)
;; ^ so emacs will warp the mouse when using a frame-select command

; make insert-mode C-u delete to beginning of line
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

(setq-default
  indent-tabs-mode nil
  tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style "linux")

(setq-default require-final-newline t)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;;;; AUTO-SCROLLING ;;;;;

; Do not center point in window when scrolling.
(setq scroll-conservatively 1)
; Leave a margin of 7 lines when scrolling.
(setq scroll-margin 5)

;;;;; INTEGRATIONS ;;;;;

(setq vc-follow-symlinks t)
(setq x-select-enable-primary t)

;;;;; VISUALS ;;;;;

(setq show-paren-delay 0)
(show-paren-mode 1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; more crap to disable scrollbars
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;;;;; AUTO-SAVING AND BACKUPS ;;;;;

(setq create-lockfiles nil) ;; unnecessary bc this is my own computer

;;;;; HACKS ;;;;;

(define-key evil-normal-state-map (kbd "C-p") 'universal-argument)

;; make frame opened by `ee` focused.
(setq x-focus-frame nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(highlight-parentheses evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
