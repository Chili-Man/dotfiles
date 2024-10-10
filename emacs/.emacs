;;; Commentary:  Packages to install:
;;  monokai-theme
;;  hcl-mode
;;  go-mode
;;  groovy-mode
;;  flycheck, go mode
;;  flycheck-golangci-lint
;;  flycheck-pycheckers
;;  yaml-mode
;;  git-modes
;;  dockerfile-mode
;;  docker-compose-mode
;;  groovy-mode
;;  nix-mode
;;  tabbar
;;  markdown-mode
;;  web-mode

;;;;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (json-mode python-mode terraform-mode flycheck-golangci-lint flycheck git-modes typescript-mode markdown-mode nixpkgs-fmt nix-mode tabbar dockerfile-mode yaml-mode groovy-mode go-mode hcl-mode monokai-theme)))
 '(show-paren-mode t)
 '(size-indication-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 128 :width normal)))))

;;;; Customizations
;; Emacs Indentation
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(defun my-setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)

  ;; groovy
  (setq-local groovy-indent-offset n)

  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-default typescript-indent-level 2); typescript mode
  (setq-default sh-basic-offset 2); shell mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  )
(defun my-personal-code-style ()
  (interactive)
  ;; use space instead of tab
  (setq indent-tabs-mode nil)
  ;; indent 2 spaces width
  (my-setup-indent 2))
(add-hook 'prog-mode-hook 'my-personal-code-style)

;; Go format on save
(defun my-go-mode-hook ()
      (setq tab-width 2 indent-tabs-mode 1)
      ; eldoc shows the signature of the function at point in the status bar.
      (go-eldoc-setup)
      (local-set-key (kbd "M-.") #'godef-jump)
      (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'my-go-mode-hook)

; As-you-type error highlighting
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Load the Monokai Theme
(load-theme 'monokai t)

;; Highlight matching parenthesis
(show-paren-mode 1)

;; Truncate Long lines
(set-default 'truncate-lines t)

;; Display column number
(setq column-number-mode t)

;; Recognize .tf as HCL mode
(add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))

;; Recognize typescript
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

;; Enable Tab Bar mode
(require 'tabbar)
(tabbar-mode t)

;; Auto refresh updated files from disk
(global-auto-revert-mode t)

;; Add linue numbers on the side
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; For Nix auto formatting
(add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Syntax highlight mode for Helm chart templates (and go templates)
(require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (setq web-mode-engines-alist
;;      '(("go"    . "\\.tpl\\'")))

;; For Nix auto formatting
(add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Allow symlinks to be automatically followed
(setq vc-follow-symlinks nil)


;; Case insensitive line sort
(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;; Flycheck golangci-lint
(eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;; flycheck-pycheckers
(global-flycheck-mode 1)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
