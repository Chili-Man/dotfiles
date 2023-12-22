;; Packages to install:
;;  zenburn-theme
;;  monokai-theme
;;  hcl-mode
;;  go-mode
;;  groovy-mode
;;  flycheck, go mode
;;  yaml-mode
;;  dockerfile-mode
;;  docker-compose-mode
;;  yafolding
;;  origami
;;  company
;;  tabnine - see https://github.com/TommyX12/company-tabnine
;;  tabbar
;;  markdown-mode
;;  web-mode


(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

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
    (terraform-mode flycheck-golangci-lint flycheck gitignore-mode typescript-mode markdown-mode nixpkgs-fmt nix-mode tabbar company-terraform company origami yafolding dockerfile-mode yaml-mode groovy-mode go-mode hcl-mode monokai-theme)))
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

;; Autocompleting
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)
;; (add-hook 'after-init-hook 'global-company-mode) ;; Enable company mode in all buffers
;; (setq company-idle-delay 0) ;; Trigger completion immediately.
;; (setq company-show-numbers t) ;; Number the candidates (use M-1, M-2 etc to select completions).

;; Auto refresh updated files from disk
(global-auto-revert-mode t)

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
;; (company-tng-configure-default)
;; (setq company-frontends
;;       '(company-tng-frontend
;;         company-pseudo-tooltip-frontend
;;         company-echo-metadata-frontend))

;; Autocomplete for terraform
(require 'company-terraform)
(company-terraform-init)

;; For Nix auto formatting
(add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Syntax highlight mode for Helm chart templates (and go templates)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("go"    . "\\.tpl\\'")))

;; For Nix auto formatting
(add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

