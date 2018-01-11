;; Add MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

;; Packages to install:
;;  zenburn-theme
;;  monokai-theme
;;  hcl-mode
;;  go-mode
;;  groovy-mode
;;  yaml-mode
;;  dockerfile-mode
;;  docker-compose-mode
;;  yafolding
;;  origami

;; For Go mode
;; go mode
;; (add-to-list 'load-path "~/.emacs.d/plugins/go-mode.el")
;;(require 'go-mode-autoloads)

;;(add-hook 'go-mode-hook
;;         (lambda ()
;;          (add-hook 'before-save-hook 'gofmt-before-save)
;;          (setq tab-width 4)
;;          (setq indent-tabs-mode 1)))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (yafolding tabbar markdown-mode docker-compose-mode dockerfile-mode monokai-theme solarized-theme zenburn-theme hcl-mode go-mode groovy-mode yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier 10 Pitch" :foundry "bitstream" :slant normal :weight normal :height 98 :width normal)))))

;; Load the Monokai Theme
(load-theme 'monokai t)

;; Enable backtraces on errors
(setq debug-on-error t)

;; Enable Tab Bar mode
(require 'tabbar)
(tabbar-mode t)

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
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  )
(defun my-personal-code-style ()
  (interactive)
  (message "My personal code style!")
  ;; use space instead of tab
  (setq indent-tabs-mode nil)
  ;; indent 2 spaces width
  (my-setup-indent 2))

;; prog-mode-hook requires emacs24+
(add-hook 'prog-mode-hook 'my-personal-code-style)

;; Truncate Long lines
(set-default 'truncate-lines t)

;; Recognize .tf as HCL mode
(add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))

;; Highlight matching parenthesis
(show-paren-mode 1)

;; Display column number
(setq column-number-mode t)

;; Line length limit
;; Breaks whitespace-cleanup
;; (require 'whitespace)
;; (setq whitespace-line-column 80) ;; limit line length
;; (setq whitespace-style '(face lines-tail))

;; (add-hook 'prog-mode-hook 'whitespace-mode)
