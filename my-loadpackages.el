; ~/.tuemacs.d/my-loadpackages.el

;;loading package
(load "~/.tuemacs.d/my-packages.el")
(require 'doremi)
(require 'color-theme)

(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)

;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (yas-load-directory "~/.tuemacs.d/snippets")
;; (add-hook 'term-mode-hook (lambda()
;; 			    (setq yas-dont-activate t)))
 

(require 'key-chord)
(setq key-chord-two-keys-delay .05
      key-chord-one-key-delay .020)

(key-chord-mode 1)

(key-chord-define-global "df"     "\C-m")
(key-chord-define-global "jk"     "\C-m")
(key-chord-define-global "io"     "\C-i")
(key-chord-define-global "we"     "\C-i")
;;(key-chord-define-global "jo"     "<backspace>")


;; Wind-move
(key-chord-define-global "2f"     "\C-x2")
(key-chord-define-global "3f"     "\C-x3")
(key-chord-define-global "1f"     "\C-x0")
(key-chord-define-global "4f"     "\C-xk")
(key-chord-define-global "sd"     'windmove-left)
(key-chord-define-global "kl"     'windmove-right)
(key-chord-define-global "sf"     'windmove-down)
(key-chord-define-global "jl"     'windmove-up)

;;Org
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;(require 'evil)
;;(evil-mode t)

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
   (global-set-key [mouse-4] (lambda ()
                               (interactive)
                               (scroll-down 1)))
   (global-set-key [mouse-5] (lambda ()
                               (interactive)
                               (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
;;(setq select-active-regions nil)
;;(setq mouse-drag-copy-region t)
;;(global-set-key [mouse-2] 'mouse-yank-at-click)
)

(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
