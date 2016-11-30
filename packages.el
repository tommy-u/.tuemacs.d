(require 'cl)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar required-packages
  '(
    monokai-theme
    doremi
    doremi-cmd
    color-theme
    iedit
    evil
    magit
    key-chord
    multiple-cursors
    helm
    helm-swoop
    helm-gtags
    sr-speedbar
    xcscope
    company
    ) "a list of packages to ensure are installed at launch.")

					; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

					; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
					; check for new packages (package versions)

  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
					; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;loading packages
(require 'doremi)

(require 'color-theme)

(require 'magit)			

(define-key global-map (kbd "C-c m") 'magit-status)

(require 'key-chord)
(setq key-chord-two-keys-delay .05
      key-chord-one-key-delay .020)

(key-chord-mode 1)
(key-chord-define-global "qw"     "~") ;;
(key-chord-define-global "df"     "\C-m") ;;
(key-chord-define-global "jk"     "\C-m") ;;Ret
(key-chord-define-global "io"     "\C-i") ;;
(key-chord-define-global "we"     "\C-i") ;;Tab

(key-chord-define-global " s"     'sr-speedbar-toggle)

;;Macros
(key-chord-define-global "12"     'kmacro-start-macro)
(key-chord-define-global "90"     'kmacro-end-or-call-macro)
(key-chord-define-global "-="     "\M-0\C-xe") ;;Play to end.
;;(key-chord-define-global "=1"     "") ;;Find way to undo all.

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

(require 'org-install)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . true) (python . true))
 )

(setq org-src-fontify-natively t)



;; Enable mouse support--
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
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(global-set-key (kbd "C-x b") 'helm-buffers-list) ; make Replace buffer list
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; make Replace buffer list

(require 'helm-swoop)
(global-set-key (kbd "M-s") 'helm-swoop)

(require 'helm-gtags)

(require 'xcscope)

(require 'company)
