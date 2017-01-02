;;;

(require 'cl)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar required-packages
  '(
    expand-region
    nyan-mode
    avy
    aggressive-indent
    monokai-theme
    magit
    key-chord
    multiple-cursors
    helm
    helm-swoop
    helm-gtags
    helm-flyspell
    helm-descbinds
    diff-hl
    helm-projectile
    sr-speedbar
    default-text-scale
    flycheck
    undo-tree
    hydra
    company
    ) "A list of packages to ensure are installed at launch.")

(defun packages-installed-p ()
  "Method to check if all packages are installed."
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(key-chord-define-global "

(require 'hydra)

(key-chord-define-global
 "jk"
 (defhydra hydra-move
   (:pre
    (set-cursor-color "#40e0d0")
    :post
    (set-cursor-color "#ffffff")
    )
   "move"
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ("h" backward-char)   
   ("J" (lambda () (interactive) (forward-line  8)))
   ("K" (lambda () (interactive) (forward-line -8)))
   ("L" forward-word)
   ("H" backward-word)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("i" kill-line)
   ("y" yank)
   ("m" set-mark-command)
   ("w" kill-region)
   ("E" er/expand-region)
   ("c" er/contract-region)
   (" " helm-swoop)
   ("o" recenter-top-bottom)
   ("u" undo-tree-undo)
   ("f" avy-goto-word-1 :exit t)
   ("x" delete-char )
   ("d" nil "quit")
   ))

(defhydra hydra-helm (:hint nil :color pink)
  "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
        ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
        ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
  ("<tab>" helm-keyboard-quit "back" :exit t)
  ("<escape>" nil "quit")
  ("\\" (insert "\\") "\\" :color blue)
  ("h" helm-beginning-of-buffer)
  ("j" helm-next-line)
  ("k" helm-previous-line)
  ("l" helm-end-of-buffer)
  ("g" helm-beginning-of-buffer)
  ("G" helm-end-of-buffer)
  ("n" helm-next-source)
  ("p" helm-previous-source)
  ("K" helm-scroll-other-window-down)
  ("J" helm-scroll-other-window)
  ("c" helm-recenter-top-bottom-other-window)
  ("m" helm-toggle-visible-mark)
  ("t" helm-toggle-all-marks)
  ("u" helm-unmark-all)
  ("H" helm-help)
  ("s" helm-buffer-help)
  ("v" helm-execute-persistent-action)
  ("d" helm-persistent-delete-marked)
  ("y" helm-yank-selection)
  ("w" helm-toggle-resplit-and-swap-windows)
  ("f" helm-follow-mode))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'expand-region)
(pending-delete-mode t)


(require 'aggressive-indent)

(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'python-mode)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(require 'magit)

(define-key global-map (kbd "C-c m") 'magit-status)

;; Maybe this would make a good hydra?
(require 'key-chord)
(setq key-chord-two-keys-delay .05
      key-chord-one-key-delay .18)

(key-chord-mode 1)
(key-chord-define-global "qw"     "~")
(key-chord-define-global "df"     "\C-m") ;;Ret
(key-chord-define-global "we"     "\C-i") ;;Tab
(key-chord-define-global " q"     'sr-speedbar-toggle)
(key-chord-define-global "jo"     'delete-backward-char)
(key-chord-define-global "JO"     'backward-kill-word)
(key-chord-define-global "fw"     'delete-forward-char)
(key-chord-define-global "jj"     'compile)
(key-chord-define-global "kk"     'comment-or-uncomment-region)

;;Commonly used fns.
(key-chord-define-global "uu"     'undo-tree-visualize)
(key-chord-define-global "xx"     'helm-M-x)
(key-chord-define-global "cc"     'magit-status)
(key-chord-define-global "DD"     'helm-descbinds)
(key-chord-define-global "yy"     'helm-show-kill-ring)
(key-chord-define-global "vv"     'eval-last-sexp)
(key-chord-define-global "hh"     'finder-by-keyword)
(key-chord-define-global "ww"     'save-buffer)
(key-chord-define-global "IO"     'er/expand-region)

;;Macros
(key-chord-define-global "12"     'kmacro-start-macro)
(key-chord-define-global "90"     'kmacro-end-or-call-macro)
(key-chord-define-global "-="     "\M-0\C-xe") ;;Play to end.

;; Wind-move
(key-chord-define-global "2f"     "\C-x2")
(key-chord-define-global "3f"     "\C-x3")
(key-chord-define-global "1f"     "\C-x0")
(key-chord-define-global "4f"     "\C-xk")

;;Should be replaced by avy.
;;(key-chord-define-global "sd"     'windmove-left)
;;(key-chord-define-global "kl"     'windmove-right)
;;(key-chord-define-global "sf"     'windmove-down)
;;(key-chord-define-global "jl"     'windmove-up)

(require 'avy)
(key-chord-define-global "JK"     'avy-goto-word-1)
(key-chord-define-global "jf"     'avy-goto-word-1)

(require 'org-install)
(setq org-hide-leading-stars t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . true) (python . true))
 )

(setq org-src-fontify-natively t)

;; Enable some mouse support--
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
  )

(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal

;;Cleanup helm sizing.
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)

(require 'helm-gtags)

(require 'diff-hl)
(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(require 'helm-swoop)
(global-set-key (kbd "C-s") 'helm-swoop)

(require 'helm-flyspell)
(key-chord-define-global "ji"     'helm-flyspell-correct) ;;Spelling
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(require 'helm-projectile)

(global-set-key (kbd "C-h f") 'helm-apropos)
(setq projectile-completion-system 'helm)

;; enable Helm version of Projectile with replacement commands
(helm-projectile-on)


(require 'default-text-scale)

(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)
(default-text-scale-increase)
(default-text-scale-increase)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'find-file-hook
          (lambda ()
            (unless (tramp-tramp-file-p (buffer-file-name))
              (flycheck-mode))))

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;turn on everywhere
(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

(provide 'packages)
;;; packages.el ends here
