;;Configuration of vanilla emacs. No external package.

(package-initialize)

(setq backup-directory-alist
      `((".*" . ,"~/.emacs_backups")))

(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs_backups" t)))

;; Should do this once a day. 
(setq confirm-kill-emacs 'y-or-n-p)

;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;Resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-j") 'keyboard-quit)

;;Compile.
(global-set-key (kbd "C-c j") 'compile)

;;Show selected text.
(setq-default transient-mark-mode t)

;;Package Management
(load "~/.tuemacs.d/packages.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f81a9aabc6a70441e4a742dfd6d10b2bae1088830dc7aba9c9922f4b1bd2ba50" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages
   (quote
    (monokai-theme solarized-theme doremi yasnippet magit color-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'monokai t)

;;Note this is no good in terminal mode.
(global-set-key (kbd "C-S-n")
		(lambda () (interactive) (forward-line  7)))
(global-set-key (kbd "C-S-p")
		(lambda () (interactive) (forward-line -7)))

;;Modifiers
(when (eq system-type 'darwin)  ; mac specific bindings
  (setq mac-right-command-modifier 'meta));Right command is meta

;;Allows closing all gud buffers.
(defvar all-gud-modes
  '(gud-mode comint-mode gdb-locals-mode gdb-frames-mode  gdb-breakpoints-mode)
  "A list of modes when using gdb")
(defun kill-all-gud-buffers ()
  "Kill all gud buffers including Debugger, Locals, Frames, Breakpoints.
Do this after `q` in Debugger buffer."
  (interactive)
  (save-excursion
        (let ((count 0))
          (dolist (buffer (buffer-list))
                (set-buffer buffer)
                (when (member major-mode all-gud-modes)
                  (setq count (1+ count))
                  (kill-buffer buffer)
                  (delete-other-windows))) ;; fix the remaining two windows issue
          (message "Killed %i buffer(s)." count))))

(setq doc-view-continuous t)


;;scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;(setq scroll-step 1) ;; keyboard scroll one line at a time


(show-paren-mode 1)
(setq show-paren-delay 0)

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))

;;If it ain't broke ...
;;When in minibuffer, don't garbage collect.
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

;;Until you leave it.
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;All below this point is a massive kludge to open a shell
;;in the current window. Needs to be eliminated, but maybe
;;after finals...
(require 'shell)

(defun shell-get-buffer-create (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
	 (prog1
	     (read-buffer "Shell buffer: "
			  ;; If the current buffer is an inactive
			  ;; shell buffer, use it as the default.
			  (if (and (eq major-mode 'shell-mode)
				   (null (get-buffer-process (current-buffer))))
			      (buffer-name)
			    (generate-new-buffer-name "*shell*")))
	   (if (file-remote-p default-directory)
	       ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
	       (setq default-directory
		     (expand-file-name
		      (read-directory-name
		       "Default directory: " default-directory default-directory
		       t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  ;; On remote hosts, the local `shell-file-name' might be useless.
  (if (and (called-interactively-p 'any)
	   (file-remote-p default-directory)
	   (null explicit-shell-file-name)
	   (null (getenv "ESHELL")))
      (with-current-buffer buffer
	(set (make-local-variable 'explicit-shell-file-name)
	     (file-remote-p
	      (expand-file-name
	       (read-file-name
		"Remote shell path: " default-directory shell-file-name
		t shell-file-name))
	      'localname))))

  ;; The buffer's window must be correctly set when we call comint (so
  ;; that comint sets the COLUMNS env var properly).
  (with-current-buffer buffer
    (unless (comint-check-proc buffer)
      (let* ((prog (or explicit-shell-file-name
		       (getenv "ESHELL") shell-file-name))
	     (name (file-name-nondirectory prog))
	     (startfile (concat "~/.emacs_" name))
	     (xargs-name (intern-soft (concat "explicit-" name "-args"))))
        (unless (file-exists-p startfile)
	  (setq startfile (concat user-emacs-directory "init_" name ".sh")))
        (apply 'make-comint-in-buffer "shell" buffer prog
	       (if (file-exists-p startfile) startfile)
	       (if (and xargs-name (boundp xargs-name))
		   (symbol-value xargs-name)
		 '("-i")))
        (shell-mode))))
  buffer)

(defun my-display-buffer (buffer alist direction &optional size pixelwise)
  "BUFFER:  The buffer that will be displayed.
ALIST:  See the doc-string of `display-buffer' for more information.
DIRECTION:  Must use one of these symbols:  'left 'right 'below 'above
SIZE:  See the doc-string for `split-window'.
PIXELWISE:  See the doc-string for `split-window'.
There are three possibilities:
-  (1) If a window on the frame already displays the target buffer,
then just reuse the same window.
-  (2) If there is already a window in the specified direction in relation
to the selected window, then display the target buffer in said window.
-  (3) If there is no window in the specified direction, then create one
in that direction and display the target buffer in said window."
  (let ((window
	 (cond
	  ((get-buffer-window buffer (selected-frame)))
	  ((window-in-direction direction))
	  (t
	   (split-window (selected-window) size direction pixelwise)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))

(defun my-shell ()
  "Create shell in current window."
  (interactive)
  (pop-to-buffer-same-window (shell-get-buffer-create)))

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init)
;;; init.el ends here
