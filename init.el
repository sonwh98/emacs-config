(require 'package) ;; You might already have this line

;;https://emacs.stackexchange.com/questions/61997/how-do-i-fix-incomprehensible-buffer-error-when-running-list-packages?newreg=1258b79c62fa449eaab41c1bd5bb9ce4
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;instructions at setting up a clojure/clojurescript development environment at http://lambdakids.stigmergy.systems/2018/6/6/hello-world.blog

;;(add-to-list 'package-archives (cons "melpa"  "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/lisp")

;;download doremi at https://www.emacswiki.org/emacs/DoReMi 
(load "doremi.el")
(load "doremi-cmd.el")

(package-initialize)

(defvar my-packages '(projectile
		      clojure-mode
		      paredit
		      web-mode
                      markdown-mode
		      magit
		      cider
                      org
                      zenburn-theme))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(global-set-key (kbd "C-x g") 'magit-status)
(delete-selection-mode t)

(put 'paredit-forward-delete 'delete-selection 'supersede)


(defvar tramp-ssh-controlmaster-options nil)

(add-hook 'clojure-mode-hook #'paredit-mode)
;;(add-hook 'clojure-mode-hook #'auto-complete-mode)
;;(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'projectile-mode)

(eval-after-load 'web-mode
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))


(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.chp\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.blog\\'" . clojure-mode))
(add-hook 'web-mode-hook #'tagedit-mode)

(setq web-mode-enable-current-element-highlight t)


;;doremi suggested keybindings https://www.emacswiki.org/emacs/doremi-cmd.el
(defalias 'doremi-prefix (make-sparse-keymap))
(defvar doremi-map (symbol-function 'doremi-prefix)
  "Keymap for Do Re Mi commands.")
(define-key global-map "\C-xt" 'doremi-prefix)
(define-key doremi-map "b" 'doremi-buffers+)
(define-key doremi-map "g" 'doremi-global-marks+)
(define-key doremi-map "m" 'doremi-marks+)
(define-key doremi-map "r" 'doremi-bookmarks+) ; reading books?
(define-key doremi-map "s" 'doremi-custom-themes+) ; custom schemes
(define-key doremi-map "w" 'doremi-window-height+)

;;https://github.com/clojure-emacs/cider/issues/2284
(setq cljr-inject-dependencies-at-jack-in nil)
(load-theme 'zenburn t)

;;(global-hl-line-mode 1)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
