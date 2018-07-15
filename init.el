(require 'package) ;; You might already have this line

;;instructions at setting up a clojure/clojurescript development environment at http://lambdakids.stigmergy.systems/2018/6/6/hello-world.blog

(add-to-list 'package-archives (cons "melpa-stable"  "https://stable.melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/lisp")

;;download doremi at https://www.emacswiki.org/emacs/DoReMi 
(load "doremi.el")
(load "doremi-cmd.el")


(package-initialize)

(defvar my-packages '(better-defaults
		      projectile
                      find-file-in-project
		      clojure-mode
		      paredit
		      web-mode
		      aggressive-indent
		      magit
		      cider
		      ac-cider
		      ))

(unless (package-installed-p 'ac-cider)
  (package-list-packages))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(global-set-key (kbd "C-x g") 'magit-status)
(delete-selection-mode t)

(put 'paredit-forward-delete 'delete-selection 'supersede)


(defvar tramp-ssh-controlmaster-options nil)

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'auto-complete-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)


(setq ffip-find-options "-not -iwholename '*/target/*' -not -iwholename '*/compiled/*' -not -iwholename '*/generated/*' ")

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

;;tmux key mapping for paredit for remote ssh
(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\e[1;5D" [C-left])

;;tmux key mapping for paredit for localhost. why are they different?
(global-set-key (kbd "M-[ A") [C-up])
(global-set-key (kbd "M-[ B") [C-down])
(global-set-key (kbd "M-[ C") [C-right])
(global-set-key (kbd "M-[ D") [C-left])

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
(load-theme 'wheatgrass t)


(global-hl-line-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ac-cider cider magit aggressive-indent web-mode paredit clojure-mode find-file-in-project projectile better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
