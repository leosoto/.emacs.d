(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'")))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator))))
(if window-system (set-exec-path-from-shell-PATH))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages 
  '(starter-kit 
    starter-kit-lisp 
    starter-kit-bindings
    starter-kit-ruby
    starter-kit-eshell
    starter-kit-js
    clojure-mode
    clojure-test-mode
    auctex
    pony-mode
    solarized-theme
    yaml-mode
    coffee-mode
    find-things-fast
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load-theme 'solarized-dark t)
(set-frame-font "Menlo-14")
(setq ns-right-alternate-modifier nil)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(define-key global-map (read-kbd-macro "M-RET") 'hippie-expand)

(require 'find-things-fast)
(global-set-key (kbd "s-o") 'ftf-find-file)
(global-set-key (kbd "s-f") 'ftf-grepsource)

# Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("36afe64261e1de73fcfadedf154e4bc2c9ec1969bde0c21798d31366897bc4d2" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
