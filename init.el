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
    haml-mode
    coffee-mode
    find-things-fast
    rinari
    ido-better-flex
    magit
    mo-git-blame
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

; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; 2 tab-with for coffeescript
(defun coffee-custom ()
  "coffee-mode-hook"
  ;; CoffeeScript uses two spaces.
  (make-local-variable 'tab-width)
  (set 'tab-width 2))
(add-hook 'coffee-mode-hook 'coffee-custom)

; Ruby:
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

; Ident with just 2 spaces in method calls:
(setq ruby-deep-indent-paren nil)

; Line numbers on the left
(global-linum-mode t)

; magit
(global-set-key (kbd "C-x g") 'magit-status)

; HAML
(add-hook 'haml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)
            ))
(remove-hook 'haml-mode-hook 'turn-on-auto-fill)
(server-start)

; Rinari (Rails)

(add-hook 'rinari-minor-mode-hook (lambda ()
  (define-prefix-command 'pd-rinari-map1)
  (define-prefix-command 'pd-rinari-map2)
  (local-set-key (kbd "s-R") 'pd-rinari-map1)
  (local-set-key (kbd "s-r") 'pd-rinari-map2)

  (define-key pd-rinari-map1 "'" 'rinari-find-by-context)
  (define-key pd-rinari-map1 ";" 'rinari-find-by-context)
  (define-key pd-rinari-map1 "c" 'rinari-console)
  (define-key pd-rinari-map1 "d" 'rinari-cap)
  (define-key pd-rinari-map1 "e" 'rinari-insert-erb-skeleton)
  (define-key pd-rinari-map1 "g" 'rinari-rgrep)
  (define-key pd-rinari-map1 "p" 'rinari-goto-partial)
  (define-key pd-rinari-map1 "q" 'rinari-sql)
  (define-key pd-rinari-map1 "r" 'rinari-rake)
  (define-key pd-rinari-map1 "s" 'rinari-script)
  (define-key pd-rinari-map1 "t" 'rinari-test)
  (define-key pd-rinari-map1 "w" 'rinari-web-server)
  (define-key pd-rinari-map1 "x" 'rinari-extract-partial)

  (define-key pd-rinari-map2 ";" 'rinari-find-by-context)
  (define-key pd-rinari-map2 "C" 'rinari-find-cells)
  (define-key pd-rinari-map2 "F" 'rinari-find-features)
  (define-key pd-rinari-map2 "M" 'rinari-find-mailer)
  (define-key pd-rinari-map2 "S" 'rinari-find-steps)
  (define-key pd-rinari-map2 "Y" 'rinari-find-sass)
  (define-key pd-rinari-map2 "a" 'rinari-find-application)
  (define-key pd-rinari-map2 "c" 'rinari-find-controller)
  (define-key pd-rinari-map2 "e" 'rinari-find-environment)
  (define-key pd-rinari-map2 "f" 'rinari-find-file-in-project)
  (define-key pd-rinari-map2 "h" 'rinari-find-helper)
  (define-key pd-rinari-map2 "i" 'rinari-find-migration)
  (define-key pd-rinari-map2 "j" 'rinari-find-javascript)
  (define-key pd-rinari-map2 "l" 'rinari-find-lib)
  (define-key pd-rinari-map2 "m" 'rinari-find-model)
  (define-key pd-rinari-map2 "n" 'rinari-find-configuration)
  (define-key pd-rinari-map2 "o" 'rinari-find-log)
  (define-key pd-rinari-map2 "p" 'rinari-find-public)
  (define-key pd-rinari-map2 "r" 'rinari-find-rspec)
  (define-key pd-rinari-map2 "s" 'rinari-find-script)
  (define-key pd-rinari-map2 "t" 'rinari-find-test)
  (define-key pd-rinari-map2 "u" 'rinari-find-plugin)
  (define-key pd-rinari-map2 "v" 'rinari-find-view)
  (define-key pd-rinari-map2 "w" 'rinari-find-worker)
  (define-key pd-rinari-map2 "x" 'rinari-find-fixture)
  (define-key pd-rinari-map2 "y" 'rinari-find-stylesheet)
  (define-key pd-rinari-map2 "z" 'rinari-find-rspec-fixture)
  ))

(add-hook 'haml-mode-hook 'rinari-minor-mode)

(ido-better-flex/enable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-list (quote (("Default viewer" "open %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Default viewer") (output-html "xdg-open"))))
 '(custom-safe-themes (quote ("36afe64261e1de73fcfadedf154e4bc2c9ec1969bde0c21798d31366897bc4d2" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
