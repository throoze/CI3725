;; .emacs
;; author: Jose Riera
;; date: Julio 2010

;;                                              FULLSCREEN
(defun fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
 		       (if (frame-parameter f 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)
(add-hook 'after-make-frame-functions 'fullscreen)


;; 						ELIMINA EL TOOLBAR
(tool-bar-mode 0)

;; 						CAMBIO DE COLOR - TEMA: ARJEN
(add-to-list 'load-path "~/elisp/color-theme-6.6.0")
(when window-system
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-arjen)
)
;;						AGREGAR CARPETA A LOAD-PATH
(let ((base "~/elisp"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))


;; 						GCL-MODE
;;(autoload 'gcl-mode "gcl-mode")
;;(setq load-path (append '("~/elisp") load-path)) 		;;LINEA POR SI NO AGREGASTE LA CARPETA AL LOAD-PATH
;;(setq auto-mode-alist (append '(("\\.gcl\\'" . gcl-mode)) auto-mode-alist))

;; 						MOVILIDAD ENTRE BUFFERS
(windmove-default-keybindings)

;;						NUMERO DE COLUMNA
(column-number-mode 1)

;;						MACRO FOR-C++
;;(fset 'for-c++
;;      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 102 111 114 32 40 41 123 return return 125 left left up left left left 59 59 down tab] 0 "%d")) arg)))
;; Definifion del shortcut                                         
;;(global-set-key (kbd "C-c C-x C-c") 'for-c++)


;;						AUTO-COMPLETE
;;LINEA POR SI NO AGREGASTE LA CARPETA AL LOAD-PATH:
(add-to-list 'load-path "~/elisp")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/elisp/dict")
(ac-config-default)


;;						FLYMAKE
(add-to-list 'load-path "~/elisp")
(require 'flymake)
(global-set-key [f7] 'flymake-display-err-menu-for-current-line)
(global-set-key [f8] 'flymake-goto-next-error)


;;						EJECUTAR
(global-set-key "\C-x\C-u" 'shell);

;;						ANTI-WORD (NO FUNCIONO)

;;(add-to-list 'load-path "~/elisp")
;;(autoload 'no-word "no-word" "word to txt")
;;(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))
(put 'downcase-region 'disabled nil)

;;                                              PYTHON-MODE
(add-to-list 'load-path "~/elisp/python-mode")
(setq load-path `("~/elisp/python-mode" . ,load-path))
(load-library "python-mode")
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))

;;; Electric Pairs
(add-hook 'python-mode-hook
     (lambda ()
      (define-key python-mode-map "\"" 'electric-pair)
      (define-key python-mode-map "\'" 'electric-pair)
      (define-key python-mode-map "(" 'electric-pair)
      (define-key python-mode-map "[" 'electric-pair)
      (define-key python-mode-map "{" 'electric-pair)))
(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;;; bind RET to py-newline-and-indent
(add-hook 'python-mode-hook '(lambda () 
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; Identacion sin tabs, sino con 4 espacios
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default py-indent-offset 4)

;; SQL MODE
;;(add-to-list 'load-path "~/elisp/sql-mode")
;;(setq load-path `("~/elisp/sql-mode" . ,load-path))
;;(load-library "sql-mode")
;;(setq auto-mode-alist
;;      (cons '("\\.sql$" . sql-mode) auto-mode-alist))

;; para que funcione con oracle:
(defun psql-init ()
    (sql-highlight-oracle-keywords))
(add-hook 'sql-mode-hook 'psql-init)

;; php-mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; yaml-mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing yaml code." t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Ident yaml files
(add-hook 'yaml-mode-hook '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))




