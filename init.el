;; enable MELPA
;(add-to-list 'package-archives
;     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'load-path "~/.emacs.d/elpa/cl-lib")
(require 'cl-lib)

;; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
;; (setenv "LC_CTYPE" "en_US.UTF-8")


(setq ispell-program-name "aspell")
;; ispell
(autoload 'ispell-word "ispell" "Check the spelling of word in buffer." 't)
(autoload 'ispell-region "ispell" "Check the spelling of region." 't)
(autoload 'ispell-buffer "ispell" "Check the spelling of buffer." t)
(global-set-key (read-kbd-macro "M-$") 'ispell-word)
(setq ispell-dictionary "spanish"
     ispell-skip-sgml t)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)
(setq flyspell-default-dictionary "spanish")

;; Turn on syntax colouring in all modes supporting it:
(global-font-lock-mode t)
(recentf-mode 1) ; keep a list of recently opened files
(delete-selection-mode 1) ;; replace selection when typing


(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files

(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening

(setq stack-trace-on-error t)

;; stop annoying questions
(setq-default abbrev-mode t)
;;(read-abbrev-file “~/.abbrev_defs”)
(setq save-abbrevs t)

;; scrolling
(global-set-key [next]
		(lambda () (interactive)
		  (condition-case nil (scroll-up)
		    (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
		(lambda () (interactive)
		  (condition-case nil (scroll-down)
		    (beginning-of-buffer (goto-char (point-min))))))

;; yanking
;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)

;; smart tabs
(setq-default tab-width 4) ; or any other preferred value
(setq cua-auto-tabify-rectangles nil)

(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
	      '(indent-relative
		indent-relative-maybe))
	(setq indent-tabs-mode nil))
    ad-do-it))

(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
	(indent-tabs-mode
	 (save-excursion
	   (beginning-of-line)
	   (while (looking-at "\t*\\( +\\)\t+")
	     (replace-match "" nil nil nil 1)))
	 (setq tab-width tab-width)
	 (let ((tab-width fill-column)
	       (,offset fill-column)
	       (wstart (window-start)))
	   (unwind-protect
	       (progn ad-do-it)
	     (set-window-start (selected-window) wstart))))
	(t
	 ad-do-it)))))

(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)
(smart-tabs-advice js2-indent-line js2-basic-offset)
(smart-tabs-advice cperl-indent-line cperl-indent-level)

(smart-tabs-advice py-indent-line py-indent-offset)
(smart-tabs-advice py-newline-and-indent py-indent-offset)
(smart-tabs-advice py-indent-region py-indent-offset)

(smart-tabs-advice ruby-indent-line ruby-indent-level)
(setq ruby-indent-tabs-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-default-style "bsd")
 '(c-tab-always-indent nil)
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(electric-pair-mode t)
 '(erc-auto-query (quote window-noselect))
 '(erc-autoaway-mode t)
 '(erc-away-nickname nil)
 '(erc-join-buffer (quote window-noselect))
 '(erc-modules (quote (completion list menu scrolltobottom autojoin button dcc fill irccontrols match move-to-prompt netsplit networks noncommands readonly ring stamp spelling track)))
 '(erc-nick-notify-mode t)
 '(erc-prompt ">")
 '(erc-public-away-p t)
 '(erc-speedbar-sort-users-type (quote alphabetical))
 '(erc-user-full-name "Tomás Solar")
 '(flyspell-mode 1 t)
 '(git-state-modeline-decoration (quote git-state-decoration-small-dot))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(haml-backspace-backdents-nesting nil)
 '(haml-indent-offset 4)
 '(identica-display-success-messages t)
 '(identica-soft-wrap-status t)
 '(ido-enable-flex-matching t)
 '(ido-mode 1 nil (ido))
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(iswitchb-mode 1)
 '(jabber-show-offline-contacts nil)
 '(jabber-show-resources nil)
 '(js2-auto-indent-p t)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(less-css-compile-at-save t)
 '(rainbow-x-colors-major-mode-list (quote (emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode lua-mode html-helper-mode php-mode css-mode lisp-mode)))
 '(safe-local-variable-values (quote ((python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))
") (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))
") (python-shell-completion-setup-code . "from IPython.core.completerlib import module_completion") (python-shell-interpreter-args . "/home/tom/public_html/gigya-data-python/gigya_data_django/manage.py shell") (python-shell-interpreter . "python") (less-css-output-directory . "../css") (less-css-compile-at-save . t))))
 '(save-place t nil (saveplace))
 '(scroll-conservatively 1)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(sml-modeline-mode t)
 '(smtpmail-smtp-server "mail.gnuchile.cl")
 '(smtpmail-smtp-service 25)
 '(tab-always-indent t)
 '(tramp-default-host "localhost")
 '(tramp-default-method "ssh")
 '(transient-mark-mode nil)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray15" :foreground "gainsboro" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "xos4" :family "Inconsolata"))))
 '(cursor ((t (:background "yellow" :foreground "black"))))
 '(highlight ((t (:background "blue"))))
 '(hl-line ((t (:inherit highlight :background "grey10"))))
 '(identica-uri-face ((t (:foreground "#9BB43E"))))
 '(identica-username-face ((t (:foreground "dark red" :underline nil))))
 '(show-paren-match ((t (:background "grey8")))))

;; tabs!
(setq-default indent-tabs-mode t)

;; python issues with tabs...
;; (add-hook 'python-mode-hook guess-style-guess-tabs-mode)
;; (add-hook 'python-mode-hook (lambda ()
;; 			      (when indent-tabs-mode
;; 				(guess-style-guess-tab-width)))

(setq tab-width 4)
(setq-default c-basic-offset 4)

;; delete trailing whitespaces!
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1))) ;; autoindent
;;(setq c-indent-comments-syntactically-p t)
;;(setq c-double-slash-is-comments-p t)

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path "~/.emacs.d")

;; calendar localization
(setq calendar-day-name-array ["lunes" "martes" "miércoles" "jueves" "viernes" "sábado" "domingo"]
      calendar-month-name-array ["enero" "febrero" "marzo" "abril" "mayo"
                                 "junio" "julio" "agosto" "septiembre"
                                 "octubre" "noviembre" "diciembre"])

;; mover línea hacia arriba
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

;; mover línea hacia abajo
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


(global-set-key (kbd "S-M-<up>") 'move-line-up)
(global-set-key (kbd "S-M-<down>") 'move-line-down)


;; atajo para comentar
;; (global-set-key [(control shift c)] 'comment-or-uncomment-region)

(global-set-key [(shift f1)] 'buffer-menu)

;;(define-key global-map (kbd "RET") 'newline-and-indent)

;; flymake
;(require 'flymake)
(add-to-list 'load-path  "~/.emacs.d/emacs-flymake/")
(add-hook 'find-file-hook 'flymake-mode)

;; enhancements for displaying flymake errors
(add-to-list 'load-path  "~/.emacs.d/emacs-flymake-cursor/")
(require 'flymake-cursor)

;; Let's run 8 checks at once instead.
(setq flymake-max-parallel-syntax-checks 8)

;; I don't want no steekin' limits.
(setq flymake-max-parallel-syntax-checks nil)


;; Yes, I want my copies in the same dir as the original.
;; (setq flymake-run-in-place t)

;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)
;; This lets me say where my temp dir is.
;;(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq temporary-file-directory "/tmp")

;; I want to see at most the first 4 errors for a line.
(setq flymake-number-of-errors-to-display 4)

;; I want to see all errors for the line.
(setq flymake-number-of-errors-to-display nil)


;; run flymake for modes...
(add-hook 'php-mode-hook 'flymake-mode)
;;(add-hook 'php-mode-hook 'flymake)

;;Here are instructions how to make flymake work with HTML:
(defun flymake-html-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "tidy" (list local-file))))

;;(add-to-list 'flymake-allowed-file-name-masks
;;             '("\\.html$\\|\\.ctp" flymake-html-init))

(add-to-list 'flymake-err-line-patterns
             '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
               nil 1 2 4))


;;;;;;; modes

;; nginx mode
(add-to-list 'load-path
              "~/.emacs.d/nginx-mode")
(require 'nginx-mode)

;; idle-highlight-mode
(add-to-list 'load-path "~/.emacs.d/idle-highlight-mode")
(require 'idle-highlight-mode)
(defun my-coding-hook ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (if window-system (hl-line-mode t))
  (idle-highlight-mode t))

(add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
(add-hook 'ruby-mode-hook 'my-coding-hook)
(add-hook 'js2-mode-hook 'my-coding-hook)
(add-hook 'php-mode-hook 'my-coding-hook)
(add-hook 'python-mode-hook 'my-coding-hook)

;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;rainbow mode
;(add-to-list 'load-path "~/.emacs.d/jd-el")
;(require 'rainbow-mode)
(add-to-list 'load-path "~/.emacs.d/rainbow-mode")
(require 'rainbow-mode)

;; CSS!!
(autoload 'css-mode "css-mode")

(eval-after-load "css-mode"
  '(add-hook 'css-mode-hook
             'rainbow-mode
             )
  )

;; LESS!!!!
(add-to-list 'load-path "~/.emacs.d/less-css-mode")
(require 'less-css-mode)

;; lua mode;
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; flymake lua
(add-to-list 'load-path "~/.emacs.d/emacs-utils")
(require 'flymake-lua)
(add-hook 'lua-mode-hook 'flymake-lua-load)
(eval-after-load "lua-mode"
  '(add-hook 'lua-mode-hook 'rainbow-mode)
  )


;;browser
(require 'w3m-load)
(require 'w3m)

(setq browse-url-generic-program (executable-find "x-www-browser")
          browse-url-browser-function 'browse-url-generic)


;;; git clone https://github.com/magnars/mark-multiple.el.git
(add-to-list 'load-path "~/.emacs.d/mark-multiple.el")
;(load  "~/.emacs.d/mark-multiple.el/rename-sgml-tag.el")
(require 'sgml-mode)
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(require 'rename-sgml-tag)
(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;;; git clone https://github.com/magnars/multiple-cursors.el.git
(add-to-list 'load-path "~/.emacs.d/multiple-cursors.el")
(require 'multiple-cursors)


;;; git clone https://github.com/magnars/dash.el.git
(add-to-list 'load-path "~/.emacs.d/dash.el")
(require 'dash)

;;; git clone https://github.com/mooz/js2-mode.git
(add-to-list 'load-path "~/.emacs.d/js2-mode")
;(require 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; s is required for js2-refactor
(add-to-list 'load-path "~/.emacs.d/s.el")
(require 's)

;;; git clone https://github.com/magnars/js2-refactor.el.git
(add-to-list 'load-path "~/.emacs.d/js2-refactor.el")
(require 'js2-refactor)

;(require 'js2-rename-var)
;(define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)


;;; git clone https://github.com/magnars/expand-region.el.git
(add-to-list 'load-path "~/.emacs.d/expand-region.el")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)


;; php mode
(add-to-list 'load-path  "~/.emacs.d/php-mode")
(require 'php-mode)

;; other php mode
;; (add-to-list 'load-path  "~/.emacs.d/pi-php-mode")
;; (require 'pi-php-mode)

(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))


(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '4)
;;
(add-hook 'php-mode-user-hook 'turn-on-font-lock)
;;
;; Not exactly related to editing HTML: enable editing help
;; with mouse-3 in all sgml files
(defun go-bind-markup-menu-to-mouse3 ()
  (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))
;;
(add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3)

(defun insert-php-region ()
  (interactive "*")
  (let ((php-template '("<?php" > n p n "?>" > n )))
    (tempo-insert-template 'php-template tempo-insert-region)
    (mmm-parse-buffer)))

(defun my-php-hook ()
  (c-set-style "cc-mode")
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq c-basic-offet 4)
  (c-toggle-hungry-state t)

  ;; C-c C-f is used by pgsml
  (define-key php-mode-map
    "\C-cd"
    'php-search-documentation)

  ;; C-c C-m is used by pgsml
  (define-key php-mode-map
    "\C-cb"
    'php-browse-manual)

  (define-key html-mode-map
    "\C-c\C-p"
    'insert-php-region))

(add-hook 'php-mode-hook 'my-php-hook)


;; web-mode :)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/web-mode"))
(require 'web-mode)
(setq web-mode-engines-alist
	  '(
		;;("django" . "\\.html\\'")
		;;("php" . "\\.ctp\\'")
		)
	  )
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))



(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-indent-style 2)
(set-face-attribute 'web-mode-css-at-rule-face nil :foreground "Pink3")
(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
(setq web-mode-disable-auto-pairing nil)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)
(setq web-mode-disable-css-colorization nil)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook 'web-mode-hook)



;; python & django
;; this now comes with emacs 24.3
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/python.el"))
;;(require 'python)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/python-django.el"))
(require 'python-django)
(global-set-key (kbd "C-x j") 'python-django-open-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;enable pep8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To enable pep8 check
;; install pep8 checker with one of those commands
;; sudo apt-get install pep8
;; or
;; sudo pip install pep8

(when (load "flymake" t)
 (defun flymake-pylint-init ()
   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
          (local-file (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
         (list "pep8" (list "--repeat" local-file))))

 (add-to-list 'flymake-allowed-file-name-masks
              '("\\.py\\'" flymake-pylint-init)))

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)


;; SASS mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/scss-mode"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; Haml mode
(defun my-tabs-mode-hook ()
      (setq indent-tabs-mode t
			)
	  )
(add-to-list 'load-path (expand-file-name "~/.emacs.d/haml-mode"))
(require 'haml-mode)

(add-hook 'haml-mode-hook 'my-tabs-mode-hook)

;; diff highline
(add-to-list 'load-path (expand-file-name "~/.emacs.d/diff-hl"))
(require 'diff-hl)
(global-diff-hl-mode)

;; git-emacs :)
(add-to-list 'load-path "~/.emacs.d/git-emacs")
(require 'git-emacs)

;; git-modes :)
(add-to-list 'load-path "~/.emacs.d/git-modes")
(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)

;; magit - a git mode for emacs
(add-to-list 'load-path "~/.emacs.d/magit")
(require 'magit)

;; monky - magit for mercurial :)
(add-to-list 'load-path "~/.emacs.d/monky/")
(require 'monky)

;; By default monky spawns a seperate hg process for every command.
;; This will be slow if the repo contains lot of changes.
;; if `monky-process-type' is set to cmdserver then monky will spawn a single
;; cmdserver and communicate over pipe.
;; Available only on mercurial versions 1.9 or higher

(setq monky-process-type 'cmdserver)

;; varnish
(require 'vcl-mode)



;; Load ERC
;(add-to-list 'load-path "~/Descargas/erc/")
;(load "~/Descargas/emacs/erc/erc.el")
;(require 'erc)
;(require 'erc-nicklist)
(erc-spelling-mode 1)
;; Load authentication info from an external source.  Put sensitive
;; passwords and the like in here.
(setq erc-email-userid "tsolar")
(setq erc-auto-query 'buffer)
(load "~/.emacs.d/erc-auth")

;; Join channels whenever connecting to Freenode.
(setq erc-autojoin-channels-alist '(("freenode.net" "#parabola" "#fsfla" "#flisol-cl")
                                    ("partidopirata.cl" "#ppirata-cl" )
                                   )
)
;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)


;; timestamps
(make-variable-buffer-local
 (defvar erc-last-datestamp nil))

(defun ks-timestamp (string)
  (erc-insert-timestamp-left string)
  (let ((datestamp (erc-format-timestamp (current-time) erc-datestamp-format)))
    (unless (string= datestamp erc-last-datestamp)
      (erc-insert-timestamp-left datestamp)
      (setq erc-last-datestamp datestamp))))
(setq erc-fill-static-center 15)

(setq erc-timestamp-only-if-changed-flag t
      erc-timestamp-format "%H:%M:%S "
      erc-datestamp-format "=== [%Y-%m-%d %a] ===\n" ; mandatory ascii art
      erc-fill-prefix      "         "
      erc-insert-timestamp-function 'ks-timestamp)


(add-hook 'window-configuration-change-hook
	   '(lambda ()
 erc-fill-column (- (window-width) 2))))

(add-to-list 'load-path "~/.emacs.d/erc-highlight-nick/")
;(erc-highlight-nicknames-mode )
(require 'erc-highlight-nicknames)

(add-to-list 'load-path "~/.emacs.d/erc-nick-notify")
(require 'erc-nick-notify)
;(require 'erc-tab)

;;colores!!!!!
(set-face-foreground 'erc-input-face "gold")
(set-face-foreground 'erc-my-nick-face "gold")
(set-face-foreground 'erc-timestamp-face "cyan")

(defvar erc-channels-to-visit nil
   "Channels that have not yet been visited by erc-next-channel-buffer")
(defun erc-next-channel-buffer ()
  "Switch to the next unvisited channel. See erc-channels-to-visit"
  (interactive)
  (when (null erc-channels-to-visit)
    (setq erc-channels-to-visit
          (remove (current-buffer) (erc-channel-list nil))))
          (let ((target (pop erc-channels-to-visit)))
          (if target
              (switch-to-buffer target)
          )
    )
)

;; contar ops voices y members..
(define-minor-mode ncm-mode "" nil
  (:eval
   (let ((ops 0)
         (voices 0)
         (members 0))
     (maphash (lambda (key value)
                (when (erc-channel-user-op-p key)
                  (setq ops (1+ ops)))
                (when (erc-channel-user-voice-p key)
                  (setq voices (1+ voices)))
                (setq members (1+ members)))
              erc-channel-users)
     (format " %S/%S/%S" ops voices members))))

(add-hook 'erc-mode-hook 'ncm-mode)
;;fin erc

;(require org)
;(require org-feed)
(setq org-feed-alist
      '(("Remember The Milk"
         "https://www.rememberthemilk.com/rss/tsolar/"
         "~/org/GTD.org"
         "Remember The Milk"
         :template "* TODO %title\n  %a\n "
         )))

;;* rtm feed timer
;;(run-at-time 3600 3600 'org-feed-update-all)

;; Org mode
(setq org-directory "~/org")
;; MobileOrg
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))

;; wanderlust stuff
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "tsolar@gmail.com")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "tsolar")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t)

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))
;; end wanderlust stuff

;; emacs-jabber stuff
(require 'jabber)
(setq jabber-account-list
	  '(("tsolar@gmail.com"
		 (:network-server . "talk.google.com")
		 (:password . "fcyqlrhzfheydxnb")
		 (:connection-type . ssl))))

(defun egh:jabber-google-groupchat-create ()
  (interactive)
  (let ((group (apply 'format "private-chat-%x%x%x%x%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x%x%x%x%x%x%x%x%x@groupchat.google.com"
					  (mapcar (lambda (x) (random x)) (make-list 32 15))))
		(account (jabber-read-account)))
	(jabber-groupchat-join account group (jabber-muc-read-my-nickname account group) t)))

(defun x-urgency-hint (frame arg &optional source)
  (let* ((wm-hints (append (x-window-property
							"WM_HINTS" frame "WM_HINTS" source nil t) nil))
		 (flags (car wm-hints)))
    (setcar wm-hints
			(if arg
				(logior flags #x00000100)
			  (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

;; usage example
(defvar jabber-activity-jids-count 0)

(defun jabber-urgency-hint ()
  (let ((count (length jabber-activity-jids)))
    (unless (= jabber-activity-jids-count count)
      (if (zerop count)
		  (x-urgency-hint (selected-frame) nil)
		(x-urgency-hint (selected-frame) t))
      (setq jabber-activity-jids-count count))))

(add-hook 'jabber-activity-update-hook 'jabber-urgency-hint)

(defvar libnotify-program "/usr/bin/notify-send")

(defun notify-send (title message)
  (start-process "notify" " notify"
		 libnotify-program "--expire-time=4000" title message))

(defun libnotify-jabber-notify (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via libnotify"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (notify-send (format "(PM) %s"
                       (jabber-jid-displayname (jabber-jid-user from)))
               (format "%s: %s" (jabber-jid-resource from) text)))
      (notify-send (format "%s" (jabber-jid-displayname from))
             text)))

;; (add-hook 'jabber-alert-message-hooks 'libnotify-jabber-notify)

(setq jabber-vcard-avatars-retrieve nil
      jabber-chat-buffer-show-avatar nil)

(define-key jabber-chat-mode-map [S-return] 'newline)
;; (define-key jabber-chat-mode-map [C-return] 'jabber-chat-buffer-send)

(setq my-chat-prompt "[%t] %n> ")
(when (featurep 'jabber)
  (setq
   jabber-chat-foreign-prompt-format my-chat-prompt
   jabber-chat-local-prompt-format my-chat-prompt
   jabber-groupchat-prompt-format my-chat-prompt
   jabber-muc-private-foreign-prompt-format "[%t] %g/%n> "
   )
  )


(defun my-jabber-chat-delete-or-bury ()
  (interactive)
  (if (eq 'jabber-chat-mode major-mode)
      (condition-case e
          (delete-frame)
        (error
         (if (string= "Attempt to delete the sole visible or iconified frame"
                      (cadr e))
			 (bury-buffer))))))

(add-hook 'jabber-chat-mode-hook 'goto-address)

(setq jabber-chat-header-line-format
	  '(" " (:eval (jabber-jid-displayname jabber-chatting-with))
    	" " (:eval (jabber-jid-resource jabber-chatting-with)) "\t";
    	(:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
				 (propertize
				  (or
				   (cdr (assoc (get buddy 'show) jabber-presence-strings))
				   (get buddy 'show))
				  'face
				  (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
					  'jabber-roster-user-online))))
    	"\t" (:eval (get (jabber-jid-symbol jabber-chatting-with) 'status))
    	(:eval (unless (equal "" *jabber-current-show*)
				 (concat "\t You're " *jabber-current-show*
					 " (" *jabber-current-status* ")")))))

(add-to-list 'load-path "~/.emacs.d/autosmiley")
(require 'autosmiley)
(add-hook 'jabber-chat-mode-hook 'autosmiley-mode)

(defun jabber ()
  (interactive)
  (define-key jabber-chat-mode-map [escape]
    'my-jabber-chat-delete-or-bury)
  (jabber-connect-all)
  (switch-to-buffer "*-jabber-*"))
