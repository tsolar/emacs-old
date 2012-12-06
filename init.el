; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
;(setenv "LC_CTYPE" "en_US.UTF-8")

                                        ; Turn on syntax colouring in all modes supporting it:
(global-font-lock-mode t)
(delete-selection-mode 1) 
(recentf-mode 1) ; keep a list of recently opened files
(delete-selection-mode 1) ;; replace selection when typing

(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-default-style "bsd")
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(default-input-method "spanish-keyboard")
 '(display-battery-mode t)
 '(display-time-mode t)
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(erc-auto-query (quote window-noselect))
 '(erc-autoaway-mode t)
 '(erc-away-nickname nil)
 '(erc-join-buffer (quote window-noselect))
 '(erc-modules (quote (completion list menu scrolltobottom autojoin button dcc fill irccontrols match move-to-prompt netsplit networks noncommands readonly ring stamp spelling track)))
 '(erc-prompt ">")
 '(erc-public-away-p t)
 '(erc-speedbar-sort-users-type (quote alphabetical))
 '(erc-user-full-name "Tomás Solar")
 '(flyspell-mode 1 t)
 '(git-state-modeline-decoration (quote git-state-decoration-small-dot))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(identica-display-success-messages t)
 '(identica-soft-wrap-status t)
 '(ido-enable-flex-matching t)
 '(ido-mode 1 nil (ido))
 '(inhibit-startup-screen t)
 '(iswitchb-mode 1)
 '(js2-auto-indent-p t)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(rainbow-x-colors-major-mode-list (quote (emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode lua-mode html-helper-mode php-mode css-mode lisp-mode)))
 '(save-place t nil (saveplace))
 '(scroll-conservatively 1)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(sml-modeline-mode t)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray15" :foreground "gainsboro" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "xos4" :family "Terminus"))))
 '(cursor ((t (:background "yellow" :foreground "black"))))
 '(hl-line ((t (:inherit highlight :background "grey10"))))
 '(region ((t (:background "#535d6c"))))
 '(show-paren-match ((t (:background "RoyalBlue4")))))

;; Only spaces, please!
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq-default c-basic-offset 4)

;(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1))) ;; autoindent
(setq c-indent-comments-syntactically-p nil)
(setq c-double-slash-is-comments-p t)

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path "~/.emacs.d")

;; calendar localization
(setq calendar-day-name-array ["lunes" "martes" "miércoles" "jueves" "viernes" "sábado" "domingo"]
      calendar-month-name-array ["enero" "febrero" "marzo" "abril" "mayo"
                                 "junio" "julio" "agosto" "septiembre"
                                 "octubre" "noviembre" "diciembre"])



;; atajo para comentar
;; (global-set-key [(control shift c)] 'comment-or-uncomment-region)

(global-set-key [(shift f1)] 'buffer-menu)

(define-key global-map (kbd "RET") 'newline-and-indent)
  
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
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.html$\\|\\.ctp" flymake-html-init))

(add-to-list 'flymake-err-line-patterns
             '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
               nil 1 2 4))


;;;;;;; modes

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



;;; git clone hhttps://github.com/magnars/mark-multiple.el.git
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

;;; git clone https://github.com/mooz/js2-mode.git
(add-to-list 'load-path "~/.emacs.d/js2-mode")
;(require 'js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


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
;; (add-to-list 'load-path  "~/.emacs.d/php-mode")
;; (require 'php-mode)

;; other php mode
(add-to-list 'load-path  "~/.emacs.d/pi-php-mode")
(require 'pi-php-mode)

(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))


;; git clone https://github.com/echosa/zf-mode.git
;; (add-to-list 'load-path "~/.emacs.d/zf-mode/")
;; (add-to-list 'load-path "~/.emacs.d/zf-mode/bundled")

;; (require 'zf-mode)
;; (zf-mode-setup)

;; multi-web-mode
(add-to-list 'load-path "~/.emacs.d/multi-web-mode")
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\?\\|<\\?=" "\\?>")
                  (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)



;; magit - a git mode for emacs
(add-to-list 'load-path "~/.emacs.d/magit")
(require 'magit)

;; git-emacs :)
(add-to-list 'load-path "~/.emacs.d/git-emacs")
(require 'git-emacs)


;;;elscreen
(load "elscreen" "ElScreen" t)
;(setq elscreen-prefix-key “\C-z”)

(defun elscreen-frame-title-update ()
  (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
 	   (screen-to-name-alist (elscreen-get-screen-to-name-alist))
 	   (title (mapconcat
 		   (lambda (screen)
 		     (format "%d%s %s"
 			     screen (elscreen-status-label screen)
 			     (get-alist screen screen-to-name-alist)))
 		   screen-list " ")))
      (if (fboundp 'set-frame-name)
 	  (set-frame-name title)
 	(setq frame-title-format title)))))

(eval-after-load "elscreen"
  '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))


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

(add-to-list 'load-path "~/.emacs.d/erc-highlight-nick/")
;(erc-highlight-nicknames-mode )
(require 'erc-highlight-nicknames)

(add-hook 'window-configuration-change-hook
	   '(lambda ()
	      (setq erc-fill-column (- (window-width) 2))))

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
         "https://www.rememberthemilk.com/rss/jonnay/"
         "~/org/GTD.org"
         "Remember The Milk"
         :template "* TODO %title\n  %a\n "
         )))

;;* rtm feed timer
(run-at-time 3600 3600 'org-feed-update-all)
