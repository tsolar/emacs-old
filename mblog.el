(custom-set-variables
 '(identica-display-success-messages t)
 '(identica-soft-wrap-status t)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil) 
 '(transient-mark-mode nil)
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray15" :foreground "gainsboro" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "xos4" :family "Inconsolata"))))
 '(cursor ((t (:background "yellow" :foreground "black"))))
 '(hl-line ((t (:inherit highlight :background "grey10"))))
 '(region ((t (:background "#535d6c"))))
 '(show-paren-match ((t (:background "RoyalBlue4")))))


(add-to-list 'load-path "~/.emacs.d/elpa/oauth2")

(add-to-list 'load-path "~/.emacs.d/emacs-oauth")
(load "~/.emacs.d/emacs-oauth/oauth.el")

(require 'oauth2)

;;; Identi.ca mode
(add-to-list 'load-path "~/.emacs.d/identica-mode")
;(setq statusnet-server "www.gnewbook.org")
;(add-to-list 'load-path "~/Descargas/identica-mode/identica-mode")
(require 'identica-mode)
(setq identica-auth-mode "oauth")
(setq identica-username "tsolar")
;(load "~/.emacs.d/identica-auth")


;;;Twittering mode
(add-to-list 'load-path "~/.emacs.d/twittering-mode") ;; if you need
(require 'twittering-mode)
(setq twittering-use-master-password t
;      twittering-auth-method 'xauth
      )
(load-library "~/.emacs.d/twitter-auth")

(identica-mode )
(split-window-horizontally )
(twittering-mode )

(add-hook 'twittering-edit-mode-hook (lambda () (ispell-minor-mode) (flyspell-mode)))
(add-hook 'identica-update-status-edit-mode-hook (lambda () (ispell-minor-mode) (flyspell-mode)))
(add-hook 'twittering-mode (lambda () (linum-mode ) (hl-line-mode)))
(add-hook 'identica-mode (lambda () (linum-mode ) (hl-line-mode )))

(set-face-foreground 'identica-username-face "tomato");"#8F0000")
(set-face-foreground 'identica-uri-face "#9BB43E");"#87B4C8")
;; (set-face-foreground 'identica-reply-face "DodgerBlue3");"#9BB43E")
(set-face-background 'identica-reply-face "RoyalBlue4")

(set-face-underline-p 'identica-username-face nil)
(set-face-underline-p 'identica-uri-face nil)
(set-face-underline-p 'identica-reply-face nil)

(set-face-foreground 'twittering-username-face "#0099B9")
(set-face-foreground 'twittering-uri-face "#95E8EC")
(set-face-underline-p 'twittering-username-face nil)
(set-face-underline-p 'twittering-uri-face nil)
