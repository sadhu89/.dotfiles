;;; -*- lexical-binding: t -*-

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
;;  '(custom-safe-themes
;;    (quote
;;     ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
;;  '(global-linum-mode nil)
;;  '(neo-window-fixed-size nil)
;;  '(nrepl-message-colors
;;    (quote
;;     ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
;;  '(package-selected-packages
;;    (quote
;;     (enh-ruby-mode shackle counsel-projectile ag counsel ruby-refactor fancy-narrow transpose-frame demo-it color-theme-sanityinc-tomorrow org-bullets eshell-git-prompt vimgolf yasnippet org-tree-slide js2-mode sml-mode evil-tutor evil haskell-mode elm-mode rainbow-mode multiple-cursors alert el-get slack dumb-jump rubocop rspec-mode github-browse-file helm-spotify-plus 0xc helm-open-github use-package all-the-icons neotree d-mode coffee-mode markdown-mode haml-mode csv-mode zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line operate-on-number move-text magit projectile ov imenu-anywhere guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major dash crux browse-kill-ring beacon anzu ace-window)))
;;  '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838"))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-document-title ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif" :height 1.5 :underline nil))))
;;  '(org-level-1 ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif" :height 1.75))))
;;  '(org-level-2 ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif" :height 1.5))))
;;  '(org-level-3 ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif" :height 1.25))))
;;  '(org-level-4 ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif" :height 1.1))))
;;  '(org-level-5 ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif"))))
;;  '(org-level-6 ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif"))))
;;  '(org-level-7 ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif"))))
;;  '(org-level-8 ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif")))))

;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(add-hook 'ruby-mode-hook #'rubocop-mode)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :ensure)


;; Slack

;; (use-package slack
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (slack-register-team
;;    :name "TrikeApps"
;;    :default t
;;    :client-id "4069449787.157999864916"
;;    :client-secret "38c0837a517e0f74b2549eff04b82049"
;;    :token "xoxp-4069449787-30948113633-76499359268-554c4e1138"
;;    :subscribed-channels '(bel-announce random dev-announce team-squid bel-chat rfr)))

;;   ;; (slack-register-team
;;   ;;  :name "test"
;;   ;;  :client-id "3333333333.77777777777"
;;   ;;  :client-secret "cccccccccccccccccccccccccccccccc"
;;   ;;  :token "xxxx-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
;;   ;;  :subscribed-channels '(hoge fuga))

;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))


;; Multiple-cursors

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(with-eval-after-load 'multiple-cursors-core
  (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
  (define-key mc/keymap (kbd "C-,") 'mc/unmark-next-like-this)
  (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this))

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key [remap other-window] 'ace-window)

;; Smartparens keys

(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)
)

(setq elm-format-on-save t)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; (setq guru-warn-only nil)

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

;; org-tree-mode
;; (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
;; (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)


;; git prompt
(eshell-git-prompt-use-theme 'git-radar)

(setq vimgolf-key "5a9213dc33bb371c4fb74b99a0db7bd3")

;; org-mode this cause troubles
;; (setq org-hide-emphasis-markers t)
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ +\\([-*]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))



;; (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces 'user
;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

(setq prelude-theme 'color-theme-sanityinc-tomorrow)

(setq org-tree-slide-slide-in-effect nil)
(setq org-tree-slide-cursor-init nil)


(setq projectile-completion-system 'ivy)
(counsel-projectile-on)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;(setq enable-recursive-minibuffers nil)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(global-set-key (kbd "C-c p s r") 'counsel-projectile-rg)
(global-set-key (kbd "s-p s r") 'counsel-projectile-rg)


;; (defun reloading (cmd)
;;   (lambda (x)
;;     (funcall cmd x)
;;     (ivy--reset-state ivy-last)))
;; (defun given-file (cmd prompt) ; needs lexical-binding
;;   (lambda (source)
;;     (let ((target
;;            (let ((enable-recursive-minibuffers t))
;;              (read-file-name
;;               (format "%s %s to:" prompt source)))))
;;       (funcall cmd source target 1))))
;; (defun confirm-delete-file (x)
;;   (dired-delete-file x 'confirm-each-subdirectory))

;; (ivy-add-actions
;;  'counsel-find-file
;;  `(("c" ,(given-file #'copy-file "Copy") "copy")
;;    ("d" ,(reloading #'confirm-delete-file) "delete")
;;    ("m" ,(reloading (given-file #'rename-file "Move")) "move")))
;; (ivy-add-actions
;;  'counsel-projectile-find-file
;;  `(("c" ,(given-file #'copy-file "Copy") "copy")
;;    ("d" ,(reloading #'confirm-delete-file) "delete")
;;    ("m" ,(reloading (given-file #'rename-file "Move")) "move")
;;    ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))

;; rspec-mode
;(setq display-buffer-reuse-frames t)
(setq rspec-primary-source-dirs '("app")) ;; To work with spec/lib folders - See https://github.com/pezra/rspec-mode/issues/157

;; enh-ruby-mode
(setq enh-ruby-deep-indent-paren nil)
(setq enh-ruby-hanging-paren-deep-indent-level 2)
;; don't insert utf-8 encoding comments in file header
(setq ruby-insert-encoding-magic-comment nil)

;; values to have a look in the future
;; (setq ruby-deep-indent-paren nil)
;; (setq ruby-insert-encoding-magic-comment nil)
;; (setq enh-ruby-deep-indent-paren t)
;; (setq enh-ruby-deep-arglist t)
;; (setq enh-ruby-hanging-brace-deep-indent-level 1)
;; (setq enh-ruby-hanging-brace-indent-level 2)
;; (setq enh-ruby-hanging-indent-level 2)
;; (setq enh-ruby-hanging-paren-deep-indent-level 0)
;; (setq enh-ruby-hanging-paren-indent-level 2)
;; (setq enh-ruby-indent-level 2)
;; (setq enh-ruby-comment-column 32)
;; (setq enh-ruby-bounce-deep-indent t)

(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook (lambda ()
                                (run-hooks 'prelude-ruby-mode-hook)))
(add-hook 'enh-ruby-mode-hook #'rubocop-mode)


;; (setq shackle-rules '((rspec-compilation-mode :frame t))
;;       shackle-default-rule '(:select t))

;(setq shackle-rules '((rspec-compilation-mode :frame t)))


;; yasnippets

(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)


(add-hook 'prog-mode-hook 'linum-mode)


(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-header-function            #'treemacs--create-header-projectile
        treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil
        treemacs-goto-tag-strategy          'refetch-index)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  :bind
  (:map global-map
        ([f8]        . treemacs-toggle)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)
        ("s-t c"  . treemacs)
        ("s-t p"  . treemacs-projectile)
        ("s-t f"  . treemacs-find-file)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sass-mode elixir-mode pdf-tools robe rvm dired+ wgrep zop-to-char zenburn-theme yasnippet yari yaml-mode which-key web-mode volatile-highlights vimgolf use-package transpose-frame sml-mode smartrep smartparens smart-mode-line slack shackle scss-mode ruby-tools ruby-refactor rubocop rspec-mode rainbow-mode ov org-tree-slide org-bullets operate-on-number multiple-cursors move-text markdown-mode magit key-chord json-mode js2-mode inf-ruby imenu-anywhere helm-spotify-plus helm-projectile helm-open-github helm-descbinds helm-ag haskell-mode haml-mode guru-mode grizzl god-mode gitignore-mode github-browse-file gitconfig-mode git-timemachine gist flycheck fancy-narrow expand-region evil-visualstar evil-tutor evil-surround evil-numbers eshell-git-prompt enh-ruby-mode elm-mode el-get editorconfig easy-kill dumb-jump discover-my-major diff-hl demo-it d-mode csv-mode crux counsel-projectile company color-theme-sanityinc-tomorrow coffee-mode browse-kill-ring beacon anzu all-the-icons ag ace-window 0xc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(setq rspec-use-rvm t)
(setq company-global-modes '(not inf-ruby-mode))
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
(bind-key* "C-c , r" 'rspec-rerun) ; Allow to rerun last spec from non ruby files

(setq special-display-buffer-names '("*rspec-compilation*" "*guard*"))


(add-hook 'enh-ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;  (rvm-activate-corresponding-ruby))


(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))





(setq prelude-user-init-file (expand-file-name "personal/custom.el" user-emacs-directory))

(defun prelude-find-user-init-file (&optional arg)
  "Edit the `prelude-user-init-file', in another window.
With a prefix argument ARG, find the `user-init-file' instead."
  (interactive "P")
  (if arg (find-file-other-window user-init-file)
    (find-file-other-window prelude-user-init-file)))

(bind-key* "C-c I" 'prelude-find-user-init-file)






(defun trike-music-current ()
  "Find out what song is playing."
  (interactive)

  (with-current-buffer (url-retrieve-synchronously
                        "http://music.trikeapps.com/currently_playing_text")
    (let ((song (->> (point-max)
                    (buffer-substring-no-properties url-http-end-of-headers)
                    (s-trim))))
      (kill-buffer)
      (message "Currently playing: %s" song))))

(defun trike-music-next ()
  "Force the office music to the next song, which hopefully sucks less."
  (interactive)
  (with-current-buffer (url-retrieve-synchronously
                        "http://music.trikeapps.com/force_next_track")
    (kill-buffer)
    (message "Song changed. You monster")))

(defun trike-music-stop ()
  "Force the office music to stop."
  (interactive)
  (with-current-buffer (url-retrieve-synchronously
                        "http://music.trikeapps.com/stopmusic")
    (kill-buffer)
    (message "Music stopped. You monster")))

(defun trike-music-restart ()
  "Force the office music to restart."
  (interactive)
  (with-current-buffer (url-retrieve-synchronously
                        "http://music.trikeapps.com/restart_music")
    (kill-buffer)
    (message "Music restarted. You monster")))


(global-set-key (kbd "s-m n") 'trike-music-next)
(global-set-key (kbd "s-m s") 'trike-music-stop)
(global-set-key (kbd "s-m r") 'trike-music-restart)
(global-set-key (kbd "s-m c") 'trike-music-current)

(defun save-buffers-kill-emacs-only-in-console ()
  "In a GUI environment, do nothing; otherwise `save-buffers-kill-emacs`."
  (interactive)
  (if (display-graphic-p)
      (message "save-buffers-kill-emacs disabled for graphical displays.")
    (save-buffers-kill-emacs)))

(defun suspend-frame-only-in-console ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs-only-in-console)
(global-set-key (kbd "C-z") 'suspend-frame-only-in-console)

(setq-default js2-basic-offset 2)

(setq initial-major-mode 'enh-ruby-mode)

(setq initial-scratch-message nil)

(defun reloading (cmd)
  (lambda (x)
    (funcall cmd x)
    (ivy--reset-state ivy-last)))
(defun given-file (cmd prompt) ; needs lexical-binding
  (lambda (source)
    (let ((target
           (let ((enable-recursive-minibuffers t))
             (read-file-name
              (format "%s %s to:" prompt source)))))
      (funcall cmd source target 1))))
(defun confirm-delete-file (x)
  (dired-delete-file x 'confirm-each-subdirectory))

(ivy-add-actions
 'counsel-find-file
 `(("c" ,(given-file #'copy-file "Copy") "copy")
   ("d" ,(reloading #'confirm-delete-file) "delete")
   ("m" ,(reloading (given-file #'rename-file "Move")) "move")))
(ivy-add-actions
 'counsel-projectile-find-file
 `(("c" ,(given-file #'copy-file "Copy") "copy")
   ("d" ,(reloading #'confirm-delete-file) "delete")
   ("m" ,(reloading (given-file #'rename-file "Move")) "move")
   ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))
