;;; Pour utiliser ce fichier, ajouter ceci dans votre fichier ".emacs" :
;;;    (setq root "~/Code/tolokoban/")
;;;    (load-file (concat root "emacs/emacs-init.el"))
;;; ---------------------------------------------------------------------

;; =====
;; MELPA
;; -----
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; ========================
;; Install needed packages.
;; ------------------------
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'use-package
                          'glsl-mode
                          'dash
			  'yasnippet
			  'json-mode
			  'js2-mode
			  'dired-narrow
			  'dired-subtree
                          'diredful
                          'multiple-cursors)

(require 'use-package)

;; Full screen.
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Corriger le problème des accents circonflexes.
(load-library "iso-transl")


;; =====
;; Dired
;; -----
;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)
                                        ; Adding colors by extension.
(use-package diredful
  :config (diredful-mode 1))
;; allow editing file permissions
(setq wdired-allow-to-change-permissions t)
;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))
(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("k" . dired-subtree-remove)
              )
  )


;; ==================================
;; IDO mode : Interactively Do Things
;; Aide  pour  switcher  entre  les  buffers  et  pour  rechercher  des
;; fichiers.
;; ----------------------------------
(require 'ido)
(ido-mode t)


;; ==============================================
;; Mode pour visualiser les fichiers de grammaire
;; ----------------------------------------------
(setq grammarKeywords
      '(("^\\@\\(leaf\\|node\\|blackhole\\|skip\\|test\\)" . font-lock-function-name-face)
        ("([^)]+)" . font-lock-constant-face)
        ("^\\(#\\|//\\).*$" . font-lock-comment-face)
        ("[!&|,=]+" . font-lock-keyword-face)
        ("[\\*\\+\\?]" . font-lock-type-face)
        )
      )

(define-derived-mode grammar-mode fundamental-mode
  (setq font-lock-defaults '(grammarKeywords))
  (setq mode-name "grammar")
  )


;; ================================
;;  L'UTF-8 est le coding prÃ©fÃ©rÃ©.
;; --------------------------------
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
                                        ;(setq coding-system-for-read 'utf-8)
                                        ;(setq coding-system-for-write 'utf-8)

;; ============================================================================
;;  Permettre de reconnaÃ®tre automatiquement les fichiers UTF-16 de Windows 7.
;; ----------------------------------------------------------------------------
;; Detect endianness of UTF-16 containing a Byte Order Mark U+FEFF
;; Detect EOL mode by looking for CR/LF on the first line
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE.*\x0D\x00$" . utf-16-le-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF.*\x0D\x00$" . utf-16-be-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)


;; Cleaning indentation of a buffer.
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (iwb)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (delete-trailing-blank-lines))
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Donne des noms de buffers plus explicites que les "toto<2>", "toto<3>", ...
(require 'uniquify)

;; org-mode
(setq org-agenda-files (list (concat "~/todo.org")))
(setq org-todo-keywords '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "CANCELED")))
(setq org-log-done 'time)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Calendrier
(setq calendar-latitude 46)
(setq calendar-longitude 6)
(setq calendar-location-name "Geneva")
(setq diary-file (concat root "diary"))
(appt-activate t)
(setq appt-message-warning-time 10)
(setq appt-display-mode-line t)
(setq appt-audible t)


;; Remplace les tabulations par des espaces lors de l'indentation.
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Permet de colorier des portions de text avec des expressions rÃ©guliÃ¨res.
(global-hi-lock-mode 1)

;; Gestion des groupes de fichiers.
(filesets-init)


;; ============================================
;;  Définir le path pour les elisp de la clef.
;; --------------------------------------------
(add-to-list 'load-path
             (concat root "elisp"))

;; ===============
;;  Markdown mode
;; ---------------
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))


;; Usefull function to get the CamelCase version if an identifier.
(defun camelize (s)
  "Transform 'tfw.web-service' into 'WebService'."
  (mapconcat 'identity (mapcar 'capitalize (split-string
                                            (car (last (split-string s "\\." ) ) )
                                            "-"
                                            )) ""))
;; ===========
;;  js2-mode
;; -----------
;;; (require 'js2-mode (concat root "elisp/js2-mode.el"))
(require 'js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js.htm$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jsn$" . json-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; ===========
;;  GLSL mode
;; -----------
(add-to-list 'auto-mode-alist '("\\.vert$" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag$" . glsl-mode))

;; ==============================================
;;  web-mode: https://github.com/fxbois/web-mode
;; ----------------------------------------------
(autoload 'web-mode "web-mode" nil t)
(require 'web-mode (concat root "elisp/web-mode.el"))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
(set-face-attribute 'web-mode-html-tag-face nil :foreground "Blue")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "DarkOrange")
(set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "Black")

;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
    ad-do-it))
(ad-activate 'grep-compute-defaults)
(global-set-key [f8] 'occur)
(global-set-key [(shift f8)] 'grep-find)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 '(iswitchb-mode t)
 '(js2-highlight-level 3)
 '(make-pointer-invisible t)
 '(save-place t nil (saveplace))
 '(show-trailing-whitespace nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "#909090"))))
 '(font-lock-doc-face ((t (:foreground "#707070" :slant italic))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "#006000"))))
 '(js2-error-face ((((class color) (background light)) (:foreground "red" :underline "red")))))

;; Ne pas afficher les numÃ©ros de ligne. par dÃ©faut pour Ã©viter les problÃ¨mes
;; avec les gros fichiers (de log, par exemple).
(global-linum-mode 0)

;; Quand on tape une touche, cela supprime le texte sÃ©lectionnÃ©.
(delete-selection-mode 1)

;; Utilisation du dictionnaire pour correction syntaxique.
(setq-default ispell-program-name "aspell")
(setq-default ispell-extra-args '("--reverse"))


;; ================================================
;; Graphviz mode
;; ------------------------------------------------
(load-file (concat root "elisp/graphviz-dot-mode.el"))

;; ================================================
;;  Visual Basic mode
;; ------------------------------------------------
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|ctl\\)$" .
                                 visual-basic-mode)) auto-mode-alist))
;; ================================================

;; ================================================
;;  C# mode
;; ------------------------------------------------
(autoload 'csharp-mode "csharp-mode" "C# mode." t)
(setq auto-mode-alist (append '(("\\.cs$" .
                                 csharp-mode)) auto-mode-alist))
;; ================================================


;; ========
;;  Octave
;; --------
(setq auto-mode-alist (append '(("\\.m$" . octave-mode)) auto-mode-alist))



;; EmpÃªche Dired d'ouvrir un nouveau buffer Ã  chaque visite d'un nouveau folder.
(put 'dired-find-alternate-file 'disabled nil)


;; ===========
;;  YAML mode
;; -----------
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;; =====================================
;;  Python mode
;; -------------------------------------
(setq py-install-directory (concat root "python-mode.el-6.0.12"))
(add-to-list 'load-path py-install-directory)
(require 'python-mode)
(setq py-load-pymacs-p t)
;; (require 'auto-complete-config)
;; (ac-config-default)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-hook 'python-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")))


;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                                          ("%b - Dir:  " default-directory)))))))

;; I like to know what time it is. These lines show the clock in the
;; status bar. Comment out first line if you prefer to show time in 12
;; hour formatx
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; show column number in status bar
(setq column-number-mode t)


(defun tolokoban-lines-toggle () (interactive) (global-linum-mode))
(global-set-key [C-f4] 'tolokoban-lines-toggle)


(global-set-key "\M-s" 'dabbrev-expand)
(global-set-key [M-kp-divide] 'dabbrev-expand)
(global-set-key [C-tab] 'other-window)
(global-set-key "\M-g" 'goto-line)

;; Restauration de la session prÃ©cÃ©dente (mettre 1 pour l'activer)
(desktop-save-mode 0)

(server-start)



;; ===========
;;  Templates
;; -----------
(require 'autoinsert)

(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(setq skeleton-pair t)

(add-hook 'find-file-hook 'auto-insert)


(define-skeleton tpl-tex ""
  "\n%\n"
  "\\documentclass[a4paper,12pt,twoside]{article}\n"
  "\\usepackage[margin=25mm]{geometry}\n"
  "\\usepackage[francais]{babel}\n"
  "\\usepackage[utf8]{inputenc}\n"
  "\\usepackage[T1]{fontenc}\n"
  "\\usepackage{listings}\n"
  "\\usepackage{ae,aeguill}\n"
  "\\usepackage{pst-all}\n"
  "\\usepackage{pstricks-add}\n"
  "\\usepackage{multido}\n"
  "\n"
  "% RÃ©glages pour l'affichage du code Python\n"
  "\\lstset{language=Python, frame=lines,\n"
  "        frameround=tttt, inputencoding=utf8}\n"
  "\\lstset{basicstyle=\\scriptsize}\n"
  "\\lstset{commentstyle=\\color{gray}}\n"
  "\\lstset{numbers=left}\n"
  "\\lstset{numberstyle=\\tiny\\color{blue}}\n"
  "\n"
  "\\setlength{\\parindent}{1cm}\n"
  "\\setlength{\\parskip}{4mm}\n"
  "\n"
  "\\begin{document}\n"
  "\\title{" "_" "}\n"
  "\\author{Tolokoban}\n"
  "\\maketitle\n"
  "\n"
  "\\begin{abstract}\n"
  "Le rÃ©sumÃ©\n"
  "\\end{abstract}\n"
  "\n"
  "\\section{Premier paragraphe}\n"
  "\n\n\n"
  "\\end{document}\n"
  )
(define-auto-insert "\(\\.tex$\)\|\(\\.pytex$\)" 'tpl-tex)


(define-skeleton tpl-python ""
  "\n"
  "# -*- coding: utf-8 -*-\n\n"
  "import pdb\n"
  "import sys\n"
  "\n"
  "def exceptionHandler(type, value, tb):\n"
  "   if hasattr(sys, 'ps1') or not sys.stderr.isatty() or type == SyntaxError:\n"
  "      # we are in interactive mode or we don't have a tty-like\n"
  "      # device, so we call the default hook\n"
  "      sys.__excepthook__(type, value, tb)\n"
  "   else:\n"
  "      import traceback\n"
  "      # we are NOT in interactive mode, print the exception...\n"
  "      traceback.print_exception(type, value, tb)\n"
  "      print\n"
  "      # ...then start the debugger in post-mortem mode.\n"
  "      pdb.pm()\n"
  "\n"
  "sys.excepthook = exceptionHandler\n"
  "\n"
  "\n"
  "\n"
  "\n"
  )
(define-auto-insert "\\.py$" 'tpl-python)



;; ======================================
;;  Mode pour fichier batch sous windows
;; --------------------------------------
(defvar bat-mode-keywords-list
  '("set" "if" "echo" "off" "exit" "goto" "pause"))
(setq bat-mode-keywords-regexp (regexp-opt bat-mode-keywords-list 'words))
(setq bat-mode-keywords
      '(
        ("^[Rr][Ee][Mm].*$" . font-lock-comment-face)
        ("%[^%]+%" . font-lock-constant-face)
        (bat-mode-keywords-regexp . font-lock-function-name-face)
        )
      )

(define-derived-mode bat-mode fundamental-mode
  (setq font-lock-keywords-case-fold-search t)
  (setq font-lock-defaults '(bat-mode-keywords))
  (setq mode-name "windows batch files mode")
  )
(setq auto-mode-alist (cons '("\\.\\(bat\\|cmd\\)$" . bat-mode) auto-mode-alist))


;; =================================
;;  Mode pour fichier ini pour TFW.
;; ---------------------------------
(setq ini-mode-keywords
      '(
        ("^[ \t]*\\[[a-z]+\\][ \t]*$" . font-lock-function-name-face)
        ("^[ \t]*[^0-9a-z_-\\[].*$" . font-lock-comment-face)
        ("^[ \t]*[a-zA-Z_][a-zA-Z0-9_\\.\\-]*:" . font-lock-variable-name-face)
        ("\\$[0-9]+" . font-lock-keyword-face)
        )
      )

(define-derived-mode ini-mode fundamental-mode
  (setq font-lock-keywords-case-fold-search t)
  (setq font-lock-defaults '(ini-mode-keywords))
  (setq mode-name "internationalization for TFW")
  )
(setq auto-mode-alist (cons '("\\.\\(ini\\)$" . ini-mode) auto-mode-alist))


;; =============================
;;  Mode pour fichiers log LODH
;; -----------------------------
(defvar log-mode-font-lock-defaults
  '(
    ;;  (auto-revert-tail-mode 1)
    ("^[0-9]+ [0-9:\\.]+" . font-lock-comment-face)
    ("\\*\\*\\* ERROR " . font-lock-warning-face)
    ("\\*\\*\\* WARNING " . font-lock-warning-face)
    (" \\[[a-zA-Z0-9 ]+\\] " . font-lock-constant-face)
    ("\"[^\"\\n\\r]+\"" . hi-yellow);;font-lock-string-face)
    ("\\(\\(  ==> \\)\\|\\(  <== \\)\\)[a-zA-Z_0-9\\.:]+" . font-lock-preprocessor-face)
    )
  )
(define-derived-mode log-mode fundamental-mode
  (setq font-lock-defaults '(log-mode-font-lock-defaults))
  ;; Supprimer la coloration par dÃ©faut des chaÃ®nes de caractÃ¨re.
  (setq font-lock-syntax-table nil)
  (setq mode-name "LODH log files mode")
  )
(setq auto-mode-alist (cons '("\\.log$" . log-mode) auto-mode-alist))



;; Compilation LaTeX en une seule touche : F12
(defun tolokoban-latex-compile-as-dvi-and-view-as-pdf ()
  "Compiler du Tex en PDF puis afficher le resultat.

Avec le buffer courant effectuer la serie de transformations
suivante :
tex -> dvi -> ps -> pdf
Puis affichage avec evince."
  (interactive)
  (let (f tex dvi ps pdf cmd)
    (setq tex (replace-regexp-in-string "\\([^/]*/\\)*" "" (buffer-file-name)))
    (setq cmd "")
    (if (string= ".pytex" (substring tex -6))
        (progn
          (setq cmd
                (concat
                 "python ../PyTeX/pytex.py "
                 buffer-file-name
                 " && "
                 "python " (substring tex 0 -6) "_.py && "))
          (setq f (substring tex 0 -5)))
      (setq f (substring tex 0 -3))
      )
    (setq tex (concat f "tex"))
    (setq idx (concat f "idx"))
    (setq dvi (concat f "dvi"))
    (setq ps (concat f "ps"))
    (setq pdf (concat f "pdf"))
    (setq cmd (concat
               cmd
               "latex " tex " "
               ;;               "&& makeindex " idx " "
               ;;               "&& latex " tex " "
               "&& dvips " dvi " "
               "&& ps2pdf " ps " "
               "&& start " pdf))
    (shell-command cmd)))
(global-set-key (kbd "<f12>") 'tolokoban-latex-compile-as-dvi-and-view-as-pdf)


;; ======================================
;;  DÃ©finition de Toloframework sur F11.
;; --------------------------------------
(defun tfw ()
  "Toloframework compiler.

Extrait la documentation et un fichier compressÃ© d'un script javascript Ã
l'extension <*.js>."
  (interactive)
  (progn
    (save-buffer)
    (shell-command (concat "python D:/Code/publicator/prj/ToloFrameWork/tfw.py "
                           buffer-file-name))
    ;;                           " D:/Code/www/TFW/@"))
    )
  )
(global-set-key (kbd "<f11>") 'tfw)

;; ====================================
;; Permet d'indenter un fichier entier.
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "<f10>") 'iwb)


;; Compare Windows side by side.
(global-set-key (kbd "<f9>") 'compare-windows)


(defun bf-pretty-print-xml-region ()
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  ;;  (interactive "r")
  (interactive)
  (let (begin end)
    (setq begin (point-min))
    (setq end (point-max))
    (save-excursion
      (mark-whole-buffer)
      (nxml-mode)
      (beginning-of-buffer)
      ;;    (mark-whole-buffer)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char)
        (insert "\n"))
      (iwb))
    (message "XML is now in pretty print format!")
    (beginning-of-buffer)))
(global-set-key [(shift f10)] 'bf-pretty-print-xml-region)


;; ============
;;  Mode LaTeX
;; ------------
(add-to-list 'auto-mode-alist '("tex$" . latex-mode))

;; ================
;;  Mode pour HTML
;; ----------------
(defun my-html-mode-hook ()
  (setq tab-width 2)
  (setq indent-tabs-mode t)
  (define-key html-mode-map (kbd "C->") 'sgml-close-tag))

(add-hook 'html-mode-hook 'my-html-mode-hook)

(put 'narrow-to-region 'disabled nil)



(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

(defun utf16 ()
  "Close current buffer and reopen it in with UTF-16 encoding."
  (interactive)

  (progn
    (setq path (buffer-file-name))
    (if path
        (progn
          (kill-buffer)
          (universal-coding-system-argument utf-16)
          (find-file path)
          )
      )
    )
  )

;; =========================
;;  Agencement des fenÃªtres
;; -------------------------
(delete-other-windows)
(find-file (concat "~/todo.org"))
(split-window-right)
(find-file (concat root "emacs-init.el"))


;; Open Recent File Menu.
(require 'recentf)
(recentf-mode 1)

;; No start-up screen.
(setq inhibit-startup-screen t)

;; Multiple Cursors.
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ))

;; Bookmarks.
(use-package bm
  :bind (("<C-f2>" . bm-toggle)
         ("<f2>" . bm-next)
         ("<S-f2>" . bm-previous)))

;; Markdown mode.
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; ===========
;;  YASnippet
;; -----------
(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory (concat root "yas/snippets"))


;; ToloFrameWork utilities.
(load-file (concat root "tfw.el"))
