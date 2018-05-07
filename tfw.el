;;; tfw.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  Tolokoban
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tools to ease the work with ToloFrameWork

;;; Code:


(defun tfw-switch-css ()
  "Switch a TFW module to the CSS part."
  (interactive)
  (let ((fname (concat (file-name-sans-extension (buffer-file-name)) ".css")))
    (other-window 1)
    (find-file fname) ) )

(defun tfw-switch-js ()
  "Switch a TFW module to the JS part."
  (interactive)
  (let ((fname (concat (file-name-sans-extension (buffer-file-name)) ".js")))
    (other-window 1)
    (find-file fname) ) )

(defun tfw-switch-xjs ()
  "Switch a TFW module to the XJS part."
  (interactive)
  (let ((fname (concat (file-name-sans-extension (buffer-file-name)) ".xjs")))
    (other-window 1)
    (find-file fname) ) )

(defun tfw-switch-ini ()
  "Switch a TFW module to the INI part."
  (interactive)
  (let ((fname (concat (file-name-sans-extension (buffer-file-name)) ".ini")))
    (other-window 1)
    (find-file fname) ) )

(defun tfw-switch-dep ()
  "Switch a TFW module to the DEP part."
  (interactive)
  (let ((fname (concat (file-name-sans-extension (buffer-file-name)) ".dep")))
    (other-window 1)
    (find-file fname) ) )

(defun tfw-switch-vert ()
  "Switch a TFW module to the VERT part."
  (interactive)
  (let ((fname (concat (file-name-sans-extension (buffer-file-name)) ".vert")))
    (other-window 1)
    (find-file fname) ) )

(defun tfw-switch-frag ()
  "Switch a TFW module to the FRAG part."
  (interactive)
  (let ((fname (concat (file-name-sans-extension (buffer-file-name)) ".frag")))
    (other-window 1)
    (find-file fname) ) )

(global-set-key (kbd "C-S-t c" ) 'tfw-switch-css)
(global-set-key (kbd "C-S-t j" ) 'tfw-switch-js)
(global-set-key (kbd "C-S-t i" ) 'tfw-switch-ini)
(global-set-key (kbd "C-S-t d" ) 'tfw-switch-dep)
(global-set-key (kbd "C-S-t v" ) 'tfw-switch-vert)
(global-set-key (kbd "C-S-t f" ) 'tfw-switch-frag)
(global-set-key (kbd "C-S-t x" ) 'tfw-switch-xjs)


(defun toloframework-main-menu ()
  "This is the ToloFrameWork main menu."
  (interactive)
  (let* (
         (current (current-buffer))
         (basename (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
         (noext (file-name-sans-extension (buffer-file-name)))
         (new (get-buffer-create "*ToloFrameWork*")) )

    (set-buffer new)
    (other-window 1)
    (switch-to-buffer new)
    (erase-buffer)
    (text-mode)

    (setq extensions '("css" "js" "ini" "dep" "vert" "frag"))
    (dolist (ext extensions)
      (insert (propertize
               (concat "[" (substring ext 0 1) "]")
               'font-lock-face '(:foreground "blue" :weight bold)))
      (insert (propertize (concat " " basename) 'font-lock-face 'font-lock-comment-face))
      (insert (concat "." ext))
      (insert "\n")
      )

    (insert "\n\n")
    (insert (propertize (concat "[q]") 'font-lock-face '(:foreground "blue" :weight bold)))
    (insert " Back to "
            (propertize (buffer-name current) 'font-lock-face '(:foreground "#080")))
    ;; Private functions
    (defun tfw-local-switch-css ()
      "Switch a TFW module to the CSS part."
      (interactive)
      (let ()
        (kill-buffer)
        (find-file (concat noext ".css"))))
    (defun tfw-local-switch-js ()
      "Switch a TFW module to the JS part."
      (interactive)
      (let ()
        (kill-buffer)
        (find-file (concat noext ".js"))))
    (defun tfw-local-switch-ini ()
      "Switch a TFW module to the INI part."
      (interactive)
      (let ()
        (kill-buffer)
        (find-file (concat noext ".ini"))))
    (defun tfw-local-switch-dep ()
      "Switch a TFW module to the DEP part."
      (interactive)
      (let ()
        (kill-buffer)
        (find-file (concat noext ".dep"))))
    (defun tfw-local-switch-xjs ()
      "Switch a TFW module to the XJS part."
      (interactive)
      (let ()
        (kill-buffer)
        (find-file (concat noext ".xjs"))))
    (defun tfw-local-switch-vert ()
      "Switch a TFW module to the VERT part."
      (interactive)
      (let ()
        (kill-buffer)
        (find-file (concat noext ".vert"))))
    (defun tfw-local-switch-frag ()
      "Switch a TFW module to the FRAG part."
      (interactive)
      (let ()
        (kill-buffer)
        (find-file (concat noext ".frag"))))

    ;; Hook the keys.
    (local-set-key "c" 'tfw-local-switch-css)
    (local-set-key "j" 'tfw-local-switch-js)
    (local-set-key "i" 'tfw-local-switch-ini)
    (local-set-key "d" 'tfw-local-switch-dep)
    (local-set-key "v" 'tfw-local-switch-vert)
    (local-set-key "f" 'tfw-local-switch-frag)
    (local-set-key "x" 'tfw-local-switch-xjs)

    (local-set-key "q" 'kill-buffer)
    ))

(global-set-key (kbd "<f12>" ) 'toloframework-main-menu)


(provide 'tfw)
;;; tfw.el ends here


;; Syntax highlighting for XJS files.
(setq xjs-highlights
      '((",\\|//[^\n\r]*[\n\r]+" . font-lock-comment-face)
        ("/\\*([^\\*]+|\\*[^/])*\\*/" . font-lock-comment-face)
        ("%[a-zA-Z]+%" . font-lock-preprocessor-face)
        ("[^ \n\r\t{},'\":]+[:space:]*:" . font-lock-variable-name-face)
        ("[a-z]+\\.[^ \n\r\t{},'\":]+[:space:]*:" . font-lock-function-name-face)
        ("{[:space:]*[a-zA-Z]*?[a-z][a-zA-Z]*[ \n\r]" . font-lock-constant-face)
        ("{[:space:]*[A-Z]+}" . font-lock-type-face)
        ("{[:space:]*[A-Z]+" . font-lock-type-face)
        ("[{}]" . font-lock-constant-face)
        ("[\\[]]" . font-lock-builtin-face)))
(define-derived-mode xjs-mode fundamental-mode "xjs"
  "Major mode for XJS code used in the Toloframework."
  (make-local-variable 'xjs-indent-offset)
  (set (make-local-variable 'xjs-indent-function) 'xjs-indent-line)
  (setq font-lock-defaults '(xjs-highlights)))
(add-to-list 'auto-mode-alist '("\\.xjs$" . xjs-mode))

(defvar xjs-indent-line 2 "Indentation offset for `xjs-mode'.")
(defun xjs-indent-function ()
  "Indent current line for `xjs-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "[[{]")
              (setq indent-col (+ indent-col xjs-indent-offset))
              (message (concat "indent-col: " indent-col))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[]}]") (>= indent-col xjs-indent-offset))
        (setq indent-col (- indent-col xjs-indent-offset))))
    (indent-line-to indent-col)))
(provide 'xjs-mode)
