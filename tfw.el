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
  (progn (find-file (concat (file-name-sans-extension (buffer-file-name)) ".css")) ) )

(defun tfw-switch-js ()
  "Switch a TFW module to the JS part."
  (interactive)
  (progn (find-file (concat (file-name-sans-extension (buffer-file-name)) ".js")) ) )

(defun tfw-switch-ini ()
  "Switch a TFW module to the INI part."
  (interactive)
  (progn (find-file (concat (file-name-sans-extension (buffer-file-name)) ".ini")) ) )

(defun tfw-switch-dep ()
  "Switch a TFW module to the DEP part."
  (interactive)
  (progn (find-file (concat (file-name-sans-extension (buffer-file-name)) ".dep")) ) )

(global-set-key (kbd "C-S-t c" ) 'tfw-switch-css)
(global-set-key (kbd "C-S-t j" ) 'tfw-switch-js)
(global-set-key (kbd "C-S-t i" ) 'tfw-switch-ini)
(global-set-key (kbd "C-S-t d" ) 'tfw-switch-dep)

(provide 'tfw)
;;; tfw.el ends here
