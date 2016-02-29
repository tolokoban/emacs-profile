;;; vb6.el --- Modes for VB6 applications            -*- lexical-binding: t; -*-

;; Copyright (C) 2015

;; Author:  Tolokoban
;; Keywords: VB6 modes

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

;;

;;; Code:


(defun vb6-on-file-click (button)
  "Open a file when a button is clicked.
The button's label is the file name relatively to the current buffer path."
  (interactive)
  (let ((path
         (concat
          (file-name-as-directory (file-name-directory buffer-file-name))
          (button-label button))))
    (print path)
    (find-file-other-window path)))



;; ==============================================
;; VBG: Visual Basic Group.
;; ----------------------------------------------
(setq vb6-vbg-mode-highlight
      '(("^[a-zA-Z]+=" . font-lock-function-name-face))
      )

(define-derived-mode vb6-vbg-mode fundamental-mode
  (setq font-lock-defaults '(vb6-vbg-mode-highlight))
  (setq mode-name "vb6-vbg")
  )

(defun x ()
  "Blabla..."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(StartupProject\\|Project\\)=" nil t)      
      (set-mark-command)
      (move-end-of-line)
      (make-button (mark) (point)
                   'action 'vb6-on-file-click))))

;; ========================================
;; Code
;; ----------------------------------------
(setq vb6-code-mode-highlight
      '(        ("'.*$" . font-lock-comment-face)
("\\<\\(Abs\\|Access\\|AddItem\\|AddNew\\|Alias\\|And\\|Any\\|App\\|AppActivate\\|Append\\|AppendChunk\\|Arrange\\|As\\|Asc\\|Atn\\|Base\\|Beep\\|BeginTrans\\|Binary\\|ByVal\\|Call\\|Case\\|CCur\\|CDbl\\|ChDir\\|ChDrive\\|Chr\\$?\\|CInt\\|Circle\\|Clear\\|Clipboard\\|CLng\\|Close\\|Cls\\|Command\\$?\\|CommitTrans\\|Compare\\|Const\\|Control\\|Controls\\|Cos\\|CreateDynaset\\|CSng\\|CStr\\|CurDir\\$\\|Currency\\|CVar\\|CVDate\\|Data\\|Date\\$?\\|DateSerial\\|DateValue\\|Day\\|Debug\\|Declare\\|DefCur\\|CefDbl\\|DefInt\\|DefLng\\|DefSng\\|DefStr\\|DefVar\\|Delete\\|Dim\\|Dir\\$?\\|Do\\|DoEvents\\|Double\\|Drag\\|Dynaset\\|Edit\\|Else\\|ElseIf\\|End\\|EndDoc\\|EndIf\\|Environ\\$\\|EOF\\|Eqv\\|Erase\\|Erl\\|Err\\|Error\\$?\\|ExecuteSQL\\|Exit\\|Exp\\|Explicit\\|False\\|FieldSize\\|FileAttr\\|FileCopy\\|FileDateTime\\|FileLen\\|Fix\\|For\\|Form\\|Format\\$?\\|Forms\\|FreeFile\\|Function\\|Get\\|GetAttr\\|GetChunk\\|GetData\\|DetFormat\\|GetText\\|Global\\|GoSub\\|GoTo\\|Hex\\$?\\|Hide\\|Hour\\|If\\|Imp\\|Input\\$?\\|InputBox\\$?\\|InStr\\|Int\\|Integer\\|Is\\|IsDate\\|IsEmpty\\|IsNull\\|IsNumeric\\|Kill\\|LBound\\|LCase\\$?\\|Left\\$?\\|Len\\|Let\\|Lib\\|Like\\|Line\\|LinkExecute\\|LinkPoke\\|LinkRequest\\|LinkSend\\|Load\\|LoadPicture\\|Loc\\|Local\\|Lock\\|LOF\\|Log\\|Long\\|Loop\\|LSet\\|LTrim\\$?\\|Me\\|Mid\\$?\\|Minute\\|MkDir\\|Mod\\|Month\\|Move\\|MoveFirst\\|MoveLast\\|MoveNext\\|MovePrevious\\|MoveRelative\\|MsgBox\\|Name\\|New\\|NewPage\\|Next\\|NextBlock\\|Not\\|Nothing\\|Now\\|Null\\|Oct\\$?\\|On\\|Open\\|OpenDataBase\\|Option\\|Or\\|Output\\|Point\\|Preserve\\|Print\\|Printer\\|PrintForm\\|Private\\|PSet\\|Public\\|Put\\|QBColor\\|Random\\|Randomize\\|Read\\|ReDim\\|Refresh\\|RegisterDataBase\\|Rem\\|RemoveItem\\|Reset\\|Restore\\|Resume\\|Return\\|RGB\\|Right\\$?\\|RmDir\\|Rnd\\|Rollback\\|RSet\\|RTrim\\$?\\|SavePicture\\|Scale\\|Second\\|Seek\\|Select\\|SendKeys\\|Set\\|SetAttr\\|SetData\\|SetFocus\\|SetText\\|Sgn\\|Shared\\|Shell\\|Show\\|Sin\\|Single\\|Space\\$?\\|Spc\\|Sqr\\|Static\\|Step\\|Stop\\|Str\\$?\\|StrComp\\|String\\$?\\|Sub\\|System\\|Tab\\|Tan\\|Text\\|TextHeight\\|TextWidth\\|Then\\|Time\\$?\\|Timer\\|TimeSerial\\|TimeValue\\|To\\|Trim\\$?\\|True\\|Type\\|TypeOf\\|UBound\\|UCase\\$?\\|Unload\\|Unlock\\|Until\\|Update\\|Using\\|Val\\|Variant\\|VarType\\|Weekday\\|Wend\\|While\\|Width\\|Write\\|Xor\\|Year\\|ZOrder
\\)\\>" . font-lock-function-name-face)
        ("\\\"[^\\\"]*\\\"" . font-lock-string-face)
        ("[!&|,=]+" . font-lock-keyword-face)
        ("[\\*\\+\\?]" . font-lock-type-face)
        )
      )

(define-derived-mode vb6-code-mode fundamental-mode
  (setq font-lock-defaults '(vb6-code-mode-highlight))
  (setq mode-name "vb6-code")
  )








(provide 'vb6)
;;; vb6.el ends here
