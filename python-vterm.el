;;; python-vterm.el --- A mode for Python REPL using vterm -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Shigeaki Nishina

;; Author: Shigeaki Nishina
;; Maintainer: Shigeaki Nishina
;; Created: March 11, 2020
;; URL: https://github.com/shg/python-vterm.el
;; Package-Requires: ((emacs "25.1") (vterm "0.0.1"))
;; Version: 0.24
;; Keywords: languages, python

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Provides a major-mode for inferior Python process that runs in vterm, and a
;; minor-mode that extends python-mode to support interaction with the inferior
;; Python process.

;;; Usage:

;; You must have python-mode and vterm installed.
;; Install python-vterm.el manually using package.el
;;
;;   (package-install-file "/path-to-download-dir/python-vterm.el")
;;
;; Eval the following line. Add this line to your init file to enable this
;; mode in future sessions.
;;
;;   (add-hook 'python-mode-hook #'python-vterm-mode)
;;
;; Now you can interact with an inferior Python REPL from a Python buffer.
;;
;; C-c C-z in a python-mode buffer to open an inferior Python REPL buffer.
;; C-c C-z in the REPL buffer to switch back to the script buffer.
;; C-<return> in the script buffer to send region or current line to REPL.
;;
;; See the code below for a few more key bidindings.

;;; Code:

(require 'vterm)
(require 'rx)


;;----------------------------------------------------------------------
(defgroup python-vterm-repl nil
  "A major mode for inferior Python REPL."
  :group 'python)

(defvar-local python-vterm-repl-program "python"
  "Name of the command for executing Python code.
Maybe either a command in the path, like python
or an absolute path name, like /usr/local/bin/python
parameters may be used, like python -q")

(defvar-local python-vterm-repl-script-buffer nil)

(defvar python-vterm-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'python-vterm-repl-switch-to-script-buffer)
    (define-key map (kbd "M-k") #'python-vterm-repl-clear-buffer)
    (define-key map (kbd "C-c C-t") #'python-vterm-repl-copy-mode)
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    map))

(define-derived-mode python-vterm-repl-mode vterm-mode "Inf-Python"
  "A major mode for inferior Python REPL."
  :group 'python-vterm-repl)

(defun python-vterm-repl-buffer-name (&optional session-name)
  "Return a Python REPL buffer name whose session name is SESSION-NAME.
If SESSION-NAME is not given, the default session name `main' is assumed."
  (format "*python:%s*" (or session-name "main")))

(defun python-vterm-repl-session-name (repl-buffer)
  "Return the session name of REPL-BUFFER."
  (let ((bn (buffer-name repl-buffer)))
    (if (string= (substring bn 1 7) "python:")
	(substring bn 7 -1)
      nil)))

(defun python-vterm-repl-list-sessions ()
  "Return a list of existing Python REPL sessions."
  (mapcan (lambda (bn)
	    (if (string-match "\\*python:\\(.*\\)\\*" bn)
		(list (match-string 1 bn))
	      nil))
	  (mapcar #'buffer-name (buffer-list))))

(defun python-vterm-repl-buffer (&optional session-name restart)
  "Return an inferior Python REPL buffer of the session name SESSION-NAME.
If there exists no such buffer, one is created and returned.
With non-nil RESTART, the existing buffer will be killed and
recreated."
  (let ((ses-name (or session-name "main")))
    (if-let ((buffer (get-buffer (python-vterm-repl-buffer-name ses-name)))
	     (alive (vterm-check-proc buffer))
	     (no-restart (not restart)))
	buffer
      (if (get-buffer-process buffer) (delete-process buffer))
      (if buffer (kill-buffer buffer))
      (let ((buffer (generate-new-buffer (python-vterm-repl-buffer-name ses-name)))
	    (vterm-shell python-vterm-repl-program))
	(with-current-buffer buffer
	  (python-vterm-repl-mode)
	  (add-function :filter-args (process-filter vterm--process)
			(python-vterm-repl-run-filter-functions-func ses-name)))
	buffer))))

(defun python-vterm-repl (&optional arg)
  "Create an inferior Python REPL buffer and open it.
The buffer name will be `*python:main*' where `main' is the default session name.
With prefix ARG, prompt for a session name.
If there's already an alive REPL buffer for the session, it will be opened."
  (interactive "P")
  (let* ((session-name
	  (cond ((null arg) nil)
		(t (completing-read "Session name: " (python-vterm-repl-list-sessions) nil nil nil nil
				    (python-vterm-repl-session-name (python-vterm-fellow-repl-buffer))))))
	 (orig-buffer (current-buffer))
	 (repl-buffer (python-vterm-repl-buffer session-name)))
    (if (and (boundp 'python-vterm-mode) python-vterm-mode)
	(with-current-buffer repl-buffer
	  (setq python-vterm-repl-script-buffer orig-buffer)))
    (pop-to-buffer-same-window repl-buffer)))

(defun python-vterm-repl-switch-to-script-buffer ()
  "Switch to the script buffer that is paired with this Python REPL buffer."
  (interactive)
  (let ((repl-buffer (current-buffer))
	(script-buffer (if (buffer-live-p python-vterm-repl-script-buffer)
			   python-vterm-repl-script-buffer
			 nil)))
    (if script-buffer
	(with-current-buffer script-buffer
	  (setq python-vterm-fellow-repl-buffer repl-buffer)
	  (switch-to-buffer-other-window script-buffer)))))

(defun python-vterm-repl-clear-buffer ()
  "Clear the content of the Python REPL buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (vterm-clear 1)))

(defvar-local python-vterm-repl-filter-functions '()
  "List of filter functions that process the output to the REPL buffer.")

(defun python-vterm-repl-run-filter-functions-func (session)
  "Return a function that runs registered filter functions for SESSION with args."
  (lambda (args)
    (with-current-buffer (python-vterm-repl-buffer session)
      (let ((proc (car args))
	    (str (cadr args)))
	(let ((funcs python-vterm-repl-filter-functions))
	  (while funcs
	    (setq str (apply (pop funcs) (list str))))
	  (list proc str))))))

(defun python-vterm-repl-buffer-status ()
  "Check and return the prompt status of the REPL.
Return a corresponding symbol or nil if not ready for input."
  (let* ((bs (buffer-string))
	 (tail (substring bs (- (min 256 (length bs))))))
    (set-text-properties 0 (length tail) nil tail)
    (let* ((lines (split-string (string-trim-right tail "[\t\n\r]+")
				(char-to-string ?\n)))
	   (prompt (car (last lines))))
      (pcase prompt
	((rx bol "python> " eol) :python)
	((rx bol "In [" (one-or-more (any "0-9")) "]: " eol) :python)
	((rx bol "help?> " eol) :help)
	((rx bol "(" (+? any) ") pkg> " eol) :pkg)
	((rx bol "shell> " eol) :shell)))))

(defvar python-vterm-repl-copy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'python-vterm-repl-copy-mode)
    (define-key map [return] #'python-vterm-repl-copy-mode-done)
    (define-key map (kbd "RET") #'python-vterm-repl-copy-mode-done)
    (define-key map (kbd "C-c C-r") #'vterm-reset-cursor-point)
    map))

(define-minor-mode python-vterm-repl-copy-mode
  "Toggle copy mode."
  :group 'python-vterm-repl
  :lighter " VTermCopy"
  :keymap python-vterm-repl-copy-mode-map
  (if python-vterm-repl-copy-mode
      (progn
	(message "Start copy mode")
	(use-local-map nil)
	(vterm-send-stop))
    (vterm-reset-cursor-point)
    (use-local-map python-vterm-repl-mode-map)
    (vterm-send-start)
    (message "End copy mode")))

(defun python-vterm-repl-copy-mode-done ()
  "Save the active region to the kill ring and exit copy mode."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (user-error "No active region"))
  (python-vterm-repl-copy-mode -1))


;;----------------------------------------------------------------------
(defgroup python-vterm nil
  "A minor mode to interact with an inferior Python REPL."
  :group 'python)

(defcustom python-vterm-hook nil
  "Hook run after starting a Python script buffer with an inferior Python REPL."
  :type 'hook
  :group 'python-vterm)

(defvar-local python-vterm-fellow-repl-buffer nil)
(defvar-local python-vterm-session nil)

(defun python-vterm-fellow-repl-buffer (&optional session-name)
  "Return the paired REPL buffer or the one specified with SESSION-NAME."
  (if session-name
      (python-vterm-repl-buffer session-name)
    (if (buffer-live-p python-vterm-fellow-repl-buffer)
	python-vterm-fellow-repl-buffer
      (if python-vterm-session
	  (python-vterm-repl-buffer python-vterm-session)
	(python-vterm-repl-buffer)))))

(defun python-vterm-switch-to-repl-buffer (&optional arg)
  "Switch to the paired REPL buffer or to the one with a specified session name.
With prefix ARG, prompt for session name."
  (interactive "P")
  (let* ((session-name
	  (cond ((null arg) nil)
		(t (completing-read "Session name: " (python-vterm-repl-list-sessions) nil nil nil nil
				    (python-vterm-repl-session-name (python-vterm-fellow-repl-buffer))))))
	 (script-buffer (current-buffer))
	 (repl-buffer (python-vterm-fellow-repl-buffer session-name)))
    (setq python-vterm-fellow-repl-buffer repl-buffer)
    (with-current-buffer repl-buffer
      (setq python-vterm-repl-script-buffer script-buffer)
      (switch-to-buffer-other-window repl-buffer))))

(defun python-vterm-send-return-key ()
  "Send a return key to the Python REPL."
  (with-current-buffer (python-vterm-fellow-repl-buffer)
    (vterm-send-return)))

(defun python-vterm-paste-string (string &optional session-name)
  "Send STRING to the Python REPL buffer using brackted paste mode.
If SESSION-NAME is given, the REPL with the session name, otherwise
the main REPL, is used."
  (with-current-buffer (python-vterm-fellow-repl-buffer session-name)
    (vterm-send-string string t)))

(defun python-vterm-send-current-line ()
  "Send the current line to the Python REPL, and move to the next line.
This sends a newline after the content of the current line even if there's no
newline at the end.  A newline is also inserted after the current line of the
script buffer."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((clmn (current-column))
	  (char (char-after))
	  (line (string-trim (thing-at-point 'line t))))
      (unless (and (zerop clmn) char)
	(when (/= 0 clmn)
	  (python-vterm-paste-string line)
	  (python-vterm-send-return-key)
	  (if (not char)
	      (newline))))))
  (forward-line))

(defun python-vterm-ensure-newline (str)
  "Add a newline at the end of STR if the last character is not a newline."
  (concat str (if (string= (substring str -1 nil) "\n") "" "\n")))

(defun python-vterm-send-region-or-current-line ()
  "Send the content of the region if the region is active, or send the current line."
  (interactive)
  (if (use-region-p)
      (let ((str (python-vterm-ensure-newline (buffer-substring-no-properties (region-beginning) (region-end)))))
	(python-vterm-paste-string str)
	(deactivate-mark))
    (python-vterm-send-current-line)))

(defun python-vterm-send-buffer ()
  "Send the whole content of the script buffer to the Python REPL line by line."
  (interactive)
  (save-excursion
    (python-vterm-paste-string (python-vterm-ensure-newline (buffer-string)))))

(defun python-vterm-send-include-buffer-file (&optional arg)
  "Send a line to evaluate the buffer's file using include() to the Python REPL.
With prefix ARG, use Revise.includet() instead."
  (interactive "P")
  (let ((fmt (if arg "Revise.includet(\"%s\")\n" "include(\"%s\")\n")))
    (if (and buffer-file-name
	     (file-exists-p buffer-file-name)
	     (not (buffer-modified-p)))
	(python-vterm-paste-string (format fmt buffer-file-name))
      (message "The buffer must be saved in a file to include."))))

(defun python-vterm-send-cd-to-buffer-directory ()
  "Change the REPL's working directory to the directory of the buffer file."
  (interactive)
  (if buffer-file-name
      (let ((buffer-directory (file-name-directory buffer-file-name)))
	(python-vterm-paste-string (format "cd(\"%s\")\n" buffer-directory))
	(with-current-buffer (python-vterm-fellow-repl-buffer)
	  (setq default-directory buffer-directory)))
    (message "The buffer is not associated with a directory.")))

(defalias 'python-vterm-sync-wd 'python-vterm-send-cd-to-buffer-directory)

(defun python-vterm-fellow-repl-buffer-status ()
  "Return REPL mode or nil if REPL is not ready for input."
  (with-current-buffer (python-vterm-fellow-repl-buffer)
    (python-vterm-repl-buffer-status)))

;;;###autoload
(define-minor-mode python-vterm-mode
  "A minor mode for a Python script buffer that interacts with an inferior Python REPL."
  :init-value nil
  :lighter " ⁂"
  :keymap
  `((,(kbd "C-c C-z") . python-vterm-switch-to-repl-buffer)
    (,(kbd "C-<return>") . python-vterm-send-region-or-current-line)
    (,(kbd "C-c C-b") . python-vterm-send-buffer)
    (,(kbd "C-c C-i") . python-vterm-send-include-buffer-file)
    (,(kbd "C-c C-d") . python-vterm-send-cd-to-buffer-directory)))


;;----------------------------------------------------------------------
;; Define some utility aliases but not override if the names are already used.
(unless (fboundp 'python)
  (defalias 'python 'python-vterm-repl))

(unless (boundp 'python-session)
  (defvaralias 'python-session 'python-vterm-session))


(provide 'python-vterm)

;;; python-vterm.el ends here
