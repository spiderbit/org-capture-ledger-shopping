;;; helper.el --- Helper functions

;; Copyright (C) 2015-2019 Stefan Huchler

;; Author: Stefan Huchler <stefan.huchler@mail.de>
;; Version: 0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'f)

(defun get-table-content (&optional start end)
  (let* ((start (or start (buffer-end -1)))
	 (end (or end (buffer-end 1)))
	 (lines (s-split "\n" (buffer-substring-no-properties
			       start end))))
    (remove nil
	    (mapcar 'string-trim
		    (mapcar (apply-partially 'remove "")
			    (mapcar (lambda (line) (split-string line "|"))
				    lines))))))

(defun my-org-read-table (&optional file linenr)
  "Return the table data as list.
Argument FILE name of the file with the org table.
Argument LINENR where the table data starts."
  (let* ((fstring (if file (f-read file) (buffer-string)))
	 (lines (s-split "\n" fstring)))
    (mapcar (lambda (line)
	      (mapcar 's-trim (s-split "|" line t)))
	    (if linenr (nthcdr linenr lines) lines))))

(defun sbit/ledger-align-buffer ()
  "Align buffer with ledger data to the desired format."
  (let ((start (progn (goto-char (point-min))
	  	      (forward-line 1) (point)))
	(end (progn (goto-char (point-max)) (point))))
    (ledger-post-align-postings start end)
    (goto-char (point-min))
    (while (> 1 (forward-line 1))
      (insert-char ?\s 2))))

(defun print-to-file2 (filename data)
  (let* ((print-length 5000)
	 (data-string (prin1-to-string data)))
    (stringp data-string)
    (f-write-text data-string 'utf-8 filename)))

(defun print-to-file (filename data)
  (with-temp-file filename
    (let* ((print-length 5000))
      (prin1 data (current-buffer)))))

(defun read-from-file2 (filename)
  (read (f-read filename)))

(defun read-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))


(provide 'helper)

;;; helper.el ends here
