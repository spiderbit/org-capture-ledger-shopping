;;; ledger-shopping-capture.el --- Org capture template to ledger format

;; Copyright (C) 2019 Stefan Huchler

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

(load "helper")
(require 'dash)
(require 'cl-lib)
(require 'cl-seq)
(require 'ledger-mode)

(defvar shopping-completing-function 'ido-completing-read
  "Function to use for minibuffer capturing.")

(defun sbit/ledger-shopping-capture-dialog ()
  (let* ((json-file "~/.emacs.d/shop-items.json"))
    (let* ((shops (if (file-exists-p json-file)
		      (json-read-file json-file) '()))
	   (names (cl-mapcar 'car shops))
	   (shop-name (intern (funcall shopping-completing-function
				       "Shop: " (mapcar 'symbol-name names))))
	   (date (org-read-date nil nil))
	   (new-prices) (new-names) (new-amounts)
	   (products (assoc-default shop-name shops)))
      (while (let* ((names (cl-mapcar 'car products))
     		    (name (progn (define-key ido-common-completion-map "\C-p" 'ido-edit-input)
			    (intern
			     (funcall shopping-completing-function
				      "Name: " (mapcar 'symbol-name names)))))
       		    (amount (read-number "Amount: " 1))
     		    (item (alist-get name products))
     		    (item (if (stringp item) (string-to-number item) item))
       		    (price (read-number "Price: " (or item 2.00))))
     	       (setq new-names (append new-names (list name)))
     	       (setq new-prices (append new-prices (list price)))
     	       (setq new-amounts (append new-amounts (list amount)))
     	       (y-or-n-p "More items? ")))
      (let* ((-compare-fn (lambda (x y) (equal (car x) (car y))))
     	     (new-products (cl-mapcar 'cons new-names new-prices))
     	     (combined-products (-distinct (append new-products products)))
	     (combined-shops (-distinct (append `((,shop-name . ,combined-products)) shops)))
     	     (format-string "      expenses:food:%s \t\t%s St @ =€%s")
     	     (format-function (lambda (name amount price)
     				(format format-string name amount price)))
	     (product-lines (cl-mapcar format-function new-names
     				     new-amounts new-prices))
     	     (shopping-items (s-join "\n" product-lines))
     	     (total (cl-reduce '+ (cl-mapcar '* new-amounts new-prices))))
	(json-write-file combined-shops json-file t)
	(with-temp-buffer
	  (insert (concat (format "  %s * %s\n%s" date shop-name shopping-items)
     			  (format "\n    assets:bank:chequing\t\t€-%s"
       				  (read-string "Total: " (format "%.2f" total)))))
	  (sbit/ledger-align-buffer)
	  (buffer-string))))))

(provide 'ledger-shopping-capture)

;;; ledger-shopping-capture.el ends here
