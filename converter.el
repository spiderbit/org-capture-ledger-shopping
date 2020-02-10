;;; converter.el --- Convert from old file-format to json format

;;; Commentary:
;; 

;;; Code:

(load "helper")

(defun shop-items-convert ()
  (interactive)
  "Convert old format to a pretty json format"
  (let* ((data (read-from-file2 "~/.emacs.d/shop-items.txt")))
    (json-write-file data "~/.emacs.d/shop-items.json" t)))

(provide 'converter)

;;; converter.el ends here
