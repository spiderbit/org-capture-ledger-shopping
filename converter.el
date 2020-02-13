;;; converter.el --- Convert from old file-format to json format

;; Copyright (C) 2015-2020 Stefan Huchler

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

(defun shop-items-convert ()
  (interactive)
  "Convert old format to a pretty json format"
  (let* ((data (read-from-file2 "~/.emacs.d/shop-items.txt")))
    (json-write-file data "~/.emacs.d/shop-items.json" t)))

(provide 'converter)

;;; converter.el ends here
