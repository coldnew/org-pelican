;;; ox-pelican.el --- Export org-mode to pelican.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/org-pelican
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5") (f "0.17.2") (blogit "0.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'blogit)


;;;; Group

(defgroup org-pelican nil
  "Options for exporting Org mode files to pelican."
  :tag "Org Export to pelican html/md files."
  :group 'org-export
  :link '(url-link :tag "Github" "https://github.com/coldnew/org-pelican"))


;;;; Load all pelican functions
;;
;; ox-pelican-core.el -- core or common use functions
;; ox-pelican-html.el -- HTML exporter
;; ox-pelican-md.el   -- Markdown exporter
;; ox-pelican-rst.el  -- Rst exporter (not done yet)
;;
(mapcar (lambda (x) (require (intern (format "ox-pelican-%s" x)) nil t))
        '("core" "html" "md"))


;;;; End User Functions

;;;###autoload
(defun org-pelican-mark-as-draft ()
  "Mark current org-mode file as pelican draft file."
  (interactive)
  (blogit--modify-option "STATUS" "draft"))

(defun org-pelican-mark-as-published ()
  "Mark current org-mode file as pelican published file."
  (interactive)
  (blogit--modify-option "STATUS" "published"))

(provide 'ox-pelican)
;;; ox-pelican.el ends here.
