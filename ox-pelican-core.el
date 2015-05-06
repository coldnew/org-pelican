;;; ox-pelican-core.el --- Core functions for ox-pelican.el.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/org-pelican
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5") (f "0.17.2") (noflet "0.0.11"))

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

(require 'noflet)
(require 'f)
(require 'ox-publish)


;;;; Paragraph

(defun org-pelican--paragraph (func paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let* (;; Fix multibyte language like chinese will be automatically add
         ;; some space since org-mode will transpose auto-fill-mode's space
         ;; to newline char.
         (fix-regexp "[[:multibyte:]]")
         (fix-contents
          (replace-regexp-in-string
           (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" contents))
         ;; Unfill paragraph to make contents look more better
         (unfill-contents
          (with-temp-buffer
            (insert fix-contents)
            (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil (point-min) (point-max))
            (buffer-string))))

    ;; Send modify data to func
    (funcall func paragraph unfill-contents info)))


;;;; Link

(defun org-pelican--link (func link contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type link))
         (raw-link (org-element-property :path link))
         (raw-path (expand-file-name raw-link))
         (pelican-link (funcall func link contents info)))

    ;; file
    (when (string= type "file")
      ;; check if file porint to absolute path
      (when (file-name-absolute-p raw-link)
        ;; calculate relative link for current post
        (setq raw-link (f-relative raw-path
                                   (file-name-directory (buffer-file-name (current-buffer)))))
        (setq pelican-link (s-replace (concat "file://" raw-path) raw-link pelican-link)))

      ;; convert relative path from `data/xxx.png' to `{filename}data/xxx.png'
      (setq pelican-link (s-replace raw-link
                                    (concat "{filename}" raw-link) pelican-link)))
    pelican-link))


;;;; Metadata

(defun org-pelican--parse-date (info)
  "Parse #+DATE: value."
  (let ((date (car (plist-get info :date))))
    (and (org-string-nw-p date)
         (if (stringp date)
             ;; FIXME: move to blogit?
             ;; backward compability with blogit
             date
           ;; parse org-timestamp
           (format-time-string "%Y-%m-%d %H:%M:%S"
                               (apply 'encode-time (org-parse-time-string
                                                    (org-element-property :raw-value date))))))))

(defun org-pelican--parse-title (info)
  "Parse #+TITLE: value."
  (let ((title (plist-get info :title)))
    (org-export-data (or title "") info)))

(provide 'ox-pelican-core)
;;; ox-pelican-core.el ends here.
