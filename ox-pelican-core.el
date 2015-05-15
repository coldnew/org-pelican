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
(require 's)
(require 'ox-publish)


;;;; Backend general

;; pelican metadata
(defvar org-pelican--options-alist
  '((:date     "DATE"       nil     nil)
    (:category "CATEGORY"   nil     nil)
    (:tags     "TAGS"       nil     nil)
    (:url      "URL"        nil     nil)
    (:save_as  "SAVE_AS"    nil     nil)
    (:slug     "SLUG"       nil     nil)
    (:status   "STATUS"     nil     nil)
    ))


;;;; Internal functions

(defun org-pelican--protect-tag (tag)
  "Convert:
       _     ->  <space>
       @     ->  -
     <space> ->  ,
"
  (replace-regexp-in-string
   "_" " "
   (replace-regexp-in-string
    " " ","
    (replace-regexp-in-string
     "@" "-"
     tag))))

(defun org-pelican--protect-string (str)
  "Convert \" -> &quot;"
  (replace-regexp-in-string
   "\"" "&quot;" (org-html-encode-plain-text str)))

(defun org-pelican--protect-string* (str)
  (org-pelican--protect-tag
   (org-pelican--protect-string str)))


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

(defun org-pelican--parse-author (info)
  (and (plist-get info :with-author)
       (let ((auth (plist-get info :author)))
         (and auth
              ;; Return raw Org syntax, skipping non
              ;; exportable objects.
              (org-element-interpret-data
               (org-element-map auth
                   (cons 'plain-text org-element-all-objects)
                 'identity info))))))

(defun org-pelican--parse-gravatar (info)
  (let ((email (plist-get info :email)))
    (if email
        (format "http://www.gravatar.com/avatar/%s" (md5 email))
      "")))

;; :date: 2010-10-03 10:20
;; :modified: 2010-10-04 18:40
;; :tags: thats, awesome
;; :category: yeah
;; :slug: my-super-post
;; :authors: Alexis Metaireau, Conan Doyle
;; :summary: Short version for index and feeds
;; :lang: en
;; :translation: true
;; :status: draft
;; :status: published
(defun org-pelican--build-meta-info
    (info title-format metainfo metainfo* toc)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel.
"
  (let ((author (org-pelican--parse-author info))
        (title (org-pelican--parse-title info))
        (date (org-pelican--parse-date info))
        (gravatar (org-pelican--parse-gravatar info))
        (description (plist-get info :description))
        (keywords (plist-get info :keywords))
        (category (plist-get info :category))
        (tags (plist-get info :tags))
        (save_as (plist-get info :save_as))
        (url (plist-get info :url))
        (slug (plist-get info :slug))
        (status (plist-get info :status))) ;; NOTE: value: draft, published
    (concat

     (format title-format title)
     "\n"

     (funcall metainfo "generator" "org-pelican")
     (funcall metainfo "author" author)
     (funcall metainfo "author_gravatar" gravatar)
     (funcall metainfo "date" date)

     (funcall metainfo "description" description)
     (funcall metainfo "keywords" keywords)

     (funcall metainfo "url" url)
     (funcall metainfo "save_as" save_as)
     (funcall metainfo "slug" slug)
     (funcall metainfo "status" status)

     ;; compact version
     (funcall metainfo* "category" category)
     (funcall metainfo* "tags" tags)

     ;; Table of contents
     (let ((depth (plist-get info :with-toc)))
       (when depth
         (funcall metainfo "toc" (funcall toc depth info))))
     )))

(provide 'ox-pelican-core)
;;; ox-pelican-core.el ends here.
