;;; ox-pelican-md.el --- Export org-mode to pelican markdown.

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

(require 'f)
(require 'ox-md)
(require 'ox-publish)

(require 'ox-pelican-core)


;;;; Backend

(org-export-define-derived-backend 'pelican-md 'md
  :translate-alist
  '(
    ;; Fix for multibyte language
    (paragraph . org-pelican-md-paragraph)
    ;; Fix for pelican metadata
    (template . org-pelican-md-template)
    ;; Fix link path to suite for pelican
    (link . org-pelican-md-link)
    ;; Make compatible with pelican
    (src-block . org-pelican-md-src-block)
    )
  :options-alist
  '(;; ;; pelican metadata
    (:date     "DATE"       nil     nil)
    (:category "CATEGORY"   nil     nil)
    (:tags     "TAGS"       nil     nil)
    (:url      "URL"        nil     nil)
    (:save_as  "SAVE_AS"    nil     nil)
    (:slug     "SLUG"       nil     nil)
    (:status   "STATUS"     nil     nil)
    ))


;;;; Paragraph

(defun org-pelican-md-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  ;; Send modify data to org-md-paragraph
  (org-pelican--paragraph 'org-md-paragraph paragraph contents info))


;;;; Link

(defun org-pelican-md-link (link contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((org-md-link-org-files-as-md nil))
    (org-pelican--link 'org-md-link link contents info)))


;;;; Example Block and Src Block

;;;; Src Block

(defun org-pelican-md-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((lang (org-element-property :language src-block)))
    (format "    :::%s\n%s\n"
            lang
            (org-md-example-block src-block contents info))))


;;;; Template

(defun org-pelican-md---build-meta-info (name var func)
  (and (org-string-nw-p var)
       (format "%s: %s\n" (capitalize name) (funcall func var))))

(defun org-pelican-md--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (org-pelican--build-meta-info
   info
   ;; title format
   "Title: %s"
   ;; method to build generic metainfo
   '(lambda (name var)
      (org-pelican-md---build-meta-info name var 'org-pelican--protect-string))
   ;; method to build compact metainfo
   '(lambda (name var)
      (org-pelican-md---build-meta-info name var 'org-pelican--protect-string*))
   ;; method to build toc
   '(lambda (depth info)
      ;;(org-pelican-html-toc depth info)
      )))

(defun org-pelican-md-template (contents info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (concat
   (org-pelican-md--build-meta-info info)
   "\n"
   contents))


;;; End-user functions

;;;###autoload
(defun org-pelican-export-as-md
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'pelican-md "*pelican markdown Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (markdown-mode))))

;;;###autoload
(defun org-pelican-publish-to-md (plist filename pub-dir)
  "Publish an org file to rst.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'pelican-md filename ".md"
                      plist pub-dir))

(provide 'ox-pelican-md)
;;; ox-pelican-md.el ends here.

;; TODO: pelican-toc support