;;; ox-pelican-html.el --- Export org-mode to pelican HTML.

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
(require 'ox-html)
(require 'ox-publish)

(require 'ox-pelican-core)


;;;; Backend

(org-export-define-derived-backend 'pelican-html 'html
  :translate-alist
  '(
    (template . org-pelican-html-template)
    ;; Fix for multibyte language
    (paragraph . org-pelican-html-paragraph)
    ;; FIXME: this should move back to blogit?
    ;; Fix toc for blogit theme
    (inner-template . org-pelican-html-inner-template)
    ;; convert relative link to let pelican can recognize
    (link . org-pelican-html-link)
    )
  :options-alist org-pelican--options-alist)


;;;; Paragraph

(defun org-pelican-html-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (org-pelican--paragraph 'org-html-paragraph
                          paragraph contents info))


;;; Template

(defun org-pelican-html-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   ;; (let ((depth (plist-get info :with-toc)))
   ;;   (when depth (org-pelican-html-toc depth info)))

   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))


;;;; Link

(defun org-pelican-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'.

In this function, we also add link file"
  (let ((org-html-link-org-files-as-html nil)
        (html-link (org-pelican--link 'org-html-link link desc info)))
    ;; fancybox support
    (replace-regexp-in-string
     "<img[^>]+src=\"\\(.*?\\)\"\\([^>]*>\\)" "<a class=\"fancybox\" href=\"\\1\"><img src=\"\\1\" \\2</a>" html-link)
    ))
;;"<img[^>]+src=\"\\(.*?\\)\"[^>]*>"
;;      "<img[^>]+src=\"\\(.*?\\)\"\\([^>]*\\)"
;; <a class="fancybox" href="link_of_image"><img src="link_of_image"/></a>

;;; Tables of Contents

;; ref: https://github.com/ingwinlu/pelican-toc
(defun org-pelican-html-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let ((toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth)))
        (outer-tag (if (and (org-html-html5-p info)
                            (plist-get info :html-html5-fancy))
                       "nav"
                     "div")))
    (when toc-entries
      (concat (format "<%s id=\"toc\">\n" outer-tag)
              (org-html--toc-text toc-entries)
              (format "</%s>\n" outer-tag)))))


;;; Template

(defun org-pelican-html---build-meta-info (name var func)
  (and (org-string-nw-p var)
       (concat
        (org-html-close-tag "meta"
                            (format " name=\"%s\" content=\"%s\""
                                    name
                                    (funcall func var))
                            info)
        "\n")))

(defun org-pelican-html--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (org-pelican--build-meta-info
   info
   ;; title format
   "<title>%s</title>"
   ;; method to build generic metainfo
   '(lambda (name var)
      (org-pelican-html---build-meta-info name var 'org-pelican--protect-string))
   ;; method to build compact metainfo
   '(lambda (name var)
      (org-pelican-html---build-meta-info name var 'org-pelican--protect-string*))
   ;; method to build toc
   '(lambda (depth info)
      (org-pelican-html-toc depth info))))

(defun org-pelican-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (org-html-doctype info)
   "\n"
   "<head>\n"
   (org-pelican-html--build-meta-info info)
   "</head>\n"
   "<body>\n"

   ;; Document contents.
   (format "<%s id=\"%s\">\n"
           (nth 1 (assq 'content org-html-divs))
           (nth 2 (assq 'content org-html-divs)))

   contents
   (format "</%s>\n"
           (nth 1 (assq 'content org-html-divs)))

   ;; Closing document.
   "</body>\n</html>"))


;;; End-user functions

;;;###autoload
(defun org-pelican-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'pelican-html "*pelican HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (html-mode))))

;;;###autoload
(defun org-pelican-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'pelican-html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension "html"))
                      plist pub-dir))

(provide 'ox-pelican-html)
;;; ox-pelican-html.el ends here.
