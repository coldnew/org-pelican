;;; ox-pelican-html.el --- Export org-mode to pelican HTML.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/org-pelican
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5") (f "0.17.2"))

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
(require 'ox-html)
(require 'ox-publish)


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
    )
  :options-alist
  '((:date "DATE" nil nil)
    (:category "CATEGORY" nil nil)
    (:tags "TAGS" nil nil)
    )
  )


;;;; Paragraph

(defun org-pelican-html-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  ;; Fix multibyte language like chinese which will be automatically add
  ;; some space since org-mode will transpose auto-fill-mode's space
  ;; to newline char.
  (let* ((fix-regexp "[[:multibyte:]]")
         (fix-contents
          (replace-regexp-in-string
           (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" contents))
         ;; Unfill paragraph to make contents look more better
         (unfill-contents
          (with-temp-buffer
            (insert fix-contents)
            (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil (point-min) (point-max))
            (buffer-string))))

    ;; Send modify data to org-html-paragraph
    (org-html-paragraph paragraph unfill-contents info)))


;;; Template

(defun org-pelican-html-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-pelican-html-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))


;;; Tables of Contents

(defun org-pelican-html-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let ((toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth))))
    (when toc-entries
      (format "<div class=\"table-of-contents\">\n\n"))))


;;; Template

(defun org-pelican-html--parse-date (info)
  (let ((date (car (plist-get info :date))))
    (if (stringp date)
        ;; FIXME: move to blogit?
        ;; backward compability with blogit
        date
      ;; parse org-timestamp
      (format-time-string "%Y-%m-%d %H:%M:%S"
                          (apply 'encode-time (org-parse-time-string
                                               (org-element-property :raw-value date)))))))

;; :date: 2010-10-03 10:20
;; :modified: 2010-10-04 18:40
;; :tags: thats, awesome
;; :category: yeah
;; :slug: my-super-post
;; :authors: Alexis Metaireau, Conan Doyle
;; :summary: Short version for index and feeds
;; :lang: en
;; :translation: true
(defun org-pelican-html--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let* ((protect-string
          (lambda (str)
            (replace-regexp-in-string
             "\"" "&quot;" (org-html-encode-plain-text str))))
         (protect-string-compact
          ;; FIXME: add option to enable/disable this
          ;; convert:
          ;;   _        -> space
          ;;   <space>  -> ,
          ;;   @        -> -
          (lambda (str)
            (replace-regexp-in-string
             "_" " "
             (replace-regexp-in-string
              " " ","
              (replace-regexp-in-string
               "@" "-"  (funcall protect-string str))))))
         ;; FIXME:
         (date (org-pelican-html--parse-date info))
         (category (plist-get info :category))
         (tags (plist-get info :tags))
         (save-as (plist-get info :save_as))
         (url (plist-get info :url)))
    (concat
     ;; Use ox-html to generate basic metainfo
     (org-html--build-meta-info info)

     (org-html-close-tag "meta" " name=\"generator\" content=\"org-pelican\"" info)
     "\n"
     (and (org-string-nw-p date)
          (concat
           (org-html-close-tag "meta"
                               (format " name=\"date\" content=\"%s\"\n"
                                       (funcall protect-string date))
                              info)
           "\n"))
     (and (org-string-nw-p category)
          (concat
           (org-html-close-tag "meta"
                               (format " name=\"category\" content=\"%s\""
                                       (funcall protect-string category))
                               info)
           "\n"))
     (and (org-string-nw-p tags)
          (concat
           (org-html-close-tag "meta"
                               (format " name=\"tags\" content=\"%s\""
                                       (funcall protect-string-compact tags))
                               info)
           "\n"))
     )))

(defun org-pelican-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (org-html-doctype info)
   "\n"
   (concat "<html"
           (when (org-html-xhtml-p info)
             (format
              " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
              (plist-get info :language) (plist-get info :language)))
           ">\n")
   "<head>\n"
   (org-pelican-html--build-meta-info info)
   (org-html--build-head info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
         (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format org-html-home/up-format
               (or link-up link-home)
               (or link-home link-up))))

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
