;; tiny-rss.el -- Flexible RSS 2.0 generator from org headings -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.
;; Author: Giovanni Santini <santigio2003@gmail.com>
;; Maintainer: Giovanni Santini <santigio2003@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (org "9.3"))
;; Keywords: org, blog, feed, rss
;; Homepage: https://github.com/San7o/tiny-rss.git

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To generate a feed item from an heading, you can add the properties
;; =TITLE=, =RSS=, =DATE=, =AUTHOR=, =LINK= and =CATEGORY= to the
;; heading, like in the below example.  Note that only the =RSS=
;; property is required to generate the feed item.  If no title is
;; specified, It will default to the title of the heading.  Other
;; unspecified properties will be set to =nil=.

;; #+begin_src org
;; * My blog post!
;;   :PROPERTIES:
;;   :RSS: t
;;   :DATE: 22 Feb 2025 14:45:30 PST
;;   :CATEGORY: Tech
;;   :AUTHOR: Richard Stallman
;;   :LINK: myblog.com/my-blog.html
;;   :END:
;;   My beautiful blog here...
;; #+end_src

;; To generate the RSS file[s], call =tiny-rss-generate= with at least
;; the arguments =input-directory= and =output-directory=, representing
;; respectively the input directory where the org files are fetched, and
;; the output directory where the =.rss= files will be generated.  If the
;; output directory does not exist, It will be created.  Additional
;; options are documented later.

;; #+begin_src emacs-lisp
;;   (require 'tiny-rss)
;;   (tiny-rss-generate
;;    :input-directory "/my/org/files"
;;    :output-directory "/output/dir/here")
;; #+end_src

;; =tiny-rss= will generate a different =.rss= feed for each specified
;; category, and aggregate all the rss items with the same category in
;; the same category file.  Each feed filename follows the convention
;; =feed$CATEGORY.rss=.  Each item will contain the properties specified in
;; the header and all the content under that header, converted in html.

;;; Code:

(require 'org)
(require 'ox-html)

;;; Filters:

(defun tiny-rss-filter-accept (item)
  "Filter to accept every ITEM.

This is the default filter used by tiny-rss, if none are specified.
A filter takes an ITEM and returns either t or nil specifying if this
RSS item should be included in the output or not, respectively.  To
know how to parse an item, please read the documentation of
the function =tiny-rss-get-items=."
  t)

(defun tiny-rss-filter-after-date (item)
  "Filter to accept only the dates past a certain other date.

This other date is fetched in the variable =tiny-rss-filter-after-date=.
This variable should follow the format
YYYYMMDD.  The date in the rss properties is expected to follow
rfc822 format.
Argument ITEM An RSS item to be checked"
  (let* ((parsed (tiny-rss-rfc822-parse-timestamp (nth 1 item)))
         (day (nth 1 parsed))
         (month (nth 2 parsed))
         (year (nth 3 parsed)))
    (setq month (tiny-rss-rfc822-month-to-number month))
    (if (string> (concat year month day) tiny-rss-filter-after-date)
    t nil)))

;;; Core:

(defun tiny-rss-generate (&rest args)
  "Entrypoint of tiny-rss to generate the RSS feed from org files.

The user can specify the following options as ARGS:
- REQUIRED input-directory string: the path of the directory where the org
  files are stored.  This will be traversed recursively and all the
  files ending with .org will be checked for headings with the
  RSS property set to true.
- REQUIRED output-directory string: the path of the directory where the
  .rss files will be generated.  If the directory does not exist,
  It will be created.
- OPTIONAL category-info: a list of category-info.  A single
  category-info is a list with the elements:
 - category: the feed category for which this information belongs
  - title: the title of the category
  - link: a link to a website or the homepage of the category
  - description: a description of the feed category
  tiny-rss will create a different .rss file for each specified
  category.  RSS items with the same category will be aggregated in
  the same .rss file.  This options lets the user specify metadata
  for the specific category.  By default, all values are nil.  If the
  category selected is nil, the information are ment for the default
  category (or no category, they are the same thing).
- OPTIONAL enforce-rfc822 t or nil: check for compliance with rfc822
  for dates.  Default is nil."
  (let ((input-directory (plist-get args :input-directory))
        (output-directory (plist-get args :output-directory))
        (categories-info (plist-get args :category-info))
        (filter (plist-get args :filter))
        (enforce-rfc822 (plist-get args :enforce-rfc822)))
    (if (not filter)
        (setq filter 'tiny-rss-filter-accept))
    (if input-directory
        (progn
          (setq input-directory (expand-file-name input-directory))
          (let ((items-list (tiny-rss-traverse-recursive input-directory)))
            (if output-directory
                (progn
                  (setq output-directory (expand-file-name output-directory))
                  (if enforce-rfc822
                      (tiny-rss-rfc822-check items-list))
                  (tiny-rss-output output-directory categories-info items-list filter)
                  (message "tiny-rss-generate: RSS feed generated successfully"))
              (error "tiny-rss-generate: No output directory specified"))))
      (error "tiny-rss-generate: No input directory specified"))))

(defun tiny-rss-traverse-recursive (directory)
  "Recusrively traverse a DIRECTORY and return a list of RSS items."
  (let* ((files-relative (directory-files-recursively directory "\\.org$" t))
         (files (mapcar 'expand-file-name files-relative)))
    (mapcar 'tiny-rss-get-items files)))

(defun tiny-rss-get-items (filename)
  "Generate a list of RSS items from a file.

An RSS item is a list of strings with the following structure:
\(TITLE DATE CATEGORY AUTHOR LINK CONTENT)
Argument FILENAME The org file that will be parsed."
  (let ((buffer (find-file filename))) ;; Open the file
    (if buffer
        (with-current-buffer buffer
          (apply 'append ;; Flatten the list
                 (org-map-entries
                  (lambda ()
                    (let* ((title (org-entry-get (point) "TITLE"))
                           (title-real (or title (org-entry-get nil "ITEM"))) ;; Fallback to heading name
                           (date (org-entry-get (point) "DATE"))
                           (category (org-entry-get (point) "CATEGORY"))
                           (author (org-entry-get (point) "AUTHOR"))
                           (link (org-entry-get (point) "LINK"))
                           (html-buffer (org-html-export-as-html nil t nil t))
                           (content (with-current-buffer "*Org HTML Export*" (buffer-string))))
                      (list (list title-real date category author link content)))) ;; Return a list containing the item
                  "RSS=\"true\"" 'file)))
      (error "tiny-rss-get-items: Unable to get buffer %s" filename))))

(defun tiny-rss-output (output-directory categories-info items-list filter)
  "Write the RSS items to .rss files.

Argument OUTPUT-DIRECTORY The path to the directory where the .rss
files will be generated.
Argument CATEGORIES-INFO User provided information about categories.
Please read =tiny-rss-generate= for more information.
Argument ITEMS-LIST The list of items to write.  Read
=tiny-tss-get-items= for additional info.
Argument FILTER User provided filter."
  (tiny-rss-create-rss-files output-directory categories-info items-list filter)
  (dolist (item-list items-list)
    (dolist (item item-list)
      (let* ((title-item (nth 0 item))
             (date (nth 1 item))
             (category (nth 2 item))
             (author (nth 3 item))
             (link (nth 4 item))
             (content (nth 5 item))
             (file (tiny-rss-file-name-from-category output-directory category)))
        (if (funcall filter item)
            (tiny-rss-add-feed-to-file title-item date category author link content file)))))
  (tiny-rss-files-add-closing-tags output-directory items-list filter))

(defun tiny-rss-file-name-from-category (output-directory category)
  "Return the name of the .rss file for a specific CATEGORY.

The name is concatenated with the output directory.
Argument OUTPUT-DIRECTORY Output directory path."
  (concat output-directory "/feed" category ".rss"))

(defun tiny-rss-create-rss-files (output-directory categories-info items-list filter)
  "Create the rss files from a list of rss items.

Argument OUTPUT-DIRECTORY Path to the directory where the .rss files
will be generated.
Argument CATEGORIES-INFO User provided information for categories.
Argument ITEMS-LIST List of RSS items.
Argument FILTER User provided filter function."
  (dolist (item-list items-list)
    (dolist (item item-list)
      (let* ((category (nth 2 item))
             (file (tiny-rss-file-name-from-category output-directory category)))
        (if (funcall filter item)
            (tiny-rss-create-rss-file file category categories-info))))))

(defun tiny-rss-files-add-closing-tags (output-directory items-list filter)
  "Close xml tags at the end of the .rss files.

This is called after all the items have been inserted.
Argument OUTPUT-DIRECTORY Directory where the .rss files are located.
Argument ITEMS-LIST List of RSS items.
Argument FILTER Filtering function."
  (dolist (item-list items-list)
    (dolist (item item-list)
      (let* ((category (nth 2 item))
             (file (tiny-rss-file-name-from-category output-directory category))
             (closing-tags "</channel></rss>"))
        (if (funcall filter item)
            (with-current-buffer (find-file file)
              (if (not (string-match-p (regexp-quote closing-tags) (buffer-string)))
                  (append-to-file closing-tags nil file))))))))
        
(defun buffer-contains-substring (string)
  "Return t if the current buffer contain a substring.

Argument STRING The substring that will be matched."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun tiny-rss-create-rss-file (file category categories-info)
  "Create an .rss FILE with the specified fields.

Argument CATEGORY Category of the feed.
Argument CATEGORIES-INFO List of category-info."
  (let ((directory (file-name-directory file))
        (title nil)
        (link nil)
        (description nil))
    (if (not (file-directory-p directory))
        (make-directory (file-name-directory file)))
    (dolist (category-info categories-info)
      (if (string= category (plist-get category-info :category))
          (progn
            (setq title (plist-get category-info :title))
            (setq link (plist-get category-info :link))
            (setq description (plist-get category-info :description)))))
    (write-region
     (format (concat
              "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
              "<rss version=\"2.0\">"
              "<channel>"
              "\n<title>%s</title>\n"
              "<link>%s</link>\n"
              "<description><![CDATA[%s]]></description>\n")
             title link description)
     nil file)))

(defun tiny-rss-add-feed-to-file (title date category author link content file)
  "Append a single item entry in an .rss FILE with the specified fields.

Argument TITLE Title of the RSS item.
Argument DATE Date of the RSS item.
Argument CATEGORY Category of the RSS item.
Argument AUTHOR Author of the RSS item.
Argument LINK A link, usually displayed at the end of the feed by RSS
clients pointing to a web render of the CONTENT."
  (message (concat "tiny-rss: adding feed to file" file))
  (append-to-file
   (format (concat
            "<item>\n"
            "<title>%s</title>\n"
            "<link>%s</link>\n"
            "<author>%s</author>\n"
            "<pubDate>%s</pubDate>\n"
            "<description><![CDATA[%s]]></description>\n"
            "</item>\n")
            title link author date content)
   nil file))

;;; rfc822 related functions:

(setq tiny-rss-rfc822-pattern "\\(?:\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\), \\)?\\([0-9]\\{2\\}\\) \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) \\([0-9]\\{4\\}\\) \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\(?:\\(:[0-9]\\{2\\}\\)?\\) \\(UT\\|GMT\\|EST\\|EDT\\|CST\\|CDT\\|MST\\|MDT\\|PST\\|PDT\\|1ALPHA\\|[+-][0-9]\\{4\\}\\)")

(defun tiny-rss-rfc822-check (items-list)
  "Check if dates in items are compatible with rfc822.

Throws an error if a date is not compliant, specifying which date.
Argument ITEMS-LIST List of items to check."
  (dolist (item-list items-list)
    (dolist (item item-list)
      (let* ((timestamp (nth 1 item))
             (this (string-match tiny-rss-rfc822-pattern timestamp)))
        (if (not this)
            (error "tiny-rss-rfc822-check: %s is not rfc822" timestamp))))))

(defun tiny-rss-rfc822-parse-timestamp (timestamp)
  "Parse a TIMESTAMP like and return a list of the parsed data.

The requrned list has the following structure:
   (DAY-NAME DAY MONTH YEAR HOUR MINUTE SECONDS TIMEZONE)"
  (if (string-match tiny-rss-rfc822-pattern timestamp)
      (list (match-string 1 timestamp)  ;; Optional day of the week
            (match-string 2 timestamp)  ;; Day
            (match-string 3 timestamp)  ;; Month
            (match-string 4 timestamp)  ;; Year
            (match-string 5 timestamp)  ;; Hour
            (match-string 6 timestamp)  ;; Minute
            (when (match-string 7 timestamp)  ;; Seconds (remove colon)
              (substring (match-string 7 timestamp) 1))
            (match-string 8 timestamp)) ;; Time zone
    (error "Error parsing date %s. does it follow rfc822?" timestamp)))

(defun tiny-rss-rfc822-month-to-number (month)
  "Convert a three-letter MONTH abbreviation to a two-digit string."
  (cdr (assoc month '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03")
                      ("Apr" . "04") ("May" . "05") ("Jun" . "06")
                      ("Jul" . "07") ("Aug" . "08") ("Sep" . "09")
                      ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))

(provide 'tiny-rss)

;;; tiny-rss.el ends here
