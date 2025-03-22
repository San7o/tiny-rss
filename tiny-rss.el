(require 'org)
(require 'ox-html)

(defun tiny-rss-generate (&rest args)
	(let ((input-directory (plist-get args :input-directory))
				(output-directory (plist-get args :output-directory))
				(title (plist-get args :title))
				(link (plist-get args :link))
				(description (plist-get args :description)))
		(if input-directory
				(progn
					(setq input-directory (expand-file-name input-directory))
					(let ((items-list (tiny-rss-traverse-recursive input-directory)))
						(if output-directory
								(progn
									(setq output-directory (expand-file-name output-directory))
									(tiny-rss-output output-directory title link description items-list))
							(error "tiny-rss-generate: No output directory specified"))))
			(error "tiny-rss-generate: No input directory specified"))))

(defun tiny-rss-traverse-recursive (directory)
	"Recusrively traverse a directory and return a list
   of RSS items."
	(let* ((files-relative (directory-files-recursively directory "\\.org$" t))
				 (files (mapcar 'expand-file-name files-relative)))
		(mapcar 'tiny-rss-get-items files)))

(defun tiny-rss-get-items (filename)
	"Generate a list of RSS items from a file. An RSS item is a list
   of strings with the following structure:
   (TITLE DATE CATEGORY CONTENT)"
	(let ((buffer (find-file filename))
				 (items))
		(if buffer
				(with-current-buffer buffer
					(org-map-entries
					 '(let* ((item)
									(title (org-entry-get (point) "TITLE"))
									(date (org-entry-get (point) "DATE"))
									(category (org-entry-get (point) "CATEGORY"))
									(html-buffer (org-html-export-as-html nil t nil t))
									(content (with-current-buffer "*Org HTML Export*" (buffer-string)))
							    (item (list title date category content)))
							(setq items (append items (list item))))
					 "RSS=\"true\"" 'file)
					items)
			(error "tiny-rss-get-items: unable to get buffer"))))

(defun tiny-rss-output (output-directory title link description items-list)
	(dolist (item-list items-list)
		(dolist (item item-list)
			(let* ((title-item (nth 0 item))
						 (date (nth 1 item))
						 (category (nth 2 item))
						 (content (nth 3 item))
						 (file (concat output-directory "/feed" category ".rss")))
				(if (not (file-exists-p file))
						(tiny-rss-create-rss-file file title link description))
				(tiny-rss-add-feed-to-file title-item date category content link file)))))
;; TODO: add end closing to channel and rss tags
;; TODO: properly append items to the file without regenerating everything

(defun tiny-rss-create-rss-file (file title link description)
	(let ((directory (file-name-directory file)))
		(if (not (file-directory-p directory))
				(make-directory (file-name-directory file)))
		(write-region
		 (format (concat
						  "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
							"<rss version=\"2.0\">"
							"<channel>"
							"\n<title>%s</title>\n"
							"<link>%s</link>\n"
							"<description><![CDATA[%s]]></description>\n")
						 title link link description)
		 nil file)))

(defun tiny-rss-add-feed-to-file (title date category content link file)
	(print (concat "Adding feed to file" file))
	(append-to-file
	 (format (concat
						"<item>\n"
						"<title>%s</title>\n"
						"<link>%s</link>\n"
						"<author>%s</author>\n"
						"<pubDate>%s</pubDate>\n"
						"<description><![CDATA[%s]]></description>\n"
						"</item>\n")
						title "TODO: Link" "TODO: Author" date description)
	 nil file))

(provide 'tiny-rss)
