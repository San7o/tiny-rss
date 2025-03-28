;; Run this elisp file to generate an example feed from the example
;; org files.

(load "~/projects/tiny-rss/tiny-rss.el")

;; Test
(setq debug-on-error t)
;;(debug-on-entry 'tiny-rss-generate)

(print (tiny-rss-rfc822-parse-timestamp "Wed, 27 Mar 2024 14:30:00 GMT"))
(if (not (tiny-rss-rfc822-parse-timestamp "Wed, 27 Mar 2024 14:30:00 GMT"))
    (error "Error parsing rfc822"))

(setq tiny-rss-filter-after-date "20250301")

(tiny-rss-generate
 :input-directory "~/projects/tiny-rss/examples"
 :output-directory "~/projects/tiny-rss/output"
 :category-info '((:category "Blog"
                             :title "RSS for my blog"
                             :link "my-website.com"
                             :description "My RSS feeds")
                  (:category "Tech"
                             :title "MyTech"
                             :link "tech-website.com"
                             :description "RSS for Tech news"))
 :filter 'tiny-rss-filter-after-date
 :enforce-rfc822 t)
