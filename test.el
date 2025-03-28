(load "~/projects/tiny-rss/tiny-rss.el")

;; Test
(setq debug-on-error t)
;;(debug-on-entry 'tiny-rss-generate)

(if (not (tiny-rss-rfc822-parse-timestamp "Wed, 27 Mar 2024 14:30:00 GMT"))
    (error "Error parsing rfc822"))

(setq tiny-rss-filter-after-date "20250301")
(tiny-rss-generate
 :input-directory "~/projects/tiny-rss/examples"
 :output-directory "~/projects/tiny-rss/output"
 :title "My RSS"
 :link "mywebsite.com"
 :description "Read all my RSS feeds"
 :filter 'tiny-rss-filter-after-date
 :enforce-rfc822 t)
