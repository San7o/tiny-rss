(load "~/projects/tiny-rss/tiny-rss.el")

;; Test
(setq debug-on-error t)
;;(debug-on-entry 'tiny-rss-generate)
(tiny-rss-generate
 :input-directory "~/projects/tiny-rss/examples"
 :output-directory "~/projects/tiny-rss/output"
 :title "My RSS"
 :link "mywebsite.com"
 :description "Read all my RSS feeds")
