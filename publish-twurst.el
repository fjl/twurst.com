;; -*- lexical-binding: t -*-

(require 'ox-publish)
(require 'ox-html)
(require 'ox-rss)
(require 'ox-org)
(require 'htmlize)
(require 'ob-go)
(require 'dash)

;; HTML Article Backend

(defun twurst-article-html-headline (headline contents info)
  (-when-let (output (org-html-headline headline contents info))
    (replace-regexp-in-string
     "<\\(h[123]\\) id=\"\\([^\"]+\\)\"[^>]*>[^<]*</\\1>"
     "<a href=\"#\\2\" class=\"headline-link\">\\&</a>"
     output)))

(defun twurst-article-html-template (contents info)
  (replace-regexp-in-string
   "<h1 class=\"title\">[^<]*</h1>"
   "<a href=\"#\" class=\"headline-link\">\\&</a>"
   (org-html-template contents info)))

(org-export-define-derived-backend 'twurst-article-html 'html
  :translate-alist '((headline . twurst-article-html-headline)
                     (template . twurst-article-html-template)))

(defun twurst-publish-article-to-html (plist filename pub-dir)
  (org-publish-org-to 'twurst-article-html filename ".html" plist pub-dir))

;; RSS Feed

(defun twurst-article-feed-articles (index-file headline)
  "Retrieves the list of Org files that should be published in the
article feed. It gets them from the list under the given `headline' in
the Org file `index-file`."
  (with-temp-buffer
    (insert-file-contents index-file)
    (org-mode)
    (goto-char (org-find-exact-headline-in-buffer headline))
    (org-narrow-to-subtree)
    (cl-loop with org-link-search-failed = nil
             for _ = (org-next-link)
             until org-link-search-failed
             collect (org-element-property :path (org-element-context))
             do (end-of-line))))

(defun twurst-article-org-rss-headline (headline contents info)
  "Exports a headline with offset increased by 1."
  (let ((offset (or (plist-get info :headline-offset) 0)))
    (org-org-headline headline contents (list* :headline-offset (1+ offset) info))))

(defun twurst-article-org-rss-ignore (keyword backend info)
  "")

(defun twurst-article-org-rss-final-output (contents backend info)
  (let* ((article-file (plist-get info :article-filename))
         (article-id (plist-get info :article-id)))
    (with-temp-buffer
      (insert (format "* %s\n" (org-export-data (plist-get info :title) info)))
      (insert ":PROPERTIES:\n")
      (insert (format ":PUBDATE: %s\n" (org-export-get-date info "<%Y-%m-%d>")))
      (insert (format ":RSS_PERMALINK: %s\n" article-file))
      (insert (format ":ID: %s\n" article-id))
      (insert ":END:\n")
      (insert contents)
      (buffer-string))))

(org-export-define-derived-backend 'twurst-article-org-rss 'org
  :filters-alist '((:filter-final-output twurst-article-org-rss-final-output))
  :translate-alist '((headline . twurst-article-org-rss-headline)
                     (keyword . twurst-article-org-rss-ignore)
                     (comment . twurst-article-org-rss-ignore)))

(defun twurst-insert-org-rss-article (filename)
  (let* ((rel-filename (s-chop-prefix twurst-project-dir filename))
         (article-file (concat (file-name-sans-extension rel-filename) ".html"))
         (article-id (sha1 (concat "twurst.com article" rel-filename)))
         (output nil))
    (with-current-buffer (find-file-noselect filename t)
      (setq output (org-export-as 'twurst-article-org-rss nil nil nil
                                  (list :article-filename article-file
                                        :article-id article-id
                                        :with-title nil
                                        :with-tags nil
                                        :with-author nil
                                        :with-date nil
                                        :with-footnotes nil))))
    (insert output)
    (newline)))

(defun twurst-org-rss-sitemap (title file-list)
  "Creates an Org file containing all org files matched by the project."
  (message "%S" file-list)
  (let* ((files (s-split "\n" (org-list-to-generic file-list (list :ifmt (lambda (_ link) (twurst-link-target link))))))
         (org-inhibit-startup t))
    (message "%S" files)
    (with-temp-buffer
      (erase-buffer)
      (dolist (file files)
        (twurst-insert-org-rss-article file)))
      (buffer-string)))

(defun twurst-link-target (link)
  (let ((sub (substring link 7)))
    (substring sub 0 (s-index-of "]" sub))))

(defadvice org-rss-final-function (around twurst-disable-rss-indent first (contents backend info))
  contents)

(defun twurst-publish-sitemap-rss-feed (plist filename pub-dir)
  "Writes an RSS feed for the sitemap generated by twurst-org-rss-sitemap."
  (when (string-match-p "rssfeed" filename)
    (let ((org-rss-use-entry-url-as-guid nil)
          ;; Disable code fontification because there is no style sheet.
          (org-html-htmlize-output-type nil)
          (org-src-preserve-indentation t)
          (disable-indent (lambda (contents backend info) contents)))
      ;; Disable HTML indentation because it makes code blocks look bad.
      (advice-add 'org-rss-final-function :override disable-indent)
      (unwind-protect
          ;; Don't use org-rss-publish-to-rss, it generates IDs for all subheadings.
          (org-publish-org-to 'rss filename (concat "." org-rss-extension) plist pub-dir)
        (advice-remove 'org-rss-final-function disable-indent)))))

;; Site Definition

(defvar twurst-project-dir (file-name-directory load-file-name))
(defvar twurst-output-dir (concat twurst-project-dir "_site/"))

(defun make-twurst-article-preamble (plist)
  (let ((filename (file-name-nondirectory (plist-get plist :input-file)))
        (date     (org-export-get-date plist "%Y-%m-%d")))
    (format "<div id=\"article-header\"><span id=\"article-header-box\">░░</span><a href=\"../index.html\">twurst.com</a> / %s <span class=\"nowrap\">(%s)</span></div>"
            filename date)))

(setq twurst-projects
      `(("twurst-articles"
         :base-directory ,(concat twurst-project-dir "articles/")
         :base-extension "org"
         :publishing-directory ,(concat twurst-output-dir "articles/")
         :publishing-function twurst-publish-article-to-html
         :section-numbers nil
         :with-toc nil
         :with-author t
         :with-date t
         :babel-evaluate nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-divs ((preamble "div" "preamble") (content "article" "content") (postamble "div" "postamble"))
         :html-container "section"
         :html-checkbox-type html
         :html-htmlize-output-type css
         :html-postamble ""
         :html-preamble make-twurst-article-preamble
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-head "<link rel=\"stylesheet\" href=\"../static/article.css\">")

        ("twurst-article-feed"
         :base-directory ,(concat twurst-project-dir "articles/")
         :base-extension "org"
         :publishing-directory ,(concat twurst-output-dir "articles/")
         :publishing-function twurst-publish-sitemap-rss-feed
         :html-link-home "https://twurst.com/"
         :html-link-use-abs-url nil
         :title "twurst.com Articles"
         :email "fjl@twurst.com"
         :rss-feed-url "https://twurst.com/articles/feed.xml"
         :rss-image-url "https://twurst.com/static/feed.png"
         :sitemap-function twurst-org-rss-sitemap
         :sitemap-filename "feed.rssfeed"
         :sitemap-sort-files chronologically
         :auto-sitemap t)

        ("twurst-index"
         :base-directory ,twurst-project-dir
         :exclude ".*"
         :include ("index.org")
         :publishing-directory ,twurst-output-dir
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :with-author nil
         :with-date nil
         :with-title nil
         :babel-evaluate nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-checkbox-type html
         :html-htmlize-output-type css
         :html-postamble ""
         :html-head-include-default-style nil
         :html-head "<link rel=\"stylesheet\" href=\"static/home.css\">")

        ("twurst-assets"
         :base-directory ,twurst-project-dir
         :publishing-directory ,twurst-output-dir
         :base-extension "png\\|svg\\|ico\\|txt"
         :publishing-function org-publish-attachment)

        ("twurst-style-assets"
         :base-directory ,(concat twurst-project-dir "static/")
         :publishing-directory ,(concat twurst-output-dir "static/")
         :base-extension "css\\|js\\|svg\\|png"
         :publishing-function org-publish-attachment)

        ("twurst.com"
         :components ("twurst-articles"
                      "twurst-article-feed"
                      "twurst-index"
                      "twurst-assets"
                      "twurst-style-assets"))))

(setq org-publish-project-alist twurst-projects)

(defun publish-twurst.com ()
  (interactive)
  (let ((org-publish-project-alist twurst-projects)
        (org-html-htmlize-output-type 'css)
        (tab-width 4)
        (org-export-babel-evaluate nil))
    (org-publish "twurst.com" t)))
