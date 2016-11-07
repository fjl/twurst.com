(require 'ox-publish)
(require 'ox-html)
(require 'htmlize)

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
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :with-author t
         :with-date t
         :babel-evaluate nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-divs ((preamble "div" "preamble") (content "main" "content") (postamble "div" "postamble"))
         :html-checkbox-type html
         :html-htmlize-output-type css
         :html-postamble ""
         :html-preamble make-twurst-article-preamble
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-head "<link rel=\"stylesheet\" href=\"../static/article.css\">")

        ("twurst-frontpage"
         :base-directory ,twurst-project-dir
         :include ("index.org")
         :publishing-directory ,twurst-output-dir
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :with-author nil
         :with-date nil
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
         :components ("twurst-articles" "twurst-frontpage" "twurst-assets" "twurst-style-assets"))))

(setq org-publish-project-alist twurst-projects)

(defun publish-twurst.com ()
  (interactive)
  (let ((org-publish-project-alist twurst-projects)
        (org-html-htmlize-output-type 'css))
    (org-publish "twurst.com" t)))

