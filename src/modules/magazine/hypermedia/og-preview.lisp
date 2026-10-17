(in-package #:magazine/hypermedia/components)

;;;; The preview images' page, /og-preview: each article's social preview
;;;; image at full size, then as X, Facebook and LinkedIn, Discord and Slack
;;;; show a link to it. For checking an image without posting it anywhere.
;;;; Found only where the Intel desk is (see controllers.lisp).
;;;;
;;;; The cards are close copies of the sites' own, sized and cropped as theirs
;;;; are: X crops to 2:1, Facebook and LinkedIn to 1.91:1. A site can change
;;;; its cards at any time; the Facebook Sharing Debugger and LinkedIn's Post
;;;; Inspector show theirs for a published page.

(defparameter *og-preview-css* "
*{box-sizing:border-box}
body{margin:0;padding:32px;background:#f2f2f2;color:#111;font:15px/1.45 system-ui,-apple-system,'Segoe UI',sans-serif}
h1{margin:0 0 4px;font-size:28px}
p{margin:0}
.lede{color:#555}
.problem{margin-top:12px;padding:10px 14px;border-radius:6px;background:#fde8e8;color:#8a1111}
.picker{display:flex;flex-wrap:wrap;gap:6px;margin:16px 0 32px}
.picker a{padding:4px 10px;border:1px solid #ccc;border-radius:999px;background:#fff;color:#111;font-size:13px;text-decoration:none}
.picker a.on{border-color:#111;background:#111;color:#fff}
.item{margin:0 0 56px;padding-top:24px;border-top:1px solid #ddd}
.item h2{margin:0 0 4px;font-size:20px}
.item h2 a{color:inherit}
.facts{margin-bottom:16px;color:#666;font-size:13px;font-variant-numeric:tabular-nums}
.facts a{color:inherit}
.full{display:block;width:100%;max-width:1200px;height:auto;margin-bottom:24px;border:1px solid #ccc}
.mocks{display:flex;flex-wrap:wrap;gap:32px 24px;align-items:flex-start}
.mock{margin:0;max-width:100%}
.mock img{display:block;width:100%}
figcaption{margin-top:8px;color:#666;font-size:12px}
.mock-x{width:506px}
.mock-x-card{position:relative;overflow:hidden;aspect-ratio:2/1;border:1px solid #cfd9de;border-radius:16px}
.mock-x-card img{height:100%;object-fit:cover}
.mock-x-domain{position:absolute;left:12px;bottom:12px;padding:0 4px;border-radius:4px;background:rgba(0,0,0,.77);color:#fff;font-size:13px}
.mock-x-from{margin-top:4px;color:#536471;font-size:13px}
.mock-fb{width:500px}
.mock-fb-card{border:1px solid #dadde1;background:#fff}
.mock-fb-card img{aspect-ratio:1.91/1;object-fit:cover}
.mock-fb-text{padding:10px 12px;border-top:1px solid #dadde1;background:#f0f2f5}
.mock-fb-domain{color:#65676b;font-size:12px;text-transform:uppercase}
.mock-fb-title{margin:2px 0;color:#050505;font-size:16px;font-weight:600}
.mock-fb-desc{overflow:hidden;color:#65676b;font-size:14px;white-space:nowrap;text-overflow:ellipsis}
.mock-discord{width:432px}
.mock-discord-card{padding:8px 16px 16px 12px;border-left:4px solid #1e1f22;border-radius:4px;background:#2b2d31;color:#dbdee1}
.mock-discord-site{margin-bottom:4px;color:#b5bac1;font-size:12px}
.mock-discord-title{margin-bottom:8px;color:#00a8fc;font-weight:600}
.mock-discord-desc{display:-webkit-box;margin-bottom:16px;overflow:hidden;font-size:14px;-webkit-line-clamp:3;-webkit-box-orient:vertical}
.mock-discord-card img{border-radius:4px}
.mock-slack{width:380px}
.mock-slack-card{padding-left:12px;border-left:4px solid #ddd;background:#fff}
.mock-slack-site{color:#1d1c1d;font-size:14px;font-weight:700}
.mock-slack-title{color:#1264a3;font-size:15px;font-weight:700}
.mock-slack-desc{display:-webkit-box;margin-bottom:8px;overflow:hidden;color:#1d1c1d;font-size:15px;-webkit-line-clamp:3;-webkit-box-orient:vertical}
.mock-slack-card img{max-width:360px;border-radius:8px}
")

(defparameter *og-preview-domain* "almightylisp.com")

(defun image-slug (image)
  (model:article-slug (og:image-article image)))

(defun og-preview-href (&optional slug)
  (format nil "~a~@[?slug=~a~]" (s:url "magazine:og-preview") (and slug (quri:url-encode slug))))

(defun render-og-mocks (image src)
  "IMAGE, whose PNG is at SRC, in each site's card."
  (let* ((article (og:image-article image))
         (title (model:article-title article))
         (description (og:plain-text (model:article-subtitle article))))
    (ah:</>
     (div :class "mocks"
       (figure :class "mock mock-x"
         (div :class "mock-x-card"
           (img :src src :alt "" :loading "lazy")
           (span :class "mock-x-domain" *og-preview-domain*))
         (p :class "mock-x-from" (format nil "From ~a" *og-preview-domain*))
         (figcaption "X: 2:1, cropped top and bottom; no title or description"))
       (figure :class "mock mock-fb"
         (div :class "mock-fb-card"
           (img :src src :alt "" :loading "lazy")
           (div :class "mock-fb-text"
             (p :class "mock-fb-domain" *og-preview-domain*)
             (p :class "mock-fb-title" title)
             (p :class "mock-fb-desc" description)))
         (figcaption "Facebook and LinkedIn: 1.91:1"))
       (figure :class "mock mock-discord"
         (div :class "mock-discord-card"
           (p :class "mock-discord-site" "Almighty Lisp")
           (p :class "mock-discord-title" title)
           (p :class "mock-discord-desc" description)
           (img :src src :alt "" :loading "lazy"))
         (figcaption "Discord"))
       (figure :class "mock mock-slack"
         (div :class "mock-slack-card"
           (p :class "mock-slack-site" "Almighty Lisp")
           (p :class "mock-slack-title" title)
           (p :class "mock-slack-desc" description)
           (img :src src :alt "" :loading "lazy"))
         (figcaption "Slack"))))))

(defun render-og-preview-item (image)
  (let ((article (og:image-article image))
        (src (s:static (og:image-static-path image))))
    (ah:</>
     (section :class "item" :id (image-slug image)
       (h2 (a :href (article-href article) (model:article-title article)))
       (p :class "facts"
         (format nil "Title ~dpx, ~d line~:p~:[~;, cut short~]  ·  Subtitle ~d line~:p~:[~;, cut short~]  ·  "
                 (og:image-title-size image) (length (og:image-title-lines image)) (og:image-title-cut image)
                 (length (og:image-dek-lines image)) (og:image-dek-cut image))
         (a :href src (og:image-file image)))
       (if (og:image-drawn-p image)
           (ah:</>
            (<>
              (img :class "full" :src src :width "1200" :height "630" :alt "")
              (render-og-mocks image src)))
           (ah:</> (p :class "problem" "Not drawn.")))))))

(defun page-og-preview (images problem &key slug)
  "IMAGES, each article's preview image, or only SLUG's. PROBLEM is why the
last image couldn't be drawn, or NIL."
  (let ((shown (if slug
                   (remove slug images :key #'image-slug :test-not #'string=)
                   images)))
    (ah:render-to-string
     (ah:</>
      (html :lang "en"
        (head
          (meta :charset "utf-8")
          (meta :name "viewport" :content "width=device-width, initial-scale=1")
          (title "Preview images — Almighty Lisp")
          (meta :name "robots" :content "noindex")
          (style *og-preview-css*))
        (body
          (header
            (h1 "Preview images")
            (p :class "lede"
              (format nil "~d article~:p. An article's image is drawn when its file is saved, into static/magazine/published/og/; reload to see a change."
                      (length images)))
            (when problem
              (ah:</> (p :class "problem" problem)))
            (nav :class "picker"
              (a :href (og-preview-href) :class (if slug "" "on") "All")
              (mapcar (lambda (image)
                        (let ((each (image-slug image)))
                          (ah:</> (a :href (og-preview-href each)
                                    :class (if (equal each slug) "on" "")
                                    each))))
                      images)))
          (mapcar #'render-og-preview-item shown)))))))
