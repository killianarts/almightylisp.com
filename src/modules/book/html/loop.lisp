
(body
 (div :class "sidebar-container"
 (div :class "book-navigation"
 (h1 :class "book-navigation__book-title")
 (p :class "book-navigation__chapter-name-heading"
 (span))
 (nav
 (ul))
 (div :class "book-navigation__previous-and-next")))
 (main :class "sidebar-main"
 (article :class "book"))
 (button :class "toc-viewer" "TOC")
 (script "HighlightLisp.highlight_auto();"))