
(body
 (div :class "sidebar-container"
 (div :class "book-navigation"
 (h1 :class "book-navigation__book-title" "Almighty Lisp: The Essentials")
 (p :class "book-navigation__chapter-name-heading"
 (span "Almighty Lisp: The Essentials"))
 (nav
 (ul
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#quicklisp"
 (span :class "book-navigation__section-name" "QUICKLISP")
 (span :class "book-navigation__section-number" "1.1.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#using-quicklisp"
 (span :class "book-navigation__section-name" "Using Quicklisp")
 (span :class "book-navigation__section-number" "1.2.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#local-code"
 (span :class "book-navigation__section-name" "Local Code")
 (span :class "book-navigation__section-number" "1.3.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#ultralisp"
 (span :class "book-navigation__section-name" "ULTRALISP")
 (span :class "book-navigation__section-number" "1.4.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#qlot"
 (span :class "book-navigation__section-name" "QLOT")
 (span :class "book-navigation__section-number" "1.5.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#ocicl"
 (span :class "book-navigation__section-name" "OCICL")
 (span :class "book-navigation__section-number" "1.6.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#vend"
 (span :class "book-navigation__section-name" "VEND")
 (span :class "book-navigation__section-number" "1.7.0")))))
 (div :class "book-navigation__previous-and-next")))
 (main :class "sidebar-main"
 (article :class "book"
 (section :id "the-ecosystem"
 (hgroup
 (span)
 (h1 "THE ECOSYSTEM"))
 (div :class "outline-text-2" :id "text-1")
 (section :id "quicklisp"
 (hgroup
 (span)
 (h2 "QUICKLISP"))
 (div :class "outline-text-3" :id "text-1-1"
 (p "There is a surprising amount of open source code available for Lisp. The most
common way to obtain it is through "
 (code "quicklisp") ", the defacto-standard library
manager for Lisp. As I said before, ASDF by default uses quicklisp to download
systems defined in the "
 (code ":depends-on") " portion of the "
 (code "defsystem") " if no local copy
exists (in the "
 (code "~/quicklisp") " folder). When you want third-party libraries, you
will likely begin by using quicklisp."))
 (section :id "using-quicklisp"
 (hgroup
 (span)
 (h3 "Using Quicklisp"))
 (div :class "outline-text-4" :id "text-1-1-1"
 (p "Using quicklisp is simple. Let's say you want to load the "
 (code "local-time") " system.
To do that:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(ql:quickload \"local-time\")"))
 (p "If you don't already have "
 (code "local-time") " in your "
 (code "~/quicklisp") " folder, quicklisp
will download it from the quicklisp repository. After downloading it, it will
then load the system as if you had run "
 (code "asdf:load-system") ". To text it, try
running "
 (code "(local-time:now)") " in the REPL.")))
 (section :id "local-code"
 (hgroup
 (span)
 (h3 "Local Code"))
 (div :class "outline-text-4" :id "text-1-1-2"
 (p "Quicklisp will download and load code from the "
 (code "quicklisp") " \"dist\". What if you
want to load your own local code? How do you create your own library that you
can add to a "
 (code ".asd") " file and load seamlessly like any other library?")
 (p "To do that with Quicklisp, you can add code to either the "
 (code "~/common-lisp") " or"
 (code "~/quicklisp/local-projects") " folders.")
 (p "I've personally had trouble with Quicklisp loading code from these folders,
rather than the code I've vendored (using "
 (code "vend") ", introduced below). As a
result, I don't like using either of these folders, but you might have better
results than I did."))))
 (section :id "ultralisp"
 (hgroup
 (span)
 (h2 "ULTRALISP"))
 (div :class "outline-text-3" :id "text-1-2"
 (p "Quicklisp is a repository that is only infrequently updated (by one guy). As a
result, you might not be able to get the latest version of a project, or it may
never be available at all on Quicklisp.")
 (p "An alternative to Quicklisp is "
 (code "ultralisp") ". Ultralisp is a \"dist\" that is
installed on Quicklisp (which has its own default \"dist\" named "
 (code "quicklisp") ") and
that is updated every five minutes. If you go to "
 (a :href "https://ultralisp.org" "https://ultralisp.org") " you can
get instructions on how to install and use it. I haven't ever used it, so I
can't give my opinion one way or the other.")
 (p "In passing, I've seen people have trouble using Ultralisp because they may
inadvertently install different versions of the same library and getting
Quicklisp (the library manager, not the dist) to load one or the other can be a
bit troublesome. This might be similar to the problems I've had with using local
code with Quicklisp as I explained above.")))
 (section :id "qlot"
 (hgroup
 (span)
 (h2 "QLOT"))
 (div :class "outline-text-3" :id "text-1-3"
 (p
 (code "Qlot") " is an alternative library dependency library, found at"
 (a :href "https://qlot.tech/" "https://qlot.tech/") ". It is designed to essentially solve \"versioning\" problems
above. It allows you to pin your dependencies to certain versions of
libraries–as found on the Quicklisp \"dist\".")
 (p "Qlot is worthy of taking a closer look at. It includes useful command-line
commands and library dependencies are defined in separate "
 (code "qlfile") " and"
 (code "qlfile.lock") " files, making it feel a bit more familiar to library management
systems found in other ecosystems. After getting set up, you might find it more
comfortable to use than Quicklisp or Ultralisp.")))
 (section :id "ocicl"
 (hgroup
 (span)
 (h2 "OCICL"))
 (div :class "outline-text-3" :id "text-1-4"
 (p
 (code "ocicl") " is another alternative to Quicklisp that does more than just package
management. It also does linting and has project scaffolding capabilities. It's
developed by a programmer at Red Hat. It's Github page states:")
 (blockquote
 (p "The main innovation behind ocicl is the idea of applying the ecosystem of
tooling and services from the world of application container images to ordinary
tarballs of Lisp code. In essence, OCI + CL = ocicl."))
 (p "If you know what it means for software to be packaged as \"OCI-compliant
artifacts\", you might find ocicl of interest. I'm not sophisticated enough to
understand the value proposition, so I've never used it.")))
 (section :id "vend"
 (hgroup
 (span)
 (h2 "VEND"))
 (div :class "outline-text-3" :id "text-1-5"
 (p
 (code "vend") " ("
 (a :href "https://github.com/fosskers/vend" "https://github.com/fosskers/vend") ") is my preferred alternative to
Quicklisp. The vend philosophy is this: vendor dependencies, and make that
simple.")
 (p "Unlike the other alternatives above, it makes no use of either Quicklisp the
tool or the \"dist\". Instead, it downloads dependencies directly from git
repositories. It has its own repository–just a list of libraries and their git
repo link.")
 (p "The marketing copy from the "
 (code "vend") " Github readme states:")
 (blockquote
 (p "Why vend?")
 (p "Dependency management in Common Lisp has traditionally centred around Quicklisp.
A desire for access to more rapid package updates spawned Ultralisp. The need
for version pinning and isolation birthed Qlot. The want for a better
distribution system brought us ocicl.")
 (p "But, could there be a simpler paradigm than just downloading the code and
putting it right there?")
 (p "With vend:")
 (p "We need not perform bespoke installation scripts to get started. We need not
wait for Quicklisp to update. We need not relegate all our systems to
~/common-lisp/. We need not worry about where ASDF is looking for systems. We
need not fret over tools performing strange internal overrides. We need not
manage extra config files or lockfiles. Plus, vend is actually an external tool
with extra commands to help you inspect and manage your dependencies."))
 (p "If we, as almighty developers, want to defeat the machines, we need to
consciously seek ways of doing more work in fewer steps. Finding and using the
simplest way to do things is a matter of survival. Vend is an important for
simplifying our tech stack and empowering us to do more. That's why for projects
in the next chapter, we will be using "
 (code "vend") ".")
 (p "I will explain how to install and use "
 (code "vend") " when it becomes necessary."))))))
 (button :class "toc-viewer" "TOC")
 (script "HighlightLisp.highlight_auto();"))