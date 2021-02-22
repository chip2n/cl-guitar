(asdf:defsystem #:guitar
  :description "Generate guitar diagrams"
  :author "Andreas Arvidsson <andreas@arvidsson.io>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:savage #:alexandria #:chiputils #:arrow-macros #:serapeum)
  :components ((:file "package")
               (:file "core")))
