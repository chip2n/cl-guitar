(asdf:defsystem #:cl-guitar
  :description "Guitar practice tool"
  :author "Andreas Arvidsson <andreas@arvidsson.io>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who)
  :components ((:file "package")
               (:file "cl-guitar")))
