;;;; spacomic.asd

(asdf:defsystem #:spacomic
  :description "Describe spacomic here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:croatoan #:swank)
  :components ((:file "package")
               (:file "spacomic")))
