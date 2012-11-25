;;;; com.elbeno.vector.asd

(asdf:defsystem #:com.elbeno.vector
  :version "0.1"
  :serial t
  :description "A simple package to do 2D vector math."
  :author "Ben Deane <lisp@elbeno.com>"
  :license "GPLv3"
  :components ((:file "package")
               (:file "vector")))
