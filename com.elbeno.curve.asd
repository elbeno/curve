;;;; com.elbeno.curve.asd

(asdf:defsystem #:com.elbeno.curve
  :version "0.1"
  :serial t
  :description "A library for modulating curves onto curves."
  :author "Ben Deane <lisp@elbeno.com>"
  :license "GPLv3"
  :depends-on (#:vecto
               #:com.elbeno.vector)
  :components ((:file "package")
               (:file "classes") ;; :depends-on ("package"))
               (:file "draw") ;; :depends-on ("package" "classes"))
               (:file "arcs") ;; :depends-on ("package" "classes"))
               (:file "sample") ;; :depends-on ("package" "classes"))
               (:file "modulate") ;; :depends-on ("package" "classes" "sample"))
               (:file "test")));; :depends-on ("package" "classes" "draw" "sample" "modulate"))))
