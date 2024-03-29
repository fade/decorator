;; -*-lisp-*-
;;;; decorator.asd

(asdf:defsystem #:decorator
  :description "Download some high resolution wallpapers blindly. A graphical chocolate box, really."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Lisp Lesser General Public License."
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre
               #:cl-interpol
               #:str
               #:alexandria
               #:ubiquitous
               #:rutils
               #:trivial-download
               #:simple-date-time
               #:cl-mechanize
               #:bordeaux-threads
               #:lparallel
               #:ironclad
               #:log4cl
               #:zpb-exif
               #:defmain
               #:net.didierverna.clon
               ;; #-DARWIN #:mcclim
               ;; :cl-progress-bar
               )
  :pathname "./"
  :components ((:file "app-utils")
               (:file "decorator")
               (:file "config")))

