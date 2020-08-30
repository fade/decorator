;; -*-lisp-*-
;;;; decorator.asd

(asdf:defsystem #:decorator
  :description "Download some high resolution wallpapers blindly. A graphical chocolate box, really."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Lisp Lesser General Public License."
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
               #:zpb-exif
               #:net.didierverna.clon
               #:mcclim
               ;; :cl-progress-bar
               )
  :pathname "./"
  :components ((:file "app-utils")
               (:file "decorator")))

