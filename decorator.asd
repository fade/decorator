;; -*-lisp-*-
;;;; decorator.asd

(asdf:defsystem #:decorator
  :description "Download some high resolution wallpapers blindly. A graphical chocolate box, really."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Lisp Lesser General Public License."
  :serial t
  :depends-on (:cl-ppcre
               :alexandria
               :ubiquitous
               :rutils
               :trivial-download
               :simple-date-time
               :cl-mechanize
               :ironclad
               :zpb-exif
               :osicat
               :cl-progress-bar)
  :pathname "./"
  :components ((:file "app-utils")
               (:file "decorator")))

