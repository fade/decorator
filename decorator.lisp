;; -*-lisp-*-
(defpackage :decorator
  (:use #:cl
        #:decorator.app-utils
        #:simple-date-time
        #:trivial-download
        #:cl-ppcre
        #:cl-mechanize)
  (:export :-main))

(in-package :decorator)

(defparameter *picture-storage*
  (ensure-directories-exist 
   (merge-pathnames
    (format nil "Desktop_pics/~A/"
            (simple-date-time:YYYYMMDD (simple-date-time:from-universal-time (get-universal-time))))
    (user-homedir-pathname)))
  "datestamped destination directory for the images we are downloading.")

(defparameter *full-wallpaper-path* "https://wallpapers.wallhaven.cc/wallpapers/full/")

(defvar *browser* (make-instance 'browser)
  "this creates a 'browser' object which is used by the rest of the mechanize protocol.")

(defun get-links (&key (url "https://alpha.wallhaven.cc/random"))
  "returns a list of URLs pointing to random wallpapers at wallhaven.cc."
  (fetch url *browser*)
  (let ((results (browser-page *browser*))
        (linkresults nil))
    (dolist (link (page-links results))
      (if (cl-ppcre:scan "^.*\/wallpaper/\\d+$" (link-url link))
          (push (link-url link) linkresults)))
    linkresults))
;; (stp:do-recursively (node results)
;;       (format t "~A~%" node))
;; (dolist (link (page-links results))
;;         (format t "~A~%" (link-uri link)))

(defun getit (link &key (url *full-wallpaper-path*))
  (let* ((seqnumber (first (last (rutils:split-sequence #\/ link))))
         (outname (format nil "~A~A~A" "wallhaven-" seqnumber ".jpg"))
         (outpath (merge-pathnames outname *picture-storage*))
         (fileurl (format nil "~A/~A" url outname)))
    (format t "~&~{~A~%~}" (list seqnumber outname outpath link fileurl))
    (let ((no nil))
      (handler-case (download fileurl outpath)
        (error (c)
          (format t "~&Caught condition, ~A" c)
          (push c no)
          nil)))))

(defun doit (linklist)
  "Given a list of target URL, from wallhaven, get the full sized
wallpaper represented by each one."
  (loop for link in linklist
        for thing = (getit link)
        :collect thing))

(defun -main (&optional args)
  (doit (get-links)))
