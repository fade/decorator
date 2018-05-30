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
      (if (cl-ppcre:scan "^.*\/wallpaper/\\d+$" (cl-mechanize::link-url link))
          (push (cl-mechanize::link-url link) linkresults)))
    linkresults))
;; (stp:do-recursively (node results)
;;       (format t "~A~%" node))
;; (dolist (link (page-links results))
;;         (format t "~A~%" (link-uri link)))

(defun pprint-download (furl opath &key (stream t))
  "Download via the cl-mechanize download feature, but emit
  appropriate wrapping newlines, so console output is not confused."
  (format stream "~&~%")
  (let ((carbuncle (download furl opath))) 
    (format stream "~&")
    carbuncle))

(defun getit (link &key (url *full-wallpaper-path*) (type "jpg"))
  (let* ((*picture-storage* (ensure-directories-exist 
                             (merge-pathnames
                              (format nil "Desktop_pics/~A/"
                                      (simple-date-time:YYYYMMDD (simple-date-time:from-universal-time (get-universal-time))))
                              (user-homedir-pathname))))
         (seqnumber (first (last (rutils:split-sequence #\/ link))))
         (outname (format nil "~A~A.~A" "wallhaven-" seqnumber type))
         (outpath (merge-pathnames outname *picture-storage*))
         (fileurl (format nil "~A~A" url outname)))
    
    (handler-case
        (progn
          (format t "~&~%========================================================================~%~&~{~A~^~%~}~%"
                  (list "sequence number:" seqnumber "output name:" outname "output path:" outpath "index link:" link "actual URL:" fileurl))
          (pprint-download fileurl outpath))
      ;; in this case, the file is probably not a jpg. try png.
      (HTTP-ERROR (c)
        (progn
          (format t "~&>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~%Caught condition, ~A~%<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~%" c)
          (format t "~&Attempting to download PNG instead ")
          (let* ((outname (format nil "~A~A.~A" "wallhaven-" seqnumber "png"))
                 (outpath (merge-pathnames  outname *picture-storage*))
                 (fileurl (format nil "~A~A" url outname)))
            (format t "::: ~A~%" fileurl)
            (if (pprint-download fileurl outpath)
                (format t "~&~{~A~^~%~}~%================================================================[[DONE]]~%"
                        (list "sequence number:" seqnumber "output name:" outname "output path:" outpath "index link:" link "actual URL:" fileurl))
                (error "Failed to get fallback png for image link:: ~A" fileurl)))))
      (error (c)
        (progn
          (format t "Unthinkable things happened here... :: ~A~%" c)
          (format t "~&~{~A~^~%~}~%========================================================================~%"
                  (list "sequence number:" seqnumber "output name:" outname "output path:" outpath "index link:" link "actual URL:" fileurl)))))))

(defun doit (linklist)
  "Given a list of target URL, from wallhaven, get the full sized
wallpaper represented by each one."
  (loop for link in linklist
        for thing = (getit link)
        :when thing
        :collect thing))

(defun -main (&optional args)
  (declare (ignorable args))
  (doit (get-links)))
