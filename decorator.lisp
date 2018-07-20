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

;;===============================================================================
;; File functions and state to keep the images and their  directories trim.
;;===============================================================================

(defun sha1-file (path)
  (let ((sha1 (ironclad:make-digest 'ironclad:sha1)))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (ironclad:update-digest sha1 stream)
      (values (ironclad:byte-array-to-hex-string (ironclad:produce-digest sha1)) path))))

(defun load-pool (&key (path *aggregate-storage-directory*))
  ;; for every file in the given directory (path), calculate its hash
  ;; and put it in the special table
  (loop for file in (uiop:directory-files path)
        for (hash path) = (multiple-value-list (sha1-file file))
        :do (progn
              (format t "~&~A :: ~A" hash path)
              (setf (gethash hash *what-we-already-have*) path)
              (format t "     [Done]"))))

(defparameter *picture-storage*
  (ensure-directories-exist 
   (merge-pathnames
    (format nil "Desktop_pics/~A/"
            (simple-date-time:YYYYMMDD (simple-date-time:from-universal-time (get-universal-time))))
    (user-homedir-pathname)))
  "datestamped destination directory for the images we are downloading.")

(defparameter *aggregate-storage-directory*
  (ensure-directories-exist
   (merge-pathnames
    (format nil "Desktop_pics/dpool/"))))

(defparameter *full-wallpaper-path* "https://wallpapers.wallhaven.cc/wallpapers/full/")

(defparameter *what-we-already-have* (make-hash-table :test #'equal))

(when (= (hash-table-count *what-we-already-have*) 0)
  (load-pool))

;;===============================================================================


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

(defun pprint-download (furl opath &key (stream t))
  "Download via the cl-mechanize download feature, but emit
  appropriate wrapping newlines, so console output is not confused."
  (format stream "~&~%")
  (let ((carbuncle (download furl opath))) 
    (format stream "~&")
    carbuncle))

(defun already-got-it? (file &key (hashtable *what-we-already-have*))
  (multiple-value-bind (hash path) (sha1-file file)
    (if (gethash hash hashtable)
        path
        nil)))

(defun delete-duplicate-file (file &key (hashtable *what-we-already-have*))
  (multiple-value-bind (hash path) (sha1-file file)
      (when (gethash hash hashtable)
        (progn
          (format t "~&Unlinking duplicate file ~A which already exists at ~A~%" path (gethash hash hashtable))
          (delete-file path)))))

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
          (pprint-download fileurl outpath)
          ;; if the file lands, but we already have it, delete it.
          (let* ((wegotit (already-got-it? outpath)))
            (if wegotit
                (delete-duplicate-file wegotit))))
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
                (progn
                  (let* ((wegotit (already-got-it? outpath)))
                    (format t "~&!!!!!!!!!!!!!!!!!! [~A] !!!!!!!!!!!!!!!!!!" outpath)
                    
                    (when wegotit
                      (format t "~&!!!!!!!!!!!!!!!!!! [~A] !!!!!!!!!!!!!!!!!!" wegotit)
                      (delete-duplicate-file wegotit)))
                  (format t "~&~{~A~^~%~}~%================================================================[[DONE]]~%"
                          (list "sequence number:" seqnumber "output name:" outname "output path:" outpath "index link:" link "actual URL:" fileurl)))
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

;;===============================================================================


;;===============================================================================

(defun -main (&optional args)
  (declare (ignorable args))
  (load-pool)
  (doit (get-links)))
