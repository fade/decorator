(in-package :decorator)

(defparameter *picture-storage*
  (ensure-directories-exist 
   (merge-pathnames
    (format nil "Desktop_pics/~A/"
            (simple-date-time:YYYYMMDD
             (simple-date-time:from-universal-time (get-universal-time))))
    (user-homedir-pathname)))
  "datestamped destination directory for the images we are downloading.")

(defparameter *aggregate-storage-directory*
  (ensure-directories-exist
   (merge-pathnames
    (format nil "Desktop_pics/dpool/")
    (user-homedir-pathname))))

(defparameter *full-wallpaper-path* "https://w.wallhaven.cc/full/"
  "this path needs to be rectified with a bucket number and a filename.")

(defparameter *what-we-already-have* (make-hash-table :test #'equal :size 100000 :synchronized t))

(eval-when (:load-toplevel :execute)
  (when (<= (hash-table-count *what-we-already-have*) 0)
    (load-pool)))

