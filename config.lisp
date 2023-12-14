(in-package :decorator)

;; determine what log info we see.
(log4cl:log-config :debug)

(defparameter *picture-base-storage*
  (ensure-directories-exist
   (merge-pathnames
    "Desktop_pics/"
    (user-homedir-pathname))))

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

(defun funcfact (n)
  (if (<= n 0)
    1
    (* (n (fact (1- n))))))

(defun no-hidden-directories (&key (dir *picture-base-storage*))
  "given a directory :dir, return a list of all non hidden
subdirectories."
  (let ((files (uiop:subdirectories dir)))
    (remove-if (lambda (it)
                 (search "/." (namestring it)))
               files)))

(defparameter *full-wallpaper-path* "https://w.wallhaven.cc/full/"
  "this path needs to be rectified with a bucket number and a filename.")

(defparameter *what-we-already-have* (make-hash-table :test #'equal :size 200000 :synchronized t))

(defparameter *filehash-storage* (make-hash-table :test 'equalp :synchronized t))

;; (eval-when (:load-toplevel :execute)
;;   (when (<= (hash-table-count *what-we-already-have*) 0)
;;     (dolist (x (no-hidden-directories))
;;       (load-pool :path x))))

(defun load-hashes ()
  (dolist (x (no-hidden-directories))
    (lparallel-load-pool :path x)))

(eval-when (:load-toplevel :execute)
  (when (<= (hash-table-count *what-we-already-have*) 0)
    (load-hashes)))
