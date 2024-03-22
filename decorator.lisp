(defpackage :decorator
  (:use #:cl
        #:decorator.app-utils
        #:simple-date-time
        #:trivial-download
        #:cl-ppcre
        #:cl-interpol
        #:cl-mechanize
        #:bordeaux-threads
        #:zpb-exif
        #:net.didierverna.clon
        ;; #:cl-progress-bar
        )
  (:export :-main))

(in-package :decorator)

;;===============================================================================
;; File functions and state to keep the images and their directories trim.
;;===============================================================================

;;; set the lparallel kernel to a pool of 22 threads, because my
;;; development workstation has a lot of CPU
;; (setf lparallel:*kernel* (lparallel:make-kernel 22))

(defun sha1-file (path)
  (let ((sha1 (ironclad:make-digest 'ironclad:sha1)))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (ironclad:update-digest sha1 stream)
      (values (ironclad:byte-array-to-hex-string (ironclad:produce-digest sha1)) path))))

(defun log-file-hashes (&key (path *aggregate-storage-directory*)
                          (output-file #P "/tmp/slow-swill.txt"))
  (with-open-file (s output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop
      for count from 1
      for file in (uiop:directory-files path)
      for (hash path) = (multiple-value-list (sha1-file file))
      :do (format s "~&[~6d] ~A :: ~A" count hash path))))

(defun lparallel-load-pool (&key (path *aggregate-storage-directory*))
  (setf lparallel:*kernel* (lparallel:make-kernel *threads*))
  (let ((count 0))
    (lparallel:pmapcar
     (lambda (file)
       (multiple-value-bind (hash path) (sha1-file file)
         (unless (gethash hash *what-we-already-have*)
           (format t "~&[~6d] ~A :: ~A" count hash path)
           (setf (gethash hash *what-we-already-have*) path)
           (incf count))))
     (uiop:directory-files path))
    (lparallel:end-kernel)))

(defun load-pool (&key (path *aggregate-storage-directory*))
  "for every file in the given directory :path, calculate its hash
  and put it in the special table"
  (format t "~&Loading hashes for pre-existing files in ~A~%" path)
  (loop
    for count from 1
    for file in (uiop:directory-files path)
    for (hash path) = (multiple-value-list (sha1-file file))
    :unless (gethash hash *what-we-already-have*)
      :do (progn
            ;; (log:info "~&[~6d] ~A :: ~A" count hash path)
            (setf (gethash hash *what-we-already-have*) path)))
  (log:info "~&[Done]~%"))

(defun finser-aux (path)
  "Given a hash and a path, do the mutation."
  (multiple-value-bind (hash path) (sha1-file path)
    (cond ((gethash hash *what-we-already-have*)
           (format t "~&We already have ~A" path))
          (t
           (format t "~&~A :: ~A" hash path)
           (setf (gethash hash *what-we-already-have*) path)))))

;;===============================================================================

(defvar *browser* (make-instance 'browser)
  "this creates a 'browser' object which is used by the rest of the mechanize protocol.")

(defun get-random-links (&key (url "https://wallhaven.cc/random") (page 1))
  "returns a list of URLs pointing to random wallpapers at wallhaven.cc."
  (enable-interpol-syntax)
  (let* ((url (format nil "~A?page=~A" url page)))
    (fetch url *browser*)
    (let ((results (browser-page *browser*))
          (linkresults nil))
      (dolist (link (page-links results))
        (if (cl-ppcre:scan "^.*/w/.*" (cl-mechanize::link-url link))
            (push (cl-mechanize::link-url link) linkresults)))
      (disable-interpol-syntax)
      linkresults)))


;; https://wallhaven.cc/search?q=city+night+woman+blue&categories=110&purity=100&sorting=relevance&order=desc
;; https://wallhaven.cc/search?q=city%20night%20woman&categories=110&purity=100&sorting=relevance&order=desc&page=2
;; https://wallhaven.cc/search?categories=101&purity=101&atleast=1920x1080&topRange=1y&sorting=toplist&order=desc&colors=0066cc&ai_art_filter=0
(defun get-searched-links (searchterms &key (categories 101)
                                         (purity 101)
                                         (atleast "1920x1080")
                                         (topRange "1y")
                                         (sorting "toplist")
                                         (colors "0066cc")
                                         (ai_art_filter 0)
                                         (url "https://wallhaven.cc/search") (page 1))
  "Return any images matched by wallhaven to the given searchterms."
  (enable-interpol-syntax)
  (let* ((searchterms (cond
                        ((and (stringp searchterms) (find #\Space searchterms))
                         (str:join "+" (str:split " " searchterms :omit-nulls t)))
                        ((and (listp searchterms) (> (length searchterms) 1))
                         (str:join "+" searchterms))
                        (t searchterms)))
         (url (format nil "~A?q=~A&categories=~A&purity=~A&atleast=~A&topRange=~A&order=desc&colors=~A&ai_art_filter=~A&page=~A" url searchterms categories purity atleast toprange colors ai_art_filter page)))
    (format t "~&|||~A~2%" url)
    (fetch url *browser*)
    (let ((results (browser-page *browser*))
          (linkresults nil))
      (dolist (link (page-links results))
        (if (cl-ppcre:scan "^.*/w/.*" (cl-mechanize::link-url link))
            (push (cl-mechanize::link-url link) linkresults)))
      (disable-interpol-syntax)
      (alexandria:shuffle linkresults))))

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
        (format t "~&Unlinking duplicate file ~A which already exists at ~A~%~%" path (gethash hash hashtable))
        (delete-file path)))))

(defun getit (link &key (url *full-wallpaper-path*) (type "jpg"))
  (let* ((*picture-storage* (ensure-directories-exist 
                             (merge-pathnames
                              (format nil "Desktop_pics/~A/"
                                      (simple-date-time:YYYYMMDD (simple-date-time:from-universal-time (get-universal-time))))
                              (user-homedir-pathname))))
         (seqnumber (first (last (rutils:split-sequence #\/ link))))
         (bucket (subseq seqnumber 0 2))
         (outname (format nil "~A~A.~A" "wallhaven-" seqnumber type))
         (outpath (merge-pathnames outname *picture-storage*))
         (fileurl (format nil "~A~A/~A" url bucket outname)))
    
    (handler-case
        (progn
          (format t "~&~%vvv=====================================================================~%~&Sequence number: ~0,9D ~%~{~A~^ ~%~}~%"
                  seqnumber (list "output name:" outname "output path:" outpath "index link:" link "actual URL:" fileurl))
          (pprint-download fileurl outpath)
          ;; if the file lands, but we already have it, delete it.
          (let* ((wegotit (already-got-it? outpath))) ;; store this here, so we don't ask twice.
            (when wegotit
              (delete-duplicate-file wegotit))))
      ;; in this case, the file is probably not a jpg. try png.
      (HTTP-ERROR (c)
        (progn
          (format t "~&>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~%Caught condition, ~A~%<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~%" c)
          (format t "~&Attempting to download PNG instead ")
          (let* ((outname (format nil "~A~A.~A" "wallhaven-" seqnumber "png"))
                 (bucket (subseq seqnumber 0 2))
                 (outpath (merge-pathnames  outname *picture-storage*))
                 (fileurl (format nil "~A~A/~A" url bucket outname)))
            (format t "::: ~A~%" fileurl)
            (if (pprint-download fileurl outpath)
                (progn
                  (let* ((wegotit (already-got-it? outpath)))
                    (format t "~&!!!!!!!!!!!!!!!!!! [~A] !!!!!!!!!!!!!!!!!!" outpath)
                    (when wegotit
                      (format t "~&!!!!!!!!!!!!!!!!!!Deleting duplicate file: [~A] !!~%~%" wegotit)
                      (delete-duplicate-file wegotit)))
                  (format t "~&sequence number: ~0,9D ~%~{~A~^ ~%~}~%========================================================================~%"
                          seqnumber (list "output name:" outname "output path:" outpath "index link:" link "actual URL:" fileurl)))
                (error "Failed to get fallback png for image link:: ~A" fileurl)))))
      
      (error (c)
        (progn
          (format t "Unthinkable things happened here... :: ~A~%" c)
          (format t "~&sequence number: ~0,9D ~%~{~A~^~%~}~%========================================================================~%"
                  seqnumber (list "output name:" outname "output path:" outpath "index link:" link "actual URL:" fileurl)))))))

(defun merge-image-directories (temppath &optional (storepath *aggregate-storage-directory*))
  "Take all the files in temppath, and move them into storepath.
Assuming the directory has been filled using the function protocol
defined in this file, then there will be no name collisions. All the
files should be unique to 'storepath"
  (let ((files (uiop:directory-files temppath)))
    (loop for candidate in files
          for outfile = (merge-pathnames (file-namestring candidate)
                                         storepath)
          do
             (progn
               (format t "~&Moving ~A to ~A... " candidate outfile)
               (uiop:copy-file candidate outfile)
               (uiop:delete-file-if-exists candidate)
               (format t "[Done]")))))

(defun doit (linklist)
  "Given a list of target URL, from wallhaven, get the full sized
wallpaper represented by each one."
  ;; (mapc #'getit linklist)
  (loop for link in linklist
        for thing = (getit link)
        :when thing
          :collect thing))

;;===============================================================================
;; Command Line Interface options and setup
;;===============================================================================

(net.didierverna.clon:defsynopsis ()
  (text :contents "This program takes search terms relative to
  background image art held at wallhaven.cc, and downloads the
  returned results in bulk. If no search terms are provided, then the
  program will download a series of random images.")
  (group (:HEADER "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version number and exit."))
  (group (:HEADER "Search options")
         (stropt :short-name "s" :long-name "search"
                 :description "Search terms for desktop background images... ")
         (stropt :short-name "p" :long-name "page"
                 :description "take the 'N'th page of a given search."
                 :argument-name "PAGE"
                 :default-value "1")))

(defun -main (&optional args)
  (declare (ignorable args))
  (make-context)
  (let ((searchterms  (str:split " " (net.didierverna.clon:getopt :short-name "s")))
        (page (parse-integer (getopt :short-name "p") :junk-allowed t)))

    (net.didierverna.clon:do-cmdline-options 
        (option name value source)

      (cond ((or (string= name "h") (string= name "help"))
             (terpri)
             (help)
             (terpri)
             (exit 0))
            ((or (string= name "v") (string= name "version"))
             (terpri)
             (format t "~&[[ ~A : ~A ]]~2%" (lisp-implementation-type) (lisp-implementation-version))
             (exit 0))))

    (if searchterms
        (progn
          (format t "getting searched images...~2%")
          (doit (get-searched-links searchterms :page page)))
        (progn 
          (format t "getting random images...~2%")
          (doit (get-random-links))))))
