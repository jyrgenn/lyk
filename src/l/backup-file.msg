From: Ralph Schleicher <rs+usenet@ralph-schleicher.de>
Subject: Re: open :direction :output :if-exists :rename
Date: 20. April 2016 at 00:24:29 GMT+2


taruss@google.com writes:

But the real answer is that if you want to manage the file actions in detail,
you will probably have to implement your own management strategy.

Here's a quick and dirty transliteration from some existing C code
of mine:

(defun backup-file (file-name &optional method)
 "Backup a file.

First argument FILE-NAME is the file to be saved.
Second argument METHOD is the backup method.  Value is either the
keyword :simple, :numbered, :existing, or :none.  Default is to
infer the backup method from the value of the 'VERSION_CONTROL'
environment variable.  See the Emacs documentation for more
details.

When this function returns, file FILE-NAME is out of the way."
 (let ((backup (backup-file-name file-name method)))
   (if (null backup)
       (delete-file file-name)
     (progn
       (ignore-errors
        (delete-file backup))
       (rename-file file-name backup)))))

(defun backup-file-name (file-name &optional method)
 "Return the backup file name for FILE-NAME."
 (when (or (null method) (eq method :default))
   (let ((env (uiop:getenv "VERSION_CONTROL")))
     (setf method (cond ((null env)
                         :existing)
                        ((or (string= env "simple")
                             (string= env "never"))
                         :simple)
                        ((or (string= env "numbered")
                             (string= env "t"))
                         :numbered)
                        ((or (string= env "existing")
                             (string= env "nil"))
                         :existing)
                        ((or (string= env "none")
                             (string= env "off"))
                         :none)
                        (t
                         ;; Any other value.
                         :existing)))))
 (ecase method
   (:simple
    (simple-backup-file-name file-name))
   (:numbered
    (numbered-backup-file-name file-name t))
   (:existing
    (numbered-backup-file-name file-name))
   (:none
    nil)))

;; TODO: Check if FILE-NAME is a directory.
(defun simple-backup-file-name (file-name)
 (concatenate 'string file-name "~"))

(defun numbered-backup-file-name (file-name &optional force)
 (let* ((dir (uiop:pathname-directory-pathname file-name))
        (base (file-namestring file-name))
        ;; Minimum file name length of a numbered backup file name.
        (len (length base))
        (minlen (+ len 4))
        ;; Highest version number.
        (vernum (if force 0 -1)))
   (iter (for dirent :in (uiop:directory-files dir))
         (for ent = (file-namestring dirent))
         (when (or (< (length ent) minlen)
                   (string/= ent base :end1 len)
                   (char/= (char ent len) #\.)
                   (char/= (char ent (1+ len)) #\~)
                   (not (digitp (char ent (+ len 2)))))
           (next-iteration))
         (for pos = (+ len 3))
         (for end = (length ent))
         (iter (while (and (< pos end) (digitp (char ent pos))))
               (incf pos))
         (unless (and (= (1+ pos) end) (char= (char ent pos) #\~))
           (next-iteration))
         ;; Found a numbered backup file.
         (for num = (parse-integer ent :start (+ len 2) :end pos))
         (when (> num vernum)
           (setf vernum num)))
   ;; Make numbered backups for files that have numbered backups.
   ;; Otherwise, make single backups.
   (if (minusp vernum)
       (simple-backup-file-name file-name)
     (concatenate 'string file-name (format nil ".~~~D~~" (1+ vernum))))))

(defun digitp (c)
 (and (standard-char-p c)
      (digit-char-p c)))

-- 
Ralph

