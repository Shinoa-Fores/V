(in-package "CL-GPG")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions

;; credit: phf and trinque
(defun make-temp-dir (path)
  "Makes a temporary directory by calling the foreign function mkdtemp.

Returns the NAMESTRING of the path of the temporary directory.

Raises WOT-DIR-CREATION of the directory creation fails."
  #+sbcl (let ((noerrors nil)
               (p nil))
           (ignore-errors
             (setq p (sb-posix:mkdtemp (namestring path)))
             (setq noerrors t))
           (if noerrors
               p
               (error 'wot-dir-creation :text (namestring path))))
  #+ccl (ccl:with-cstrs ((s (namestring path)))
          (if (ccl:%null-ptr-p (#_mkdtemp s))
              (error 'wot-dir-creation :text (namestring path))
              (ccl:%get-cstring s))))

(defun run-subprocess (program args)
  #+sbcl (sb-ext:process-exit-code (sb-ext:run-program program args))
  #+ccl (nth-value 1 (ccl:external-process-status
                      (ccl:run-program program args)))
  #-(or :sbcl :ccl) (error 'unsupported-cl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition of a wot

(defstruct wot
  (basename "" :type string)
  (homedir "" :type string)
  (key-names '() :type list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error conditions

(define-condition bad-seal (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if gpg detects a bad signature.")
  (:report (lambda (condition stream)
             (format stream
                     "GnuPG failed to verify seal ~S."
                     (text condition)))))

(define-condition no-seal (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if a vpatch has no seal.")
  (:report (lambda (condition stream)
             (format stream
                     "Failed to find a seal for vpatch ~S."
                     (text condition)))))

(define-condition keyring-parent-dir-dne (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if the parent directory for the temporary
keyring dir does not exist.")
  (:report (lambda (condition stream)
             (format stream
                     "Keyring parent directory not found at location ~S."
                     (text condition)))))

(define-condition wot-dir-creation (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if a wot dir does not exist.")
  (:report (lambda (condition stream)
             (format stream
                     "Failed to make temporary WoT directory: ~S."
                     (text condition)))))

(define-condition wot-dir-dne (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if a wot dir does not exist.")
  (:report (lambda (condition stream)
             (format stream
                     "WoT directory not found at location ~S."
                     (text condition)))))

(define-condition bad-public-key (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if gpg failes to load a public key.")
  (:report (lambda (condition stream)
             (format stream
                     "GnuPG failed to import key ~S."
                     (text condition)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creating a wot

(defparameter *default-keyring-dir-template* "gpgXXXXXX")

(defun create-wot (wot-dir
                   keyring-parent-dir
                   &key
                     (keyring-dir-template *default-keyring-dir-template*))
  "Generates a gpg keyring under KEYRING-PARENT-DIR, loading as keys
in all files in WOT-DIR ending in .asc .

Returns a WOT object corresponding to the generated keyring.

Raises WOT-DIR-DNE if WOT-DIR does not exist.

Raises KEYRING-PARENT-DIR-DNE if KEYRING-PARENT-DIR does not exist. Note that
this procedure attempts to create KEYRING-PARENT-DIR if it does not exist.

Raises BAD-PUBLIC-KEY if gpg fails to import a key."
  (if (not (probe-file wot-dir))
      (error 'wot-dir-dne :text wot-dir))
  (if (not (probe-file keyring-parent-dir))
      (error 'keyring-dir-dne :text keyring-parent-dir))
  (let ((homedir (make-temp-dir
                  (merge-pathnames (make-pathname :name
                                                  keyring-dir-template)
                                   keyring-parent-dir))))
    (make-wot
     :homedir homedir
     :basename (file-namestring homedir)
     :key-names (mapcar #'(lambda (w)
                            (let ((name (namestring w)))
                              (if (not (eq 0
                                           (run-subprocess
                                            *gpg-location*
                                            (list "--homedir"
                                                  homedir
                                                  "--import"
                                                  name))))
                                  (error 'bad-public-key :text name)
                                  (file-namestring name))))
                        (directory
                         (make-pathname :directory `(:relative ,(namestring wot-dir))
                                        :name :wild
                                        :type "asc"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove a wot

(defun remove-wot (wot)
  (run-subprocess *rm-location* (list "-rf" (wot-homedir wot))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using a wot, check the validity of a signature against a file

(defmethod check-trust-on-wot (filepath sealpath wot)
  (format t "Checking ~a against ~a ... "
          (file-namestring filepath)
          (file-namestring sealpath))
  (if (not (eq 0
               (run-subprocess
                *gpg-location*
                (list
                 "--homedir"
                 (wot-homedir wot)
                 "--verify"
                 (namestring sealpath)
                 (namestring filepath)))))
      (error 'bad-seal :text (file-namestring sealpath))
      (format t "passed. ~%")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matching vpatches to seals by file name

(defun split-string (string delimeter)
  (with-input-from-string (s string)
    (do ((c (read-char s nil 'end)
            (read-char s nil 'end))
         (curr nil)
         (rtn nil))
        ((equal c 'end)
         (progn
           (if curr (push (coerce (nreverse curr) 'string) rtn))
           (nreverse rtn)))
      (if (char-equal c delimeter)
          (progn
            (push (coerce (nreverse curr) 'string) rtn)
            (setq curr nil))
          (progn
            (push c curr))))))

(defun find-seals-for-vpatch (vpatch-pathname seal-dir)
  (labels ((name-before-first-period (string)
             (subseq string 0 (position #\. string))))
    (let ((vpatch-name (name-before-first-period (pathname-name vpatch-pathname))))
      (remove-if-not #'(lambda (p) (string= (name-before-first-period
                                             (pathname-name p))
                                            vpatch-name))
                     (directory (make-pathname :directory
                                               (pathname-directory seal-dir)
                                               :name :wild
                                               :type "sig"))))))

(defun check-trust (pathname-list seal-dir wot-dir)
  (let ((wot (create-wot wot-dir
                         (make-temp-dir (namestring
                                         (make-pathname
                                          :directory `(:relative ,*tmp-location*)
                                          :name "gpgXXXXXX"))))))
    (dolist (p pathname-list)
      (let ((seals (find-seals-for-vpatch p seal-dir)))
        (if (null seals)
            (error 'no-seal :text (pathname p))
            (dolist (v (find-seals-for-vpatch p seal-dir))
              (check-trust-on-wot p v wot)))))))
