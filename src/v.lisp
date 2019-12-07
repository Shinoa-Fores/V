(in-package v)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global tuning parameters

(defparameter *default-vpatch-dir* (make-pathname
                                    :directory '(:relative "patches")))
(defparameter *default-wot-dir* (make-pathname
                                 :directory '(:relative "wot")))
(defparameter *default-seal-dir* (make-pathname
                                  :directory '(:relative "seals")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error conditions


(define-condition cyclic (error)
  ()
  (:documentation "Cycle encountered during topological sort."))

(define-condition no-seal (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if a vpatch has no seal.")
  (:report (lambda (condition stream)
             (format stream
                     "Failed to find a seal for vpatch ~S."
                     (text condition)))))

(define-condition unsupported-cl (error)
  ()
  (:documentation "Unsupported Common Lisp implementation detected."))

(define-condition vpatch-lookup (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised when no vpatch matches a search pattern.")
  (:report (lambda (condition stream)
             (format stream
                     "Failed to find vpatch matching ~S."
                     (text condition)))))

(define-condition hash-failure (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if a Keccak hash of a press file doesn't
match the vpatch hash.")
  (:report (lambda (condition stream)
             (format stream
                     "Hash of file does not match expected: ~S."
                     (text condition)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes

(defclass subpatch ()
  ((path :initarg :path :reader path)
   (pre-hash :initarg :pre-hash :reader pre-hash)
   (post-hash :initarg :post-hash :reader post-hash))
  (:documentation "A subpatch is a patch for a single file."))

(defclass vpatch ()
  ((name :initarg :name :reader name)
   (subpatches :initarg :subpatches :reader subpatches)
   (path :initarg :path :reader path))
  (:documentation "A representation of a vpatch."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printing objects

(defmethod print-object ((obj subpatch) stream)
  (print-unreadable-object (obj stream :type t)
    (labels ((fingerprint (hash-string)
               (if (equal "false" hash-string)
                   "_"
                   (subseq hash-string (- (length hash-string) 4)))))
      (format stream
              "~a -> ~a"
              (fingerprint (pre-hash obj))
              (fingerprint (post-hash obj))))))

(defmethod print-object ((obj vpatch) stream)
  (print-unreadable-object (obj stream :type t)
    (princ (name obj) stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generics

(defgeneric alignedp (obj1 obj2))
(defgeneric alignment (obj1 obj2))
(defgeneric adjacentp (obj1 obj2))
(defgeneric parentp (vp1 vp2))
(defgeneric childp (vp1 vp2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading vpatches

(defun extract-hashes (vpatch-filepath)
  (flet ((last-word (string)
           (subseq string
                   (1+ (position #\Space string :from-end t))))
         (subpatch-start (string)
           (and (>= (length string) 4)
                (equal "diff" (subseq string 0 4)))))
    (let ((hash-list nil))
      (with-open-file (s vpatch-filepath)
        (do ((L (read-line s) (read-line s nil)))
            ((eql L nil))
          (if (subpatch-start L)
              (setq hash-list
                    (cons (mapcar #'last-word
                                  (list L (read-line s) (read-line s)))
                          hash-list)))))
      hash-list)))

(defun make-vpatch (filepath)
  "Given a filepath, MAKE-VPATCH attempts to read its contents
and form a VPATCH object.

Returns the newly created VPATCH object."
  (let ((name (file-namestring (namestring filepath))))
    (make-instance 'vpatch
                   :name name
                   :path filepath
                   :subpatches (mapcar #'(lambda (x)
                                           (make-instance
                                            'subpatch
                                            :path (first x)
                                            :pre-hash (second x)
                                            :post-hash (third x)))
                                       (extract-hashes filepath)))))

(defun load-vpatches (path)
  "Returns a list containing the application of MAKE-VPATCH to
every .vpatch file at PATH."
  (mapcar #'make-vpatch
          (directory (merge-pathnames path
                                      (make-pathname :name :wild
                                                     :type "vpatch")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generating the dependency graph

(defmethod alignedp ((sp1 subpatch) (sp2 subpatch))
  (equal (path sp1) (path sp2)))

(defmethod alignment ((vp1 vpatch) (vp2 vpatch))
  (loop for sp1 in (subpatches vp1)
     append (loop for sp2 in (subpatches vp2)
               if (alignedp sp1 sp2)
               collect (list sp1 sp2))))

(defmethod parentp ((sp1 subpatch) (sp2 subpatch))
  (equal (post-hash sp1) (pre-hash sp2)))

(defmethod childp ((sp1 subpatch) (sp2 subpatch))
  (parentp sp2 sp1))

(defmethod parentp ((vp1 vpatch) (vp2 vpatch))
  (let ((alignment (alignment vp1 vp2)))
    (labels ((parentp-apply (x) (apply #'parentp x)))
      (and (every #'parentp-apply alignment)
           (some #'parentp-apply alignment)))))

(defmethod childp ((vp1 vpatch) (vp2 vpatch))
  (parentp vp2 vp1))

(defun generate-depgraph (vpatch-list)
  "Generate a directed graph from the input list VPATCH-LIST of vpatches.

Returns a LIST whose first member is the input list of vpatches,
and second member is a list of all directed edges (VP1 VP2)
where VP1 is a parent of VP2."
  (make-instance 'cl-toposort:directed-graph
                 :vertices vpatch-list
                 :edges
                 (loop for vp1 in vpatch-list
                    append (loop for vp2 in vpatch-list
                              if (parentp vp1 vp2)
                              collect (make-instance 'cl-toposort:directed-edge
                                                     :head vp1
                                                     :tail vp2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using keccak to validate the output of a press

(defun check-hashes (vpatch output-dir)
  (labels ((subpatch-path-to-actual (subpatch)
             (merge-pathnames (subseq (path subpatch)
                                      (1+ (position #\/ (path subpatch))))
                              (make-pathname
                               :directory `(:relative ,output-dir))))
           (compare-hashes (subpatch hash-accessor)
             (if (string= (funcall hash-accessor subpatch)
                          "false")
                 (not (probe-file (subpatch-path-to-actual subpatch)))
                 (string= (funcall hash-accessor subpatch)
                          (string-downcase
                           (cl-keccak::keccak-hash-file
                            (subpatch-path-to-actual subpatch)
                            cl-keccak::+bitrate+
                            cl-keccak::+output-bits+))))))
    (dolist (sp (subpatches vpatch))
      (format t "    Checking hash on ~A~%"
              (namestring (subpatch-path-to-actual sp)))
      (if (not (compare-hashes sp #'post-hash))
          (error 'hash-failure :text (namestring (subpatch-path-to-actual sp)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; procedures to assist main operations

(defun ancestors (vpatch
                  vpatch-list
                  ancestor-function)
  "Finds all vpatches in VPATCH-LIST which can result from repeated
application of ANCESTOR-FUNCTION, starting at VPATCH.

Returns the transitive closure of the binary relation
ANCESTOR-FUNCTION in the list VPATCH-LIST, rooted at VPATCH."
  (labels ((anc-rec (vp1)
             (let ((ancestors
                    (remove-if-not #'(lambda (vp2)
                                       (funcall ancestor-function
                                                vp1
                                                vp2))
                                   vpatch-list)))
               ;; below sexp technically not needed, increases efficiency
               (setq vpatch-list
                     (remove-if #'(lambda (v)
                                    (member v ancestors))
                                vpatch-list))
               (if (null ancestors)
                   nil
                   (append ancestors
                           (remove-duplicates
                            (apply #'append
                                   (mapcar #'anc-rec
                                           ancestors))))))))
    (anc-rec vpatch)))

(defun lookup (subseq vpatch-list &key error-on-fail)
  (let ((result (find-if #'(lambda (vp)
                             (search subseq
                                     (file-namestring
                                      (namestring
                                       (path vp)))))
                         vpatch-list)))
    (if (and (null result) error-on-fail)
        (error 'vpatch-lookup :text subseq)
        result)))

(defmacro interpret-and-verify (items &rest body)
  "This macro makes it cleaner for various operations to take in
either strings identifying vpatches, or vpatch objects. Allowing
strings makes operations simpler at the REPL, and allows for
POSIX terminal interaction.

If VPATCH is a string and is not a substring of the name of
some vpatch, raises a VPATCH-LOOKUP error."
  `(let* ,(append
           '((created-wot nil))
           '((wot (if (null wot)
                      (progn (setq created-wot t)
                             (make-wot))
                      wot)))
           (if (member 'vpatch-list items)
               '((vpatch-list (cl-gpg:check-trust
                               (if (null vpatch-list)
                                   (mapcar #'path
                                           (load-vpatches *default-vpatch-dir*))
                                   vpatch-list)
                               *default-seal-dir*
                               *default-wot-dir*)))
               '())
           (if (member 'vpatch items)
               '((vpatch (check-trust
                          (if (stringp vpatch)
                              (list (lookup vpatch
                                            vpatch-list
                                            :error-on-fail t))
                              vpatch)
                          wot)))
               '())
           `((result (progn ,@body))))
     (if created-wot (remove-wot wot))
     result))

(defmacro after-finding-vpatches-if-needed (&rest body)
  `(progn (if (null vpatch-list)
              (setq vpatch-list (load-vpatches *default-vpatch-dir*)))
          ,@body))

(defmacro after-verifying-patches (&rest body)
  `(progn (cl-gpg:check-trust (mapcar #'path vpatch-list)
                              *default-seal-dir*
                              *default-wot-dir*)
          ,@body))

(defmacro after-looking-up-vpatch-if-needed (&rest body)
  `(progn (if (stringp vpatch)
              (setq vpatch (lookup vpatch vpatch-list :error-on-fail t))
              (cl-gpg:check-trust (list (path vpatch))
                                  *default-seal-dir*
                                  *default-wot-dir*))
          ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main operations

(defun flow (&optional vpatch-list)
  (after-finding-vpatches-if-needed
   (after-verifying-patches
    (cl-toposort:toposort
     (generate-depgraph
      vpatch-list)))))

(defun roots (&optional vpatch-list)
  (after-finding-vpatches-if-needed
   (after-verifying-patches
    (cl-toposort:toposort
     (generate-depgraph
      (remove-if-not #'(lambda (vp)
                         (cl-toposort::rootp vp
                                             (cl-toposort::edges
                                              (generate-depgraph vpatch-list))))
                     vpatch-list))))))

(defun leaves (&optional vpatch-list)
  (after-finding-vpatches-if-needed
   (after-verifying-patches
    (cl-toposort:toposort
     (generate-depgraph
      (remove-if-not #'(lambda (vp)
                         (cl-toposort::leafp vp
                                             (cl-toposort::edges
                                              (generate-depgraph vpatch-list))))
                     vpatch-list))))))

(defun antecedents (vpatch &optional vpatch-list)
  (after-finding-vpatches-if-needed
   (after-looking-up-vpatch-if-needed
    (after-verifying-patches
     (cl-toposort:toposort
      (generate-depgraph
       (ancestors vpatch vpatch-list #'childp)))))))

(defun descendants (vpatch &optional vpatch-list)
  (after-finding-vpatches-if-needed
   (after-looking-up-vpatch-if-needed
    (after-verifying-patches
     (cl-toposort:toposort
      (generate-depgraph
       (ancestors vpatch vpatch-list #'parentp)))))))

(defun press-path (vpatch &optional vpatch-list)
  (after-finding-vpatches-if-needed
   (after-looking-up-vpatch-if-needed
    (append (antecedents vpatch vpatch-list)
            (list vpatch)))))

(defun press (vpatch output-dir &optional vpatch-list)
  (after-finding-vpatches-if-needed
   (after-looking-up-vpatch-if-needed
    (notany #'(lambda (vp)
                (format t "Pressing ~A~%" (name vp))
                (cl-patch:patch (path vp)
                                (make-pathname
                                 :directory `(:relative
                                              ,output-dir)))
                (format t "  Checking hashes in ~A~%" (name vp))
                (check-hashes vp output-dir))
            (cl-toposort:toposort
             (generate-depgraph
              (press-path vpatch vpatch-list)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for use as a binturd

(defun print-usage ()
  (format t "Usage:
<command> ::= v <nullary> | <unary> | <binary>
<nullary> ::= flow | roots | leaves
  <unary> ::= ante | desc | path <vpatch-designator>
 <binary> ::= press <vpatch-designator> <press-directory>

<vpatch-designator> is a substring of the basename of some vpatch.
"))

(defun print-all (obj)
  (labels ((print-all-recur (r)
             (cond
               ((null r) nil)
               ((listp r)
                (mapcar #'print-all-recur r))
               ((typep r 'vpatch) (format t "~a~%" (name r)))
               ((eq r t) (format t "Success~%"))
               (t (format t "~a~%" r)))))
    (print-all-recur obj)))

(defun main ()
  (handler-case
      (let ((args #+sbcl (cdr sb-ext:*posix-argv*)
                  #+ccl (cdr ccl:*command-line-argument-list*)))
        (labels ((call (n procedure)
                   (if (not (= (length (cdr args)) n))
                       (print-usage)
                       (print-all (apply procedure (cdr args))))))
          (let ((cmd (car args)))
            (cond
              ((string= cmd "flow")   (call 0 #'flow))
              ((string= cmd "roots")  (call 0 #'roots))
              ((string= cmd "leaves") (call 0 #'leaves))
              ((string= cmd "ante")   (call 1 #'antecedents))
              ((string= cmd "desc")   (call 1 #'descendants))
              ((string= cmd "path")   (call 1 #'press-path))
              ((string= cmd "press")  (call 2 #'press))
              (t (print-usage))))))
    (cl-gpg::bad-public-key (c)
      (format t "GnuPG failed to import key ~S.~%" (text c)))
    (cl-gpg::bad-seal (c)
      (format t "GnuPG failed to verify seal ~S.~%" (text c)))
    (cyclic ()
      (format t "Cycle encountered during topological sort.~%"))
    (cl-gpg::no-seal (c)
      (format t "Failed to find a seal for vpatch ~S.~%" (text c)))
    (cl-patch::output-dir-dne (c)
      (format t "Output directory not found at location ~S." (text c)))
    (cl-patch::patch-failure (c)
      (format t "Failed to apply vpatch ~S.~%" (text c)))
    (unsupported-cl ()
      (format t "Unsupported Common Lisp implementation detected.~%"))
    (vpatch-lookup (c)
      (format t "Failed to find vpatch matching ~S.~%" (text c)))
    (cl-gpg::wot-dir-creation (c)
      (format t "Failed to make temporary WoT directory: ~S.~%" (text c)))
    (cl-gpg::wot-dir-dne (c)
      (format t "WoT directory not found at location ~S.~%" (text c)))
    (hash-failure (c)
      (format t "Hash of file does not match expected: ~S.~%" (text c)))))
