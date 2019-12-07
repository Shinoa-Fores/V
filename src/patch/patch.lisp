(in-package "CL-PATCH")

(define-condition output-dir-dne (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if the output dir does not exist.")
  (:report (lambda (condition stream)
             (format stream
                     "Output directory not found at location ~S."
                     (text condition)))))

(define-condition patch-failure (error)
  ((text :initarg :text :reader text))
  (:documentation "Raised if a patching operation fails.")
  (:report (lambda (condition stream)
             (format stream
                     "Failed to apply vpatch ~S."
                     (text condition)))))

(defun run-subprocess (program args)
  #+sbcl (sb-ext:process-exit-code (sb-ext:run-program program args))
  #+ccl (nth-value 1 (ccl:external-process-status
                      (ccl:run-program program args)))
  #-(or :sbcl :ccl) (error 'unsupported-cl))

(defun patch (patch output-dir)
  (ensure-directories-exist output-dir)
  (if (not (probe-file output-dir))
      (error 'output-dir-dne)
      (if (eq 0
              (run-subprocess *patch-location*
                              (list "--dir" (namestring output-dir)
                                    "-F" "0" "-E" "-p1" "-i"
                                    (namestring (probe-file patch)))))
          nil
          (error 'patch-failure :text (namestring patch)))))
