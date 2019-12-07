(in-package "CL-KECCAK")

(defun bit-chunk (bit-vector chunk-size)
  (assert (= 0 (mod (length bit-vector) chunk-size)))
  (let ((chunks '()))
    (dotimes (c (/ (length bit-vector) chunk-size))
      (push (subseq bit-vector
                    (* c chunk-size)
                    (* (1+ c) chunk-size))
            chunks))
    (nreverse chunks)))

(defun bit-pad-right (bv n)
  (do ((x (coerce bv 'list) (append x '(0))))
      ((>= (length x) n)
       (coerce x 'simple-bit-vector))))

(defun bit-vector-concatenate (bit-vector-sequence)
  (apply #'concatenate 'simple-bit-vector bit-vector-sequence))

(defun bit-vector-concatenate-uniform-vector (bit-vector-vector member-size)
  (let ((rtn (make-sequence 'simple-bit-vector
                            (* member-size (length bit-vector-vector)))))
    (dotimes (i (length bit-vector-vector))
      (replace rtn
               (aref bit-vector-vector i)
               :start1 (* i member-size)
               :end1 (* (1+ i) member-size)))
    rtn))

(defun bit-vector-to-integer (bv)
  (reduce #'(lambda (a b) (+ a (* 2 b)))
          bv
          :from-end t))

(defun bit-vector-to-hex (bv)
  (apply #'concatenate
         'string
         (mapcar (lambda (n)
                   (let ((s (write-to-string n :base 16)))
                     (if (= (length s) 2)
                         s
                         (concatenate 'string "0" s))))
                 (mapcar #'bit-vector-to-integer
                         (bit-chunk bv 8)))))

(defun integer-to-bit-vector (n)
  (labels ((bit-array-iter (n array)
             (if (zerop n)
                 array
                 (multiple-value-bind (q r)
                     (floor n 2)
                   (bit-array-iter q
                                   (append array (list r)))))))
    (bit-pad-right (bit-array-iter n '()) 8)))

(defun file-to-bit-vector (filepath)
  (with-open-file (f filepath :direction :input :element-type 'bit)
    (bit-vector-concatenate-uniform-vector
     (map 'vector
          #'integer-to-bit-vector
          (let ((s (make-sequence 'list (file-length f))))
            (read-sequence s f)
            s))
     +bits-in-byte+)))
