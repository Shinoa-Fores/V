(in-package "CL-KECCAK")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magic numbers

(defconstant +row-size+ 5)
(defconstant +column-size+ 5)
(defconstant +lane-size+ (expt 2 +keccak_L+))
(defconstant +keccak-width+ (* +row-size+ +column-size+ +lane-size+))
(defconstant +round-quantity+ (+ 12 (* 2 +keccak_L+)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lanes

;; definition of a lane
(deftype lane () `(simple-bit-vector ,+lane-size+))

;; instantiation of lanes
(defun make-lane ()
  (make-sequence 'lane +lane-size+ :initial-element 0))

;; copy a lane into a new memory location
(defun copy-lane (lane)
  (make-array `(,+lane-size+) :element-type 'bit
              :initial-contents lane))

;; basic operations on lanes
(defun lane-and (a b)
  (declare (type lane a b))
  (bit-and a b))

(defun lane-xor (a b)
  (declare (type lane a b))
  (bit-xor a b))

(defun lane-not (a)
  (declare (type lane a))
  (bit-not a))

(defun lane-rot (a n)
  (let* ((rtn (make-lane)))
    (dotimes (z +lane-size+)
      (setf (aref rtn (mod (+ z n) +lane-size+))
            (aref a z)))
    rtn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magic lanes

(defparameter *round-constants*
  #(#*1000000000000000000000000000000000000000000000000000000000000000
    #*0100000100000001000000000000000000000000000000000000000000000000
    #*0101000100000001000000000000000000000000000000000000000000000001
    #*0000000000000001000000000000000100000000000000000000000000000001
    #*1101000100000001000000000000000000000000000000000000000000000000
    #*1000000000000000000000000000000100000000000000000000000000000000
    #*1000000100000001000000000000000100000000000000000000000000000001
    #*1001000000000001000000000000000000000000000000000000000000000001
    #*0101000100000000000000000000000000000000000000000000000000000000
    #*0001000100000000000000000000000000000000000000000000000000000000
    #*1001000000000001000000000000000100000000000000000000000000000000
    #*0101000000000000000000000000000100000000000000000000000000000000
    #*1101000100000001000000000000000100000000000000000000000000000000
    #*1101000100000000000000000000000000000000000000000000000000000001
    #*1001000100000001000000000000000000000000000000000000000000000001
    #*1100000000000001000000000000000000000000000000000000000000000001
    #*0100000000000001000000000000000000000000000000000000000000000001
    #*0000000100000000000000000000000000000000000000000000000000000001
    #*0101000000000001000000000000000000000000000000000000000000000000
    #*0101000000000000000000000000000100000000000000000000000000000001
    #*1000000100000001000000000000000100000000000000000000000000000001
    #*0000000100000001000000000000000000000000000000000000000000000001
    #*1000000000000000000000000000000100000000000000000000000000000000
    #*0001000000000001000000000000000100000000000000000000000000000001))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; states

;; definition of a keccak state
(deftype state () `(array lane (,+row-size+ ,+column-size+)))

;; instantiation of states
(defun make-state ()
  (make-array `(,+row-size+ ,+column-size+)
              :element-type 'lane
              :initial-element (make-lane)))

;; accessing the lanes of a state
(defun lane (a x y)
  (declare (type state a)
           (type fixnum x y))
  (aref a (mod x +row-size+) (mod y +column-size+)))

;; mutating the lanes of a state
(defmethod set-lane (a x y L)
  (setf (aref a (mod x +row-size+) (mod y +column-size+))
        L))

(defsetf lane set-lane)

;; a macro for modifying and returning a state
(defmacro with-return-state (s &rest body)
  `(let ((,(first s) (if (= ,(length s) 2)
                         (copy-state ,(second s))
                         (make-state))))
     (progn ,@body)
     ,(first s)))

;; a macro for traversing the lanes of a state in the
;; standard order (x,y) = (0,0), (1,0), (2,0), ...
(defmacro while-traversing-state (vars &body body)
  `(dotimes (,(second vars) +column-size+)
     (dotimes (,(first vars) +row-size+)
       ,@body)))

;; copy a state into a new memory location
(defun copy-state (state)
  (let ((s (make-array `(,+row-size+ ,+column-size+)
                       :initial-element (make-lane)
                       :element-type 'lane)))
    (dotimes (x +row-size+)
      (dotimes (y +column-size+)
        (setf (lane s x y)
              (copy-lane (lane state x y)))))
    s))

;; transform a state into a single bit vector, concatenating
;; the lanes in the standard order
(defun state-linearize (state)
  (let ((r '()))
    (with-return-state (s state)
      (while-traversing-state (x y)
        (push (lane s x y) r)))
    (bit-vector-concatenate (nreverse r))))

;; transform a bit vector into a state, filling the
;; lanes in the standard order
(defun state-unlinearize (linearized-state)
  (let ((chunked-state (bit-chunk linearized-state
                                  +lane-size+)))
    (with-return-state (s)
      (while-traversing-state (x y)
        (setf (lane s x y) (pop chunked-state))))))

(defun state-xor (state bit-vector)
  (state-unlinearize (bit-xor (state-linearize state)
                              (bit-pad-right bit-vector
                                             +keccak-width+))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keccak round operations

(defun theta (a)
  (with-return-state (b)
    (let* ((c (make-sequence '(vector lane) +row-size+
                             :initial-element (make-lane)))
           (d (make-sequence '(vector lane) +row-size+
                             :initial-element (make-lane))))
      (dotimes (x +row-size+)
        (setf (aref c x)
              (lane a x 0))
        (loop for y from 1 below +column-size+
           do (setf (aref c x)
                    (lane-xor (aref c x)
                              (lane a x y)))))
      (dotimes (x +row-size+)
        (setf (aref d x)
              (lane-xor (aref c (mod (- x 1) +row-size+))
                        (lane-rot (aref c (mod (+ x 1) +row-size+))
                                  1)))
        (dotimes (y +column-size+)
          (setf (lane b x y)
                (lane-xor (lane a x y)
                          (aref d x))))))))

(defun rho (a)
  (with-return-state (b)
    (setf (lane b 0 0) (lane a 0 0))
    (let ((x 1) (y 0))
      (dotimes (q 24)
        (setf (lane b x y)
              (lane-rot (lane a x y)
                        (/ (* (+ q 1)
                              (+ q 2))
                           2)))
        (psetq x y
               y (+ (* 2 x)
                    (* 3 y)))))))

(defun k-pi (a)
  (with-return-state (b)
    (dotimes (x +row-size+)
      (dotimes (y +column-size+)
        (setf (lane b y (+ (* 2 x)
                           (* 3 y)))
              (lane a x y))))))

(defun chi (a)
  (with-return-state (b)
    (dotimes (x +row-size+)
      (dotimes (y +column-size+)
        (setf (lane b x y)
              (lane-xor (lane a x y)
                        (lane-and (lane-not (lane a (+ x 1) y))
                                  (lane a (+ x 2) y))))))))

(defun iota (r a)
  (with-return-state (b a)
    (setf (lane b 0 0)
          (lane-xor (lane b 0 0)
                    (aref *round-constants* r)))))

(defun keccak-permute (a)
  (with-return-state (b a)
    (dotimes (r +round-quantity+)
      (setq b (iota r (chi (k-pi (rho (theta b)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sponge

(defun keccak-pad (bit-vector bitrate)
  (labels ((remaining-space (bit-vector bitrate)
             (abs (nth-value 1 (ceiling (+ 2 (length bit-vector))
                                        bitrate)))))
    (bit-vector-concatenate (list
                             bit-vector
                             #*1
                             (make-sequence 'simple-bit-vector
                                            (remaining-space bit-vector
                                                             bitrate))
                             #*1))))

(defun keccak-absorb (bit-vector bitrate)
  (assert (< bitrate +keccak-width+))
  (with-return-state (s)
    (dolist (c (bit-chunk (keccak-pad bit-vector bitrate) bitrate))
      (setq s (state-xor s c))
      (setq s (keccak-permute s)))))

(defun keccak-squeeze (state  bitrate output-bits)
  (assert (< bitrate +keccak-width+))
  (let ((rtn '()))
    (do ((remaining-bits output-bits (- remaining-bits
                                        bitrate)))
        ((> bitrate remaining-bits)
         (push (subseq (state-linearize state) 0 remaining-bits)
               rtn))
      (push (subseq (state-linearize state) 0 bitrate)
            rtn)
      (setq state (keccak-permute state)))
    (bit-vector-concatenate (nreverse rtn))))

(defun keccak-sponge (input-bit-vector bitrate output-bits)
  (keccak-squeeze (keccak-absorb input-bit-vector
                                 bitrate)
                  bitrate
                  output-bits))

(defun keccak-hash-file (filepath bitrate output-bits)
  (bit-vector-to-hex (keccak-sponge (file-to-bit-vector filepath)
                                    bitrate
                                    output-bits)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for use as an executable

(defun main ()
  (let ((args #+sbcl (cdr sb-ext:*posix-argv*)
              #+ccl (cdr ccl:*command-line-argument-list*)))
    (princ (string-downcase (keccak-hash-file (first args)
                                              +bitrate+
                                              +output-bits+)))))
