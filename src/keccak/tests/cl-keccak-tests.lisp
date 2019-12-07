(in-package "CL-KECCAK")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions for moving between integers and lanes
;; these are not used in cl-keccak proper. rather, they are
;; for testing and generating the round constants

(defun bit-truncate-right (bv n)
  (subseq bv 0 n))

(defun integer-to-lane (n)
  (labels ((bit-array-iter (n array)
             (if (zerop n)
                 array
                 (multiple-value-bind (q r)
                     (floor n 2)
                   (bit-array-iter q
                                   (append array (list r)))))))
    (bit-truncate-right (bit-pad-right (bit-array-iter n '())
                                       +lane-size+)
                        +lane-size+)))

(defun lane-to-integer (bv)
  (reduce #'(lambda (a b) (+ a (* 2 b)))
          bv
          :from-end t))

(defun lane-to-string (lane &optional (raw t))
  (if raw
      (format nil "~a" lane)
      (let ((fmt-str (format nil
                             "~~~d,'0X"
                             (max 0 (/ +lane-size+ 4)))))
        (format nil fmt-str (lane-to-integer lane)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test for magic constants.
;; numbers used to generate the lanes can be found here:
;; https://keccak.team/keccak_specs_summary.html#roundConstants

(defun test-magic-constants (&optional (stream t))
  (let ((errors nil)
        (calculated-constants
         (make-array '(24)
                     :element-type 'lane
                     :initial-contents
                     (mapcar #'integer-to-lane
                             '(#x0000000000000001
                               #x0000000000008082
                               #x800000000000808a
                               #x8000000080008000
                               #x000000000000808b
                               #x0000000080000001
                               #x8000000080008081
                               #x8000000000008009
                               #x000000000000008a
                               #x0000000000000088
                               #x0000000080008009
                               #x000000008000000a
                               #x000000008000808b
                               #x800000000000008b
                               #x8000000000008089
                               #x8000000000008003
                               #x8000000000008002
                               #x8000000000000080
                               #x000000000000800a
                               #x800000008000000a
                               #x8000000080008081
                               #x8000000000008080
                               #x0000000080000001
                               #x8000000080008008)))))
    (dotimes (x 24)
      (let ((a (aref calculated-constants x))
            (b (aref +round-constants+ x)))
        (format stream "Constant number ~d~%" x)
        (format stream "Actual:   ~a~%" a)
        (format stream "Expected: ~a~%" b)
        (format stream "Status: ~a~%"
                (if (equal a b) "OK" (progn (push x errors)
                                            "ERROR")))))
    (if errors
        (format t "ERRORS DETECTED! SEE CONSTANTS: ~a" errors)
        (format t "Test passed with no errors."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various procedures to test states

(defun print-state (state &optional (raw t) (stream t))
  (while-traversing-state (x y)
    (format stream
            (if (or raw (= x (1- +row-size+)))
                "~a~%"
                "~a ")
            (lane-to-string (lane state x y) raw))))

(defun read-state (stream)
  (with-return-state (b)
    (dotimes (y +column-size+)
      (dotimes (x +row-size+)
        (setf (lane b x y) (read stream))))))

(defun diff-states (state1 state2)
  (let ((diff '()))
    (dotimes (x +row-size+)
      (dotimes (y +column-size+)
        (if (not (equal (lane state1 x y)
                        (lane state2 x y)))
            (setq diff (append diff (list (cons x y)))))))
    diff))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests of the permutation functions

(defun print-keccak-permute (state &optional (stream t) (raw t))
  (format stream ";; Initial state:~%")
  (print-state state raw stream)
  (dotimes (r +round-quantity+)
    (let ((maps (list (cons "theta" #'theta)
                      (cons "rho" #'rho)
                      (cons "pi" #'k-pi)
                      (cons "chi" #'chi)
                      (cons "iota" #'(lambda (a) (iota r a))))))
      (format stream "~%;; Round ~d~%~%" r)
      (dolist (m maps)
        (format stream ";; After ~a:~%" (car m))
        (print-state (setq state
                           (funcall (cdr m) state))
                     raw
                     stream))))
  (format stream "~%;; Final state:~%")
  (state-linearize state)
  (print-state state raw stream))

(defun test-keccak-permute (test-file &optional (stream t))
  (with-open-file (f test-file :direction :input)
    (handler-case
        (let* ((input-state (read-state f))
               (calculated-state input-state))
          (dotimes (r +round-quantity+)
            (let ((maps `(("theta" ,#'theta)
                          ("rho" ,#'rho)
                          ("pi" ,#'k-pi)
                          ("chi" ,#'chi)
                          ("iota" ,#'(lambda (a) (iota r a))))))
              (dolist (m maps)
                (format stream "Testing: (~2,'0d, ~a)~%" r (first m))
                (psetq input-state (read-state f)
                       calculated-state (funcall (second m)
                                                 calculated-state))
                (format stream "Expected:~%")
                (print-state input-state nil stream)
                (format stream "Calculated:~%")
                (print-state calculated-state nil stream)
                (if (null (diff-states input-state calculated-state))
                    (format stream "Passed: (~2,'0d, ~a)~%" r (first m))
                    (progn
                      (format stream "~%FAILED on permutation ~a, round ~d~%"
                              (first m) r)
                      (format stream "Input state:~%")
                      (print-state input-state nil stream)
                      (format stream "Calculated state:~%")
                      (print-state calculated-state nil stream)
                      (error "State mismatch")))))))
      (error (c) t)))
  (format stream "All permutation function tests passed.~%"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; procedure for testing the sponge

(defun test-keccak-sponge (input-bit-vector
                           expected-output-bit-vector-file
                           bitrate
                           output-bits
                           &optional (stream t) (output nil))
  (let ((expected-output-bit-vector
         (with-open-file (f expected-output-bit-vector-file :direction :input)
           (read f))))
    (format stream
            "Testing sponge with input ~A, bitrate ~d, and output bit quantity ~d.~%"
            input-bit-vector
            bitrate
            output-bits)
    (setq output (keccak-sponge input-bit-vector bitrate output-bits))
    (format stream "Output:~%~a~%" (bit-vector-to-hex output))
    (format stream
            (if (equal expected-output-bit-vector output)
                "Output matches expected value. Test passed.~%"
                "TEST FAILED! Output does NOT match expected value.~%"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; running the tests

(defun run-tests (&optional (out t))
  (let ((tests
         `(("Testing row constant generation~%"
            ,(lambda () (test-magic-constants
                         out)))
           ("Testing permutations from zero state~%"
            ,(lambda () (test-keccak-permute "testzerostate.txt"
                                             out)))
           ("Testing permutations from nonzero state~%"
            ,(lambda () (test-keccak-permute "testnonzerostate.txt"
                                             out)))
           ("First sponge test~%"
            ,(lambda () (test-keccak-sponge #*11001
                                            "testspongeoutput1.txt"
                                            1344
                                            4096
                                            out)))
           ("Second sponge test~%"
            ,(lambda () (test-keccak-sponge #*110010100001101011011110100110
                                            "testspongeoutput2.txt"
                                            1344
                                            1344
                                            out))))))
    (do ((error nil)
         (n 0 (incf n)))
        ((or error
             (= n (length tests))))
      (format out (first (nth n tests)))
      (funcall (second (nth n tests)))
      (format out "~%~%"))))
