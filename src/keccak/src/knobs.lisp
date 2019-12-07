(in-package "CL-KECCAK")

;; the number of bits in a byte, arch-specific
(defconstant +bits-in-byte+ 8)

;; the keccak L parameter, takes a value in {1,2,3,4,5,6}
(defconstant +keccak_L+ 6)

;; the number of bits absorbed into the sponge on each pass
(defconstant +bitrate+ 1344)

;; the desired number of output bits
(defconstant +output-bits+ 512)
