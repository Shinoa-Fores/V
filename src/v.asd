(defpackage v-asd
  (:use common-lisp asdf))

(in-package v-asd)

;; sbcl 1.4.4 barfs on making an executable without this
#+sbcl (require :sb-posix)

(defsystem v
  :components ((:file "package")
               (:file "v"
                      :depends-on ("package"
                                   gpg
                                   keccak
                                   patch
                                   toposort))
               (:module gpg
                        :serial t
                        :components ((:file "package")
                                     (:file "knobs")
                                     (:file "cl-gpg")))
               (:module keccak
                        :serial t
                        :components ((:file "src/package")
                                     (:file "src/knobs")
                                     (:file "src/bits")
                                     (:file "src/cl-keccak")))
               (:module patch
                        :serial t
                        :components ((:file "package")
                                     (:file "knobs")
                                     (:file "patch")))
               (:module toposort
                        :serial t
                        :components ((:file "package")
                                     (:file "toposort")))))
