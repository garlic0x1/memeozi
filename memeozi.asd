(asdf:defsystem "memeozi"
  :description "A collection of memoization macros"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria :cl-annot :bordeaux-threads)
  :components ((:module "src"
                :components (;; (:file "types")
                             (:file "core")))))
