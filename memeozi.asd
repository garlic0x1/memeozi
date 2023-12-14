(asdf:defsystem "memeozi"
  :description "A memoization library for Common Lisp"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria :cl-annot :bordeaux-threads)
  :components ((:module "src"
                :components ((:file "core")))))
