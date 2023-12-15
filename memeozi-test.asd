(asdf:defsystem "memeozi-test"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria :fiveam :memeozi :trivia)
  :components ((:module "t"
                :components ((:file "memeozi-test")
                             (:file "strategy-test")))))
