(asdf:defsystem :trivial-csv
  :description "Yet Another CSV Library for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://sjl.bitbucket.io/trivial-csv/"
  :license "MIT/X11"
  :version "0.0.1"

  :depends-on ()

  :in-order-to ((asdf:test-op (asdf:test-op :trivial-csv/test)))

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "main")))))

(asdf:defsystem :trivial-csv/test
  :description
  "Test suite for trivial-csv."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"

  :depends-on (:trivial-csv :1am)

  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (op system)
             (funcall (read-from-string "trivial-csv/test:run-tests"))))

