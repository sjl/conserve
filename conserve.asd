(asdf:defsystem :conserve
  :description "Yet Another CSV Library for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://sjl.bitbucket.io/conserve/"
  :license "MIT/X11"
  :version "0.0.1"

  :depends-on ()

  :in-order-to ((asdf:test-op (asdf:test-op :conserve/test)))

  :serial t
  :components ((:file "package")
               (:module "src" :serial t :components
                ((:file "main")))))

(asdf:defsystem :conserve/test
  :description
  "Test suite for conserve."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"

  :depends-on (:conserve :1am :cl-csv :fare-csv)

  :serial t
  :components ((:file "package.test")
               (:module "test" :serial t :components
                ((:file "tests"))))
  :perform (asdf:test-op (op system)
             (funcall (read-from-string "conserve/test:run-tests"))))

