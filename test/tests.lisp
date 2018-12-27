(in-package :conserve/test)


;;;; Utils --------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern (apply #'concatenate 'string (mapcar #'princ-to-string args)))))

(defmacro define-test (name &body body)
  `(test ,(symb 'test- name)
    (let ((*package* ,*package*))
      ,@body)))

(defmacro define-csv-test (name csv data &optional bindings)
  `(define-test ,name
     (let ,bindings
       (let ((data ,data)
             (csv (format nil ,csv)))
         (is (equalp data (conserve:read-rows csv)))
         (is (equalp csv (conserve:write-rows data nil)))))))

(defun run-tests ()
  (1am:run))


;;;; Compatibility Layer ------------------------------------------------------
(defun cl-csv-write (data stream)
  (cl-csv:write-csv data :stream stream :newline (string #\newline)))

(defun cl-csv-write-string (data)
  (cl-csv:write-csv data :stream nil :newline (string #\newline)))

(defun cl-csv-read (stream)
  (cl-csv:read-csv stream
                   :newline (string #\newline)
                   :trim-outer-whitespace nil))

(defun cl-csv-read-string (string)
  (cl-csv:read-csv string
                   :newline #\newline
                   :trim-outer-whitespace nil))


(defun fare-csv-write (data stream)
  (fare-csv:with-rfc4180-csv-syntax ()
    (fare-csv:write-csv-lines data stream)))

(defun fare-csv-write-string (data)
  (with-output-to-string (s)
    (fare-csv-write data s)))

(defun fix-fare-csv-empty-lines (rows)
  (mapcar (lambda (row)
            (if (null row)
              (list "")
              row))
          rows))

(defun fare-csv-read (stream)
  (fare-csv:with-rfc4180-csv-syntax ()
    (fare-csv:read-csv-stream stream)))

(defun fare-csv-read-string (string)
  (fare-csv-read (make-string-input-stream string)))


;;;; Basic Tests --------------------------------------------------------------
(define-csv-test simple-csv
  "a,b,c~@
   d,e,f~%"
  '(("a" "b" "c")
    ("d" "e" "f")))

(define-csv-test empty
  ""
  '())

(define-csv-test blank-lines
  "a~%~%c~%"
  '(("a")
    ("")
    ("c")))

(define-csv-test empty-fields
  "a,,b~@
   ,a,b~@
   a,b,~@
   ,,a,,~%"
   '(("a" "" "b")
     ("" "a" "b")
     ("a" "b" "")
     ("" "" "a" "" "")))

(define-csv-test spaces
  "a  b,   c,d   ~%"
  '(("a  b"
     "   c"
     "d   ")))

(define-csv-test basic-quoting
  "a,b,c~@
   \"a,b\",c~%"
  '(("a" "b" "c")
    ("a,b" "c")))

(define-csv-test quote-escaping
  "foo,\"x\"\"y\",baz~@
   \"\"\"start\",\"end\"\"\"~%"
  '(("foo" "x\"y" "baz")
    ("\"start" "end\"")))

(define-csv-test quoted-newlines
  "a,\"foo~%bar\",b~%"
  `(("a" ,(format nil "foo~%bar") "b")))

(define-csv-test other-delimiter
  "a_b,c_\"foo_bar\"~%"
  '(("a"
     "b,c"
     "foo_bar"))
  ((conserve:*delimiter* #\_)))

(define-test no-trailing-newline
  (is (equal '(("a" "b")) (conserve:read-rows "a,b"))))

(define-test read-single-row
  (is (equal '("a" "b")
              (conserve:read-row (format nil "a,b~%c,d") nil nil))))

(define-test read-row-eof-value
  (is (equal :eof (conserve:read-row "" nil :eof))))

(define-test read-row-eof-error
  (signals end-of-file (conserve:read-row "" t)))


;;;; Fuzzing ------------------------------------------------------------------
(defparameter *string-characters*
  (format nil "abc\", ~%"))

(defun random-char (&optional (string *string-characters*))
  (aref string (random (length string))))

(defun random-field (characters)
  (with-output-to-string (s)
    (dotimes (i characters)
      (write-char (random-char) s))))

(defun random-row (fields characters)
  (loop :repeat fields :collect (random-field (random characters))))

(defun random-data (rows fields characters)
  (loop :repeat rows :collect (random-row (1+ (random fields))
                                          characters)))


(define-test fuzz-round-trip
  (dotimes (i 500)
    (let* ((data (random-data 100 10 15))
           (output (conserve:write-rows data nil))
           (round-tripped (conserve:read-rows output)))
      (is (equal data round-tripped)))))

(define-test fuzz-against-cl-csv
  (dotimes (i 100)
    (let* ((data (random-data 10 10 10))
           (conserve-out (conserve:write-rows data nil))
           (cl-csv-out (cl-csv-write-string data))
           (conserve->cl-csv (cl-csv-read-string conserve-out))
           (cl-csv->conserve (conserve:read-rows cl-csv-out)))
      (is (= (length data) (length conserve->cl-csv)))
      (is (= (length data) (length cl-csv->conserve)))
      (is (equal data conserve->cl-csv))
      (is (equal data cl-csv->conserve)))))

(define-test fuzz-against-fare-csv
  (dotimes (i 100)
    (let* ((data (random-data 10 10 10))
           (conserve-out (conserve:write-rows data nil))
           (fare-csv-out (fare-csv-write-string data))
           (conserve->fare-csv (fix-fare-csv-empty-lines (fare-csv-read-string conserve-out)))
           (fare-csv->conserve (conserve:read-rows fare-csv-out)))
      (is (= (length data) (length conserve->fare-csv)))
      (is (= (length data) (length fare-csv->conserve)))
      (is (equal data conserve->fare-csv))
      (is (equal data fare-csv->conserve)))))


;;;; Benchmarking -------------------------------------------------------------
(defun round-trip/conserve (data)
  (conserve:read-rows (conserve:write-rows data nil)))

(defun round-trip/cl-csv (data)
  (cl-csv-read-string (cl-csv-write-string data)))

(defun round-trip/fare-csv (data)
  (fare-csv-read-string (fare-csv-write-string data)))


(defun benchmark-round-trip/conserve (data)
  (format t "~%Timing in-memory round trip for Conserve:~%")
  (let ((result (time (round-trip/conserve data))))
    (assert (equal data result))))

(defun benchmark-round-trip/fare-csv (data)
  (format t "~%Timing in-memory round trip for fare-csv:~%")
  (let ((result (time (round-trip/fare-csv data))))
    (assert (equal data (fix-fare-csv-empty-lines result)))))

(defun benchmark-round-trip/cl-csv (data)
  (format t "~%Timing in-memory round trip for cl-csv:~%")
  (let ((result (time (round-trip/cl-csv data))))
    (assert (equal data result))))


(defun benchmark-round-trip ()
  (let ((data (random-data 1000 100 500)))
    (benchmark-round-trip/conserve data)
    (benchmark-round-trip/fare-csv data)
    (benchmark-round-trip/cl-csv data)))


(defun write-file/conserve (data repetitions)
  (with-open-file (s "test/data/large-conserve.csv"
                     :direction :output
                     :if-exists :supersede)
    (loop :repeat repetitions :do (conserve:write-rows data s))))

(defun write-file/cl-csv (data repetitions)
  (with-open-file (s "test/data/large-cl-csv.csv"
                     :direction :output
                     :if-exists :supersede)
    (loop :repeat repetitions :do
          (cl-csv:write-csv data
                            :stream s
                            :newline (string #\newline)))))

(defun write-file/fare-csv (data repetitions)
  (with-open-file (s "test/data/large-fare-csv.csv"
                     :direction :output
                     :if-exists :supersede)
    (fare-csv:with-rfc4180-csv-syntax ()
      (loop :repeat repetitions :do (fare-csv:write-csv-lines data s)))))

(defvar *data* nil)
(defparameter *verify-large-file-reads* t)
(defparameter *repetitions* 30)
(defparameter *rows* 1000)
(defparameter *fields* 200)
(defparameter *characters* 80)
(defparameter *size* (* *repetitions* *rows* *fields* *characters* 3))

(defun benchmark-large-file/write ()
  (format t "~%Generating data~%")
  (let ((data (time (random-data *rows* *fields* *characters*))))
    (setf *data* data)

    (format t "~%Timing large file write for Conserve:~%")
    #+sbcl (sb-ext:gc :full t)
    (time (write-file/conserve data *repetitions*))

    (format t "~%Timing large file write for fare-csv:~%")
    #+sbcl (sb-ext:gc :full t)
    (time (write-file/fare-csv data *repetitions*))

    (format t "~%Timing large file write for cl-csv:~%")
    #+sbcl (sb-ext:gc :full t)
    (time (write-file/cl-csv data *repetitions*))

    (values)))


(defun read-file/conserve ()
  (with-open-file (s "test/data/large-conserve.csv")
    (if *verify-large-file-reads*
      (loop
        :for original :in *data*
        :for row = (conserve:read-row s nil :eof)
        :until (eql :eof row)
        :do (assert (equal original row)))
      (loop
        :for row = (conserve:read-row s nil :eof)
        :until (eql :eof row)))))

(defun read-file/fare-csv ()
  (with-open-file (s "test/data/large-fare-csv.csv")
    (fare-csv:with-rfc4180-csv-syntax ()
      (if *verify-large-file-reads*
        (loop
          :for original :in *data*
          :until (eql :eof (peek-char nil s nil :eof))
          :do (assert (equal original (or (fare-csv:read-csv-line s) '("")))))
        (loop
          :until (eql :eof (peek-char nil s nil :eof))
          :do (fare-csv:read-csv-line s))))))

(defun read-file/cl-csv ()
  (with-open-file (s "test/data/large-cl-csv.csv")
    (handler-case
        (if *verify-large-file-reads*
          (loop
            :for original :in *data*
            :for row = (cl-csv:read-csv-row s
                                            :newline (string #\newline)
                                            :trim-outer-whitespace nil)
            :do (assert (equal original row)))
          (loop
            (cl-csv:read-csv-row s
                                 :newline (string #\newline)
                                 :trim-outer-whitespace nil)))
      (end-of-file () nil))))

(defun benchmark-large-file/read ()
  ;; circular list to make iterating easier in the readers
  (setf (cdr (last *data*)) *data*)

  (format t "~%Timing large file read for Conserve:~%")
  #+sbcl (sb-ext:gc :full t)
  (time (read-file/conserve))

  (format t "~%Timing large file read for fare-csv:~%")
  #+sbcl (sb-ext:gc :full t)
  (time (read-file/fare-csv))

  (format t "~%Timing large file read for cl-csv:~%")
  #+sbcl (sb-ext:gc :full t)
  (time (read-file/cl-csv))

  (values))

(defun benchmark-large-file ()
  (unless (y-or-n-p
            "This benchmark could require over ~:Dmb of hard disk space.~@
             Do you want to proceed?"
            (truncate *size* (* 1024 1024)))
    (return-from benchmark-large-file))
  (benchmark-large-file/write)
  (benchmark-large-file/read))

