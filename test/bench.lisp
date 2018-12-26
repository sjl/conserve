(in-package :conserve)

(defparameter *field-length* 50)
(defparameter *row-length* 50)
(defparameter *data-rows* 1000)
(defparameter *data-repetitions* 10)

(defun random-char (string)
  (aref string (random (length string))))

(defun random-field ()
  (with-output-to-string (s)
    (dotimes (i (random *field-length*))
      (write-char (random-char (concatenate 'string (string #\newline)
                                            " abcdefghijklmnop,\""))
                  s))))

(defun random-row ()
  (loop :repeat (1+ (random *row-length*)) :collect (random-field)))

(defun random-data ()
  (loop :repeat *data-rows* :collect (random-row)))

(defun bench-this (data)
  (let* ((str (make-string-input-stream
                (with-output-to-string (s)
                  (time (write-rows data s)))))
         (result (time (read-rows str))))
    (equal data result)))

(defun bench-cl-csv (data)
  (let* ((str (make-string-input-stream
                (with-output-to-string (s)
                  (time (cl-csv:write-csv data :stream s)))))
         (result (time (cl-csv:read-csv str))))
    (equal data result)))

(defun bench ()
  (let ((data (random-data)))
    (write-line "MINE")
    (bench-this data)
    (write-line "cl-csv")
    (bench-cl-csv data))
  )


(defvar *data* nil)

(defun generate-large-data ()
  (setf *data* (random-data)))

(defun write-file-this ()
  (with-open-file (s "test/data/large-this.csv"
                     :direction :output
                     :if-exists :supersede)
    (time
      (loop :repeat *data-repetitions* :do
            (write-rows *data* s)))))

(defun write-file-cl-csv ()
  (with-open-file (s "test/data/large-cl-csv.csv"
                     :direction :output
                     :if-exists :supersede)
    (time (loop :repeat *data-repetitions* :do
                (cl-csv:write-csv *data*
                                  :stream s
                                  :newline (string #\newline))))))
(defun write-file-fare ()
  (with-open-file (s "test/data/large-fare.csv"
                     :direction :output
                     :if-exists :supersede)
    (fare-csv:with-rfc4180-csv-syntax ()
      (time (loop :repeat *data-repetitions* :do
                  (fare-csv:write-csv-lines *data* s))))))

(defun bench-write-file ()
  (write-line "Generating data.")
  (time (generate-large-data))

  (write-line "Benchmarking this (writing).")
  #+sbcl (sb-ext:gc :full t)
  (write-file-this)

  ;; (write-line "Benchmarking cl-csv (writing).")
  ;; #+sbcl (sb-ext:gc :full t)
  ;; (write-file-cl-csv)

  (write-line "Benchmarking fare-csv (writing).")
  #+sbcl (sb-ext:gc :full t)
  (write-file-fare))


(defun read-file-this ()
  (with-open-file (s "test/data/large-this.csv")
    (time (loop
            :with data = *data*
            :for row = (read-row s nil :eof)
            :for expected-row = (progn (when (null data)
                                         (setf data *data*))
                                       (pop data))
            :until (eql :eof row)
            :summing 1
            :do (assert (equal expected-row row))))))

(defun read-file-cl-csv ()
  (with-open-file (s "test/data/large-cl-csv.csv")
    (let ((result 0))
      (time (handler-case
                (loop
                  :with data = *data*
                  :for row = (cl-csv:read-csv-row s
                                                  :newline (string #\newline)
                                                  :trim-outer-whitespace nil)
                  :for expected-row = (progn (when (null data)
                                               (setf data *data*))
                                             (pop data))
                  :do (incf result) (assert (equal expected-row row)))
              (end-of-file () result))))))

;; There are a couple of problems with fare-csv that make it annoying to work
;; with:
;;
;; fare-csv:read-csv-line can't tell you the difference between '("") and eof so
;; we have to check for it manually to know when we're done.  This is only
;; a problem when we're reading line by line.
;;
;; fare-csv also can't roundtrip '("").  CSV itself is incapable of
;; differentiating '() and '("") unless you force quoting, but fare-csv chooses
;; '() over '("").  Why would you sell out the marginally-useful case (a 1-col
;; CSV) in favor of the utterly useless case (a 0-col CSV)?
(defun read-file-fare ()
  (with-open-file (s "test/data/large-fare.csv")
    (fare-csv:with-rfc4180-csv-syntax ()
      (time (loop
              :with data = *data*
              :for eof = (peek-char nil s nil :eof)
              :for row = (fare-csv:read-csv-line s)
              :for expected-row = (progn (when (null data)
                                           (setf data *data*))
                                         (pop data))
              :until (eql :eof eof)
              :summing 1
              :do (if (equal expected-row '(""))
                    (assert (equal nil row))
                    (assert (equal expected-row row))))))))

(defun bench-read-file ()
  (write-line "Benchmarking this (reading).")
  #+sbcl (sb-ext:gc :full t)
  (format t "Read ~D rows.~2%" (read-file-this))

  ;; (write-line "Benchmarking cl-csv (reading).")
  ;; #+sbcl (sb-ext:gc :full t)
  ;; (format t "Read ~D rows.~2%" (read-file-cl-csv))

  (write-line "Benchmarking fare-csv (reading).")
  #+sbcl (sb-ext:gc :full t)
  (format t "Read ~D rows.~2%" (read-file-fare))

  (values))

(defun bench-file ()
  (bench-write-file)
  (bench-read-file))

