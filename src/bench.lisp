(in-package :trivial-csv)

(defun random-char (string)
  (aref string (random (length string))))

(defun random-field ()
  (with-output-to-string (s)
    (dotimes (i (random 100))
      (write-char (random-char (concatenate 'string (string #\newline)
                                            " abcdefghijklmnop,\""))
                  s))))

(defun random-row ()
  (loop :repeat (1+ (random 20)) :collect (random-field)))

(defun random-data (rows)
  (loop :repeat rows :collect (random-row)))

(defun bench-this (data)
  (let* ((str (make-string-input-stream
                (with-output-to-string (s)
                  (time (write-rows data s)))))
         (result (time (read-rows str))))
    (equal data result)))

(defun bench-other (data)
  (let* ((str (make-string-input-stream
                (with-output-to-string (s)
                  (time (cl-csv:write-csv data :stream s)))))
         (result (time (cl-csv:read-csv str))))
    (equal data result)))

(defun bench ()
  (let ((data (random-data 5000)))
    (write-line "MINE")
    (bench-this data)
    (write-line "OTHER")
    (bench-other data))
  )


(defparameter *data* nil)

(defun generate-large-data ()
  (setf *data* (random-data 5000)))

(defun write-file-this ()
  (with-open-file (s "test/large-this.csv"
                     :direction :output
                     :if-exists :supersede)
    (time
      (loop :repeat 10 :do (write-rows *data* s)))))

(defun write-file-other ()
  (with-open-file (s "test/large-other.csv"
                     :direction :output
                     :if-exists :supersede)
    (time (loop :repeat 10 :do (cl-csv:write-csv *data*
                                                 :stream s
                                                 :newline (string #\newline))))))

(defun bench-write-file ()
  (write-line "Generating data.")
  (time (generate-large-data))
  (write-line "Benchmarking this (writing).")
  (write-file-this)
  (write-line "Benchmarking other (writing).")
  (write-file-other))


(defun read-file-this ()
  (with-open-file (s "test/large-this.csv")
    (time (loop
            :with data = *data*
            :for row = (read-row s nil :eof)
            :for expected-row = (progn (when (null data)
                                         (setf data *data*))
                                       (pop data))
            :until (eql :eof row)
            :do (assert (equal expected-row row))))))

(defun read-file-other ()
  (with-open-file (s "test/large-other.csv")
    (time (handler-case
              (loop
                :with data = *data*
                :for row = (cl-csv:read-csv-row s
                                                :newline (string #\newline)
                                                :trim-outer-whitespace nil)
                :for expected-row = (progn (when (null data)
                                             (setf data *data*))
                                           (pop data))
                :do (assert (equal expected-row row)))
            (end-of-file () nil)))))

(defun bench-read-file ()
  (write-line "Benchmarking this (reading).")
  (read-file-this)
  (write-line "Benchmarking other (reading).")
  (read-file-other))

(defun bench-file ()
  (bench-write-file)
  (bench-read-file))
