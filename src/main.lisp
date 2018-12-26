(in-package :trivial-csv)

;;;; Configuration ------------------------------------------------------------
(defparameter *delimiter* #\,)

(defun check-delimiter ()
  (check-type *delimiter* (and character (not (member #\newline #\")))))


;;;; Reading ------------------------------------------------------------------
;;; We reuse a single string output stream for all the fields in a row, rather
;;; than having a fresh one per field.  It's a little more tedious but it's
;;; *significantly* less consing (e.g. 500mb versus 1.5gb).

(defun read-char-if (char stream &optional (eof-error-p t) eof-value)
  "If the next character in `stream` is `char`, read and return it.  Otherwise, return `nil`."
  (cond
    ((eql char (peek-char nil stream eof-error-p nil)) (read-char stream))
    ((eql char eof-value) char)
    (t nil)))

(defun read-unquoted-field (stream delimiter result)
  "Read an unquoted field (but not the ending field delimiter) from `stream` into `result`."
  (loop :for next = (peek-char nil stream nil delimiter)
        :until (cond
                 ((char= next delimiter) t)
                 ((char= next #\newline) t)
                 ((char= next #\") (error "Unquoted field contains quote."))
                 (t nil))
        :do (write-char (read-char stream) result)))

(defun read-quoted-field (stream delimiter result)
  "Read a quoted field (but not the ending field delimiter) from `stream` into `result`."
  (declare (ignore delimiter))
  (read-char stream) ; chomp initial quote
  (loop :for char = (read-char stream)
        :until (and (char= #\" char)
                    (not (read-char-if #\" stream nil)))
        :do (write-char char result)))

(defun read-field (stream delimiter result)
  "Read and return a single field from `stream` into `result`."
  (let* ((field (case (peek-char nil stream nil :eof)
                  (#\" (read-quoted-field stream delimiter result) t)
                  (#\newline (read-char stream) nil)
                  (:eof nil)
                  (t (read-unquoted-field stream delimiter result) t)))
         (done (cond
                 ((null field) t) ; empty field at the end
                 ((read-char-if delimiter stream nil) nil) ; normal field
                 ((read-char-if #\newline stream nil #\newline) t) ; last field
                 (t (error "Bad data after field ~S: ~S"
                           field (peek-char nil stream))))))
    (values (if field
              (get-output-stream-string result)
              "")
            done)))

(defun read-row% (stream delimiter eof-error-p eof-value)
  (if (eql :eof (peek-char nil stream eof-error-p :eof))
    eof-value
    (loop :with field
          :with done
          :with result = (make-string-output-stream)
          :do (setf (values field done) (read-field stream delimiter result))
          :collect field
          :until done)))


;;;; Writing ------------------------------------------------------------------
(defun field-needs-quoting-p (field delimiter)
  (some (lambda (char)
          (or (char= char #\newline)
              (char= char #\")
              (char= char delimiter)))
        field))

(defun write-quoted-field (field stream)
  (write-char #\" stream)
  (loop :for char :across field :do
        (write-char char stream)
        (when (char= #\" char)
          (write-char char stream)))
  (write-char #\" stream))

(defun write-unquoted-field (field stream)
  (write-string field stream))

(defun write-field (field stream delimiter)
  (if (field-needs-quoting-p field delimiter)
    (write-quoted-field field stream)
    (write-unquoted-field field stream)))

(defun write-row% (row stream delimiter)
  (check-type row (not null))
  (loop :for (field . more) :on row :do
        (write-field field stream delimiter)
        (when more
          (write-char delimiter stream)))
  (write-char #\newline stream))


;;;; API ----------------------------------------------------------------------
(defun read-row (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  "Read and return a row of fields from the CSV data in `stream`.

  The result will be completely fresh.

  If the end of file for the stream is encountered immediately, an error is
  signaled unless `eof-error-p` is false, in which case `eof-value` is returned.

  "
  (check-type stream stream)
  (assert (input-stream-p stream) (stream)
          "Stream ~S is not an input stream." stream)
  (check-delimiter)
  (read-row% stream *delimiter* eof-error-p eof-value))

(defun read-rows (&optional (stream *standard-input*))
  "Read and return all CSV rows from the CSV data in `stream`.

  The result will be completely fresh.

  "
  (check-type stream stream)
  (assert (input-stream-p stream) (stream)
          "Stream ~S is not an input stream." stream)
  (check-delimiter)
  (loop :with delimiter = *delimiter*
        :for row = (read-row% stream delimiter nil :eof)
        :until (eql row :eof)
        :collect row))

(defun write-row (row &optional (stream *standard-output*))
  "Write `row` to `stream` as CSV data."
  (check-delimiter)
  (write-row% row stream *delimiter*)
  (values))

(defun write-rows (rows &optional (stream *standard-output*))
  "Write `rows` to `stream` as CSV data."
  (check-delimiter)
  (loop :with delimiter = *delimiter*
        :for row :in rows
        :do (write-row% row stream delimiter)))

