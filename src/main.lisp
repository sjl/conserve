(in-package :conserve)

;;;; Configuration ------------------------------------------------------------
(defparameter *delimiter* #\,)

(defun check-delimiter ()
  (check-type *delimiter* (and character (not (member #\newline #\")))))


;;;; Reading ------------------------------------------------------------------
;;; We reuse a single string output stream for all the fields in a row, rather
;;; than having a fresh one per field.  It's a little more tedious but it's
;;; *significantly* less consing (e.g. 500mb versus 1.5gb).

(defun read-char-if (char stream)
  "If the next character in `stream` is `char`, read and return it.  Otherwise return `nil`."
  (if (eql char (peek-char nil stream nil nil))
    (read-char stream)
    nil))

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
                    (not (read-char-if #\" stream)))
        :do (write-char char result)))

(defun read-field (stream delimiter result)
  "Read and return a single field from `stream` using `result`.

  Returns two values: a string of the field and a boolean denoting whether
  we're done reading this row.

  "
  (case (peek-char nil stream nil :eof)
    (#\" (read-quoted-field stream delimiter result))
    ((#\newline :eof) nil)
    (t (read-unquoted-field stream delimiter result)))
  (let ((next (peek-char nil stream nil :eof)))
    (values (get-output-stream-string result)
            (cond ((eql next :eof) t)
                  ((eql next #\newline) (read-char stream) t)
                  ((eql next delimiter) (read-char stream) nil)
                  (t (error "Bad data after field: ~S" next))))))

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
(defun ensure-input-stream (stream-or-string)
  (etypecase stream-or-string
    (stream stream-or-string)
    (string (make-string-input-stream stream-or-string))))

(defun read-row
    (&optional (stream-or-string *standard-input*) (eof-error-p t) eof-value)
  "Read and return a row of fields from the CSV data in `stream-or-string`.

  The result will be completely fresh.

  If the end of file for the stream is encountered immediately, an error is
  signaled unless `eof-error-p` is false, in which case `eof-value` is returned.

  "
  (check-delimiter)
  (let ((stream (ensure-input-stream stream-or-string)))
    (assert (input-stream-p stream) (stream)
      "Stream ~S is not an input stream." stream)
    (read-row% stream *delimiter* eof-error-p eof-value)))

(defun read-rows (&optional (stream-or-string *standard-input*))
  "Read and return all CSV rows from the CSV data in `stream-or-string`.

  The result will be completely fresh.

  "
  (check-delimiter)
  (let ((stream (ensure-input-stream stream-or-string)))
    (assert (input-stream-p stream) ()
      "Stream ~S is not an input stream." stream)
    (loop :with delimiter = *delimiter*
          :for row = (read-row% stream delimiter nil :eof)
          :until (eql row :eof)
          :collect row)))


(defmacro with-output-to-stream-or-string
    ((symbol &optional (stream-or-null symbol)) &body body)
  "Bind `symbol` to an output stream, run `body`, and return appropriately.

  If `stream-or-null` is a stream, `symbol` will be bound to it and nothing will
  be returned.

  If `stream-or-null` is `nil`, `symbol` will be bound to a string output stream
  and the resulting string will be returned.

  "
  (let ((want-string (gensym "WANT-STRING")))
    `(let* ((,symbol ,stream-or-null)
            (,want-string (null ,symbol))
            (,symbol (or ,symbol (make-string-output-stream))))
       ,@body
       (if ,want-string
         (get-output-stream-string ,symbol)
         (values)))))

(defun write-row (row &optional (stream *standard-output*))
  "Write `row` to `stream` as CSV data.

  If `stream` is `nil`, the data will be returned as a fresh string instead.

  "
  (check-delimiter)
  (check-type stream (or null stream))
  (with-output-to-stream-or-string (stream)
    (assert (output-stream-p stream) (stream)
      "Stream ~S is not an output stream." stream)
    (write-row% row stream *delimiter*)))

(defun write-rows (rows &optional (stream *standard-output*))
  "Write `rows` to `stream` as CSV data.

  If `stream` is `nil`, the data will be returned as a fresh string instead.

  "
  (check-delimiter)
  (check-type stream (or null stream))
  (with-output-to-stream-or-string (stream)
    (assert (output-stream-p stream) (stream)
      "Stream ~S is not an output stream." stream)
    (loop :with delimiter = *delimiter*
          :for row :in rows
          :do (write-row% row stream delimiter))))
