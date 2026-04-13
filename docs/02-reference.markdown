# API Reference

The following is a list of all user-facing parts of Conserve.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `CONSERVE`

### `*DELIMITER*` (variable)

### `READ-ROW` (function)

    (READ-ROW &OPTIONAL (STREAM-OR-STRING *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE)

Read and return a row of fields from the CSV data in `stream-or-string`.

  The result will be a fresh list.

  If the end of file for the stream is encountered immediately, an error is
  signaled unless `eof-error-p` is false, in which case `eof-value` is returned.

  

### `READ-ROWS` (function)

    (READ-ROWS &OPTIONAL (STREAM-OR-STRING *STANDARD-INPUT*))

Read and return all CSV rows from the CSV data in `stream-or-string`.

  The result will be a completely fresh list of lists.

  

### `WRITE-ROW` (function)

    (WRITE-ROW ROW &OPTIONAL (STREAM *STANDARD-OUTPUT*))

Write `row` to `stream` as CSV data.

  `row` must be a list of strings.

  If `stream` is `nil`, the data will be returned as a fresh string instead.

  

### `WRITE-ROWS` (function)

    (WRITE-ROWS ROWS &OPTIONAL (STREAM *STANDARD-OUTPUT*))

Write `rows` to `stream` as CSV data.

  `rows` must be a list of lists of strings.  The consequences are undefined if
  all the rows do not have the same number of fields.

  If `stream` is `nil`, the data will be returned as a fresh string instead.

  

