Usage
=====

Conserve is a small Common Lisp library for reading and writing [RFC
4180](https://tools.ietf.org/html/rfc4180) CSV data.

It was made because I was tired of dealing with complicated libraries with many
dependencies for parsing simple CSV data files.

[TOC]

## Example Data

In the following documentation we'll read from the following `example.csv`:

    id,name,score
    1,foo,88.8
    2,bar,100
    3,baz,77

## Reading

CSV files can be read row-by-row or all at once, from a stream or a string.  If
you want to read from a file it's up to you to open the file (with the
appropriate external format) yourself.

To read all rows from a stream, use `conserve:read-rows`:

    (with-open-file (f "example.csv" :direction :input)
      (conserve:read-rows f))
    ; =>
    ; (("id" "name" "score")
    ;  ("1"  "foo"  "88.8")
    ;  ("2"  "bar"  "100")
    ;  ("3"  "baz"  "77"))

Conserve does not process headers in any special way, nor does it parse values
at all.  Rows are returned as lists of strings — what you do with that is up to
you.

    (defun parse-row (row)
      (destructuring-bind (id name score) row
        (list (parse-integer id)
              name
              (parse-float:parse-float score))))

    (with-open-file (f "example.csv" :direction :input)
      (destructuring-bind (header . rows)
          (conserve:read-rows f)
      (values header (map-into rows #'parse-row rows))))

    ; =>
    ; ("id" "name" "score")
    ; ((1   "foo"  88.8)
    ;  (2   "bar"  100.0)
    ;  (3   "baz"  77.0))

Use `conserve:read-row` to read a single row at a time:

    (with-open-file (f "example.csv" :direction :input)
      (conserve:read-row f))
    ; =>
    ; ("id" "name" "score")

Note that `conserve:read-row` has the same interface as most Common Lisp
`read-*` style functions, so it can often be used in places that expect that
interface:

    (iterate
      (for (id name nil) :in-file "example.csv" :using #'conserve:read-row)
      (finding (parse-integer id) :such-that (string= name "bar")))
    ; => 2

Both reading functions support reading from a string instead of a stream:

    (conserve:read-row "foo,\"a,b,c\",bar")
    ; => ("foo" "a,b,c" "bar")

## Writing

Much like reading, Conserve supports writing one or many rows at a time.

Use `conserve:write-rows` to write a list of rows:

    (with-open-file (f "out1.csv" :direction :output)
      (conserve:write-rows '(("id" "name" "score")
                             ("1" "foo" "88.8")
                             ("2" "bar" "100.0")
                             ("3" "baz" "77.0"))
                           f))

Use `conserve:write-row` to write a single row at a time:

    (with-open-file (f "out2.csv" :direction :output)
      (conserve:write-row '("id" "name" "score") f)
      (conserve:write-row '("1" "foo" "88.8") f)
      (conserve:write-row '("2" "bar" "100.0") f)
      (conserve:write-row '("3" "baz" "77.0") f))

Rows must be a list of *strings* — Conserve does not attempt to guess how you
would like to serialize other objects to strings.

If `nil` is passed as a stream, Conserve will return the resulting CSV as
a string:

    (conserve:write-row '("foo"
                          "some \"quoted\" field"
                          "comma,field")
                        nil)
    ; =>
    "foo,\"some \"\"quoted\"\" field\",\"comma,field\"
    "

## Delimiter

Conserve allows one single piece of customization: the choice of delimiter to
use.  You can change the delimiter by binding `conserve:*delimiter*`:

    (let ((conserve:*delimiter* #\,))
      (conserve:write-row '("a" "b") nil))
    ; =>
    "a,b
    "

    (let ((conserve:*delimiter* #\|))
      (conserve:write-row '("a" "b") nil))
    ; =>
    "a|b
    "

    (let ((conserve:*delimiter* #\tab))
      (conserve:write-row '("foo,bar" "foo|bar") nil))
    ; =>
    "foo,bar        foo|bar
    "

## Test Suite

The test suite include both hardcoded tests against particular edge cases, as
well as round-trip fuzz testing against `cl-csv` and `fare-csv` to make sure it
produces similar results.  You will need to Quickload those other CSV parsers to
run the test suite (but not to use Conserve itself, of course).
