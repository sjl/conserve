#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :trivial-csv :silent t)
(time (asdf:test-system :trivial-csv))
(quit)
