#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :conserve :silent t)
(time (asdf:test-system :conserve))
(quit)
