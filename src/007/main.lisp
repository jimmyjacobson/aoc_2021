
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :uiop)

(defparameter *filename* "input.txt")

(defparameter *data*
  (mapcar #'parse-integer
           (ppcre:split "\\," (uiop:read-file-line *filename*))))
