(ql:quickload :alexandria)
(ql:quickload :cl-ppcre) 

(defparameter *filename* "input.txt")
(defparameter *max* 80)

(defvar *school*
  (mapcar #'parse-integer
          (ppcre:split "\\,"
                       (alexandria:read-file-into-string *filename*))))

(defun simulate (school)
  (mapcar (lambda (x)
            (cond
              ((equal x 0)
               (list 6 8))
              (t (- x 1))))
          school))

(format t "Simulating!~%")
(loop for step from 1 to *max*
      with input = *school*
      do (format t "Step ~a~%" step)
          (setf input (alexandria:flatten (simulate input)))
         finally (format t "There are ~a fish" (length input)))



;; Part two asks for a *max* of 256, in order to do that need to switch to tracking the number of fish per bucket instead of a list.
(defparameter *school-array* (make-array 9))

(loop for item in *school*
      do (incf (aref *school-array* item)))

(loop for step from 1 to 256
      with temp = 0
      do (format t "Part 2: Step ~a~%" step)
         (setf temp (aref *school-array* 0))
         (loop for i from 0 to 7
               do (setf (aref *school-array* i) (aref *school-array* (+ i 1))))
         (setf (aref *school-array* 8) temp)
         (incf (aref *school-array* 6) temp)
         finally (format t "There are ~a fish" (reduce #'+ *school-array*)))
