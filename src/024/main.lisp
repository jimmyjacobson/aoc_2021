;;; Day 24
(ql:quickload :uiop)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defvar x 0)
(defvar y 0)
(defvar z 0)
(defvar w 0)
(defvar *input* nil)

(defun read-program (filename)
  (uiop:read-file-forms filename))

(defun execute-program (program input)
  (setf x 0 y 0 z 0 w 0 *input* input)
  (let ((lisp-program (append `(progn) (mapcar #'eval program))))
    (eval lisp-program)
    (format t "~%RESULTS~%x:~a~%y:~a~%z:~a~%w:~a~%" x y z w)))


;;; ALU Operations - translate ALU operations into lisp code that can be run
(defun inp (a)
  ;; (inp 'w 5) returns (setf 'w 5)
  `(setf ,a (pop *input*)))

(defun add (a b)
  `(setf ,a (+ ,a ,b)))

(defun mul (a b)
  `(setf ,a (* ,a ,b)))

(defun div (a b)
  `(setf ,a (floor (/ ,a ,b))))

(defun modulo (a b)
  `(setf ,a (mod ,a ,b)))

(defun equal-aoc (a b)
  `(if (eql ,a ,b)
       (setf ,a 1)
       (setf ,a 0)))

;;; Programs

(defvar *negative*  (read-program "programs/negative.txt"))
(defvar *binary* (read-program "programs/binary.txt"))
(defparameter *>-than-3x* (read-program "programs/greater-than-3x.txt"))
(defparameter *monad* (read-program "programs/monad.txt"))

;;; scratch

(loop for i from 9 downto 1
      do (let ((x (format nil "~a9999999999999" i)))
           (print x)
           (if (not (position #\0 x))
               (block program
                 (execute-program *monad* (mapcar #'parse-integer (ppcre:split "" x)))
                 (if (eq z 0)
                     (return))))))

(let ((prefix nil)
      (suffix nil))
  (loop for i from 13 downto 0
        do (format t "~{~a~}~%" (alexandria:iota i :start 9 :step 0)))



