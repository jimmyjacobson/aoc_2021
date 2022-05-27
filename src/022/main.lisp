;;; Day 22
;;; https://adventofcode.com/2021/day/22
;; Consider treating the cuboid as a set of 3d points and the on/off operation
;; removes or adds points to the set.  The set is too big to treat as an array

(ql:quickload :cl-ppcre)
(ql:quickload :uiop)
(ql:quickload :alexandria)
;;; Consider using iota, map-product,

(defparameter +pattern+ "^(on|off) x=(-?\\d+)\.\.(-?\\d+),y=(-?\\d+)\.\.(-?\\d+),z=(-?\\d+)\.\.(-?\\d+).*$")
(defparameter *min* -50)
(defparameter *max* 50)
(defparameter *steps* '(("on" (10 10 10) 3)
                        ("on" (11 11 11) 3)
                        ("off" (9 9 9) 3)
                        ("on" (10 10 10) 1)))
(defparameter *cuboid* nil)
(setf *cuboid* nil)

(setf *steps*
      (mapcar (lambda (line)
                (ppcre:register-groups-bind
                    (mode (#'parse-integer x1 x2 y1 y2 z1 z2))
                    (+pattern+ line :sharedp t)
                  (if (check-bounds x1 x2 y1 y2 z1 z2)
                      (list mode (list x1 y1 z1)
                            (list (scratch-math x2 x1)
                                  (scratch-math y2 y1)
                                  (scratch-math z2 z1))))))
              (uiop:read-file-lines "test2.txt")))

(dolist (step *steps*)
  (print step)
  (cond
    ((string= "on" (first step))
     (alexandria:unionf *cuboid*
                        (generate-cuboid (second step) (third step))
                        :test #'equalp))
    ((string= "off" (first step))
     (setf *cuboid* (set-difference
                     *cuboid*
                     (generate-cuboid (second step) (third step))
                     :test #'equalp)))
    (t nil))
  (format t "~%~a cubes are on~%" (length *cuboid*)))

(defun check-bounds (x1 x2 y1 y2 z1 z2)
  (or t
      (and (>= x1 *min*)
           (>= y1 *min*)
           (>= z1 *min*)
           (<= x2 *max*)
           (<= y2 *max*)
           (<= z2 *max*))))

(defun scratch-math (a b)
;; return a - b + 1 with checks for bounds
  (+ 1 (- a b)))


;;Fixme - I made a mistake,they aren't squares but possible rectangular cuboids
(defun generate-cuboid (origin len)
;;; generate a list of 3d points between 0,0,0 and x,y,z
  (alexandria:map-product
   'list
    (alexandria:iota (first len) :start (first origin))
    (alexandria:iota (second len) :start (second origin))
    (alexandria:iota (third len) :start (third origin))))

  
