;;; Day 5
(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :uiop)


(defparameter *max-x* 1000)
(defparameter *max-y* 1000)
(defparameter *grid* (make-array '(1000 1000)))
(defparameter *score* 0)
(defparameter *limit* 2)
(defparameter +pattern+ "(\\d+),(\\d+) -> (\\d+),(\\d+)") 

(defparameter *lines*
;;Parse line with register group binds to end up with points of form
;; ((x1,y1) (x2,y2))
  (mapcar (lambda (line)
            (ppcre:register-groups-bind
                ((#'parse-integer x1 y1 x2 y2))
                (+pattern+ line :sharedp t)
              (list (list x1 y1) (list x2 y2))))
          (uiop:read-file-lines "input.txt")))

(loop for i from 0 below *max-x*
      do (loop for j from 0 below *max-y*
               do (loop for line in *lines*
                        do (if (diag-point-intersect-p (list i j) line)
                               (incf (aref *grid* j i))))))

(loop for i from 0 below *max-x* do
  (loop for j from 0 below *max-y*
        count (>= (aref *grid* i j) *limit*) into score
        finally (incf *score* score))
      finally (print *score*))

;; Point Line Intersect Formula
;; point (x y)
;; line ((x1 y1) (x2 y2))
;; assume only horizontal or vertical lines
(defun point-intersect-p (point line)
  (let ((x (first point))
        (y (second point))
        (x1 (first (first line)))
        (y1 (second (first line)))
        (x2 (first (second line)))
        (y2 (second (second line))))
    (cond
      ((and (eql x1 x2)
            (eql x x1)
            (>= y (min y1 y2))
            (<= y (max y1 y2)))
       t)
      ((and (eql y1 y2)
            (eql y y1)
            (>= x (min x1 x2))
            (<= x (max x1 x2)))
       t)
      (t nil))))

;; Point Line Intersect Formula
;; point (x y)
;; line ((x1 y1) (x2 y2))
;; assume only horizontal or vertical lines and 45 degree 
(defun diag-point-intersect-p (point line)
  (let ((x (first point))
        (y (second point))
        (x1 (first (first line)))
        (y1 (second (first line)))
        (x2 (first (second line)))
        (y2 (second (second line))))
    (cond
      ((and (eql x1 x2)
            (eql x x1)
            (>= y (min y1 y2))
            (<= y (max y1 y2)))
       t)
      ((and (eql y1 y2)
            (eql y y1)
            (>= x (min x1 x2))
            (<= x (max x1 x2)))
       t)
      ;; slope of  1
      ((and (eql 1 (point-slope x y x1 y1))
            (>= x (min x1 x2))
            (<= x (max x1 x2))
            (>= y (min y1 y2))
            (<= y (max y1 y2)))
       t)
      ;; point is an endpoint
      ((or (and (eql x x1) (eql y y1))
           (and (eql x x2) (eql y y2)))
       t)
      (t nil))))

(defun point-slope (x1 y1 x2 y2)
  (let ((num (- y2 y1))
        (denom (- x2 x1)))
    (if (eql 0 denom)
        nil
        (abs (/ num denom)))))
