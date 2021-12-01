;; (in-package :com.jimmyjacobson.aoc2021)

(defparameter *filename* "./src/001/input.txt")

(defun read-file-to-list (filename)
  (let ((numbers nil))
    (with-open-file (in filename)
      (loop for line = (read in nil)
	          while line do (push line numbers)))
    (reverse numbers)))

(defun main (numbers)
  (let ((count 0))
    (dotimes (i (- (length numbers) 1))
      (if (< (elt numbers i) (elt numbers (+ i 1)))
          (incf count)))
    count))

(defun windows (numbers)
  (let ((values nil))
    (dotimes (i (- (length numbers) 2))
      (push (+ (elt numbers i)
               (elt numbers (+ i 1))
               (elt numbers (+ i 2)))
            values))
    (reverse values)))

;; Answer 1
(main (read-file-to-list *filename*))

;; Answer 2
(main (windows (read-file-to-list *filename*)))
