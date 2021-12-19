;;; Day 4 bingo
;;; Represent bingo boards as array multi dimensional array or list of lists?
;;; Array is much easier to traverse, but lists can do set check
;;; ignore diagnoals

(ql:quickload "cl-ppcre")

(defun main (filename)
  ;;Simulates a bingo game by iterating over a list of moves
  ;;checking each board against the current list until a winnder is
  ;;found
  (multiple-value-bind (moves boards) (read-input filename)
    (simulate-bingo boards moves)))

(defun simulate-bingo (boards moves)
  ;;simulate turns by iterating over moves list
  (loop for i from 5 to (length moves)
        with turn
        with score
        do (setf turn (subseq moves 0 i))
           ;;(setf score (find-winner boards turn))
           (setf score (find-last-winner boards turn))
       ;; until (not (null score))
        finally (return score)))


(defun find-winner (boards moves)
  (dolist (board boards)
    (if (eql t (check-board board moves))
        (return (calculate-board-score board moves)))))

(defun find-last-winner (boards moves)
  (let ((winner nil))
    (print moves)
    (dolist (board boards)
      (if (eql t (check-board board moves))
          (setf winner (calculate-board-score board moves))))
    winner))


(defun check-board (board moves)
  ;;check a board for win by checking rows, columns
  ;;return true if wins
  (let ((rowMatch) (colMatch))
    (loop for i from 0 to 4
          do (setf rowMatch t)
             (setf colMatch t)
             (loop for j from 0 to 4
                   do
                      ;;check row
                      (if (not (find (aref board i j) moves))
                          (setf rowMatch nil))
                      ;;check column
                      (if (not (find (aref board j i) moves))
                          (setf colMatch nil)))
             (if (or (eql t rowMatch) (eql t colMatch))
                 (return)))
    (or rowMatch colMatch))) ;; return true if row or column match

(defun calculate-board-score (board moves)
  ;;The sum of all board members not in moves
  ;;Multiplied by the last element of moves
  (print board)
  (let ((total 0))
    (loop for i from 0 to 4
          do (loop for j from 0 to 4
                   when (not (find (aref board i j) moves))
                     do (setf total (+ total (aref board i j)))))
    (* total (elt moves (- (length moves) 1)))))

(defun read-input (filename)
  (let ((moves nil)
        (boards '()))
    (with-open-file (in filename)
      (setf moves (ppcre:split "," (read-line in))) ;; read moves from first line
      (setf moves (mapcar #'parse-integer moves)) ;; convert moves to integers
      (read-line in) ;;read blank line and ignore
      (loop for line = (read-line in nil)
            with row
            with (board '())
            while line
            do (if (eql 0 (length line))
                   (progn (push (make-array '(5 5)
                                            :initial-contents
                                            (reverse board))
                                boards)
                           (setf board '()))
                   (push (mapcar #'parse-integer
                                 (util-strip-empty-entries
                                  (ppcre:split "\\s" line))) board))
            finally (push (make-array '(5 5)
                                     :initial-contents
                                     (reverse board))
                          boards))
      (values moves (reverse boards)))))

(defun util-strip-empty-entries (seq)
  (remove-if #'(lambda (x) (eql 0 (length x))) seq))

;;scratch
(defparameter *moves* '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1))

(defparameter *boards*
  (list
   (make-array '(5 5)
               :initial-contents
               '((22 13 17 11  0)
                 (8  2 23  4 24)
                 (21  9 14 16  7)
                 (6 10  3 18  5)
                 (1 12 20 15 19)))
   (make-array '(5 5)
               :initial-contents
               '((3 15  0  2 22)
                 (9 18 13 17  5)
                 (19  8  7 25 23)
                 (20 11 10 24  4)
                 (14 21 16 12  6)))
   (make-array '(5 5)
               :initial-contents
               '((14 21 17 24  4)
                 (10 16 15  9 19)
                 (18  8 23 26 20)
                 (22 11 13  6  5)
                 (2  0 12  3  7)))))

(mapcar
 (lambda (x)
   (if (eql t (check-board x *moves*))
       (calculate-board-score x (subseq *moves* 0 12))
       nil))
 *boards*)


(simulate-bingo *boards* *moves*) 

(multiple-value-bind (m b) (read-input "input.txt")
  (setf *boards* b)
  (setf *moves* m))
