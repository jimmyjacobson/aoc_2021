;;; 8
(ql:quickload "cl-ppcre")

;;; part 1

(with-open-file (in "input.txt")
  (let ((digits '()))
    (loop for line = (read-line in nil)
          while line
          do (dolist (item (parse-line-to-list line))
               (if (easy-digit-p item)
                   (push item digits)))
          finally (print (length digits)))))

(defun parse-line-to-list (line)
  (ppcre:split "\\s" (subseq line (+ 2 (position #\| line)))))

(defun easy-digit-p (str)
  (and (not (eql 5 (length str)))
      (not (eql 6 (length str)))))

;;; part 2, use a tree for mapping code to value
(defparameter *keys* (make-hash-table :test #'equalp))

(defun main-2 (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          with codes
          with output
          with result
          do 
             (setf *keys* (make-hash-table :test #'equalp))
             (setf codes
                   (ppcre:split "\\s" (subseq line 0 (position #\| line))))
             (setf output
                   (ppcre:split "\\s" (subseq line (+ 2 (position #\| line)))))
             (build-code-book codes)
             (setf result (compute-output output))
             (format t "Input:~% ~a ~%Output:~% ~a~%~%" line result)
          sum result)))

(defun build-code-book (codes)
  ;;takes a list of codes and populates *keys* with a little brute force
  (loop until (eql 20 (hash-table-count *keys*))
        do (dolist (code codes) (decode code))))

(defun decode (str)
  ;; decodes the 7 signal codes into numbers and stores in hash
  (let ((len (length str)))
    (cond
      ;; if length = 2 you know 1
      ((eql 2 len)
       (add-to-hash str 1))
      ;; if length = 3 you know 7
      ((eql 3 len)
       (add-to-hash str 7))
      ;; if length = 4 you know 4
      ((eql 4 len)
       (add-to-hash str 4))
      ;; if length = 5 and you know 1, you know 3
      ;; because 1 is a subset of 3
      ((and (eql 5 len)
            (subset-p (get-from-hash 1) str))
       (add-to-hash str 3))
      ;; if length = 5 and you know 9  you know 5
      ;; because 5 is a subset of 9
      ((and (eql 5 len)
            (subset-p str (get-from-hash 9)))
       (add-to-hash str 5))
      ;; if length = 5 and you know 3 and 5 you know 2
      ;; because it's what remains
      ((and (eql 5 len)
            (get-from-hash 3)
            (get-from-hash 5))
       (add-to-hash str 2))
      ;; if length = 6 and you know 4 you know 9
      ;; because 4 is a subset of 9
      ((and (eql 6 len)
            (subset-p (get-from-hash 4) str))
       (add-to-hash str 9))
      ;; if length = 6 and you know 5  you know 6
      ;; because 5 is a subset of 6
      ((and (eql 6 len)
            (subset-p (get-from-hash 5) str))
       (add-to-hash str 6))
      ;; if length = 6 and you know 9 and 6 you know 0
      ;; because it's what remains
      ((and (eql 6 len)
            (get-from-hash 6)
            (get-from-hash 9))
       (add-to-hash str 0))
      ;;if length = 7 you know 8
      ((eql 7 len)
       (add-to-hash str 8))
      (nil))))


(defun subset-p (str target)
  ;; loops over each character in string and tests for existence in target
  (if (or (not str) (not target))
      nil
      (loop for i from 0 to (- (length str) 1)
            do (if (not (find (aref str i) target)) (return))
            finally (return t))))

(defun add-to-hash (key value)
  ;; add key value and inverse to hash
  (setf (gethash (sort key #'string<) *keys*) value)
  (setf (gethash value *keys*) (sort key #'string<)))

(defun get-from-hash (key)
  (if (stringp key)
      (gethash (sort key #'string<) *keys*)
      (gethash key *keys*)))

(defun compute-output (output)
  ;; compute output signal using code book
  ;; this could be way cleaner but I'm losing patience
  (+ (* 1000 (get-from-hash (first output)))
     (* 100 (get-from-hash (second output)))
     (* 10 (get-from-hash (third output)))
     (get-from-hash (fourth output))))
;;; scratch
(defparameter *codes* (ppcre:split "\\s" "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"))

(+ (* 1000 (get-from-hash "cdfeb"))
   (* 100 (get-from-hash "fcadb"))
   (* 10 (get-from-hash "cdfeb"))
   (get-from-hash "cdbaf"))

(main-2 "input.txt")
