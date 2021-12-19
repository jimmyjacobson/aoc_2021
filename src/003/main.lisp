;;
(defparameter *test-data*
  (list #b00100
        #b11110
        #b10110
        #b10111
        #b10101
        #b01111
        #b00111
        #b11100
        #b10000
        #b11001
        #b00010
        #b01010))



;; scratch
(count 0 (mapcar (lambda (x) (ash x -4)) *test-data*)) 

(let ((gammaList
        (loop for i from -11 to 0 
              collecting 
              (let ((seq (mapcar (lambda (x) (ash x i)) *test-data*)))
                (cond
                  ((> (count 1 seq)
                      (count 0 seq))
                   1)
                  (t 0))))))
  (print gammaList)
  (parse-integer (format nil "~{~a~}" gammaList) :radix 2))

(let ((epsilonList
        (loop for i from -11 to 0
              collecting
              (let ((seq (mapcar (lambda (x) (ash x i)) *test-data*)))
                (cond
                      ((> (count 1 seq)
                          (count 0 seq))
                       0)
                      (t 1))))))
  (print epsilonList)
  (parse-integer (format nil "~{~a~}" epsilonList) :radix 2))


(setf *test-data* nil)
(with-open-file (in "../003/input.txt")
  (loop for line = (read-line in nil)
        while line do (push (parse-integer line :radix 2) *test-data*))
  (setf *test-data* (reverse *test-data*)))

(* 1032 3063)
