;; Day 2

(defparameter *filename* "./test.txt")
(defvar *commands*)
(ql:quickload "cl-ppcre")

(defun main (filename)
  (let ((result nil)
        (commands
          (process-commands
           (mapcar #'parse-string-to-list
                   (read-file-to-list filename)))))
    ;;sum up list of command/coordinates
    (setf result (reduce (lambda (a b)
                           (list
                            (+ (first a) (first b))
                            (+ (second a) (second b))))
                         commands))
    (* (first result) (second result))))
    
            

(defun process-commands (commands)
  (mapcar (lambda (c)
            (apply #'exec-command c))
          commands))

(defun exec-command  (direction magnitude)
  (cond
    ((string= direction "forward")
     (list magnitude 0))
    ((string= direction "down")
     (list 0 magnitude))
    ((string= direction "up")
     (list 0 (* -1 magnitude)))
    (t (list 0 0))))

(defun read-file-to-list (filename)
  (with-open-file (in filename)
    (let ((commands nil))
      (loop for line = (read-line in nil)
            while line do (push line commands))
      (reverse commands))))

(defun parse-string-to-list (s)
  (ppcre:register-groups-bind
      (x (#'parse-integer y))
      ("(\\w+) (\\d+)" s :sharedp t)
    (list x y)))

;;; scratch below

;;not needed due to the parse integer funciton on the regex parsing
(destructuring-bind (x y) (first *commands*)
  (list x (parse-integer y)))

;; iterate on a list of commands and apply them to the result space
(mapcar (lambda (c) (apply #'exec-command c))
        '(("forward" 5) ("down" 5)))

;; sums up a list of coordinate pairs
(reduce (lambda (a b)
          (list
           (+ (first a) (first b))
           (+ (second a) (second b))))
        '((5 0) (0 5)))
