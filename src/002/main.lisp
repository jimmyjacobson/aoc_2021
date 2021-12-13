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

(defun main-2 (filename)
  (let ((results nil)
        (commands
          (process-commands-with-aim
           (mapcar #'parse-string-to-list
                   (read-file-to-list filename)))))
    (setf results (reduce (lambda (a b)
                           (list
                            (+ (first a) (first b))
                            (+ (second a) (second b))))
                         commands))
    (* (first results) (second results))))
    
            

(defun process-commands (commands)
  (mapcar (lambda (c)
            (apply #'exec-command c))
          commands))

(defun process-commands-with-aim (commands)
  (let ((aim 0))
    (mapcar (lambda (c)
              (let ((command (append c (list aim)))
                    (response nil))
                (setf response (apply #'exec-command-with-aim command))
                (setf aim (third response))
                response))
            commands)))

(defun exec-command  (direction magnitude &optional (aim 0))
  (cond
    ((string= direction "forward")
     (list magnitude 0 aim))
    ((string= direction "down")
     (list 0 magnitude (+ aim magnitude)))
    ((string= direction "up")
     (list 0 (* -1 magnitude) (- aim magnitude)))
    (t (list 0 0 0))))

(defun exec-command-with-aim (direction magnitude aim)
  (cond
    ((string= direction "forward")
     (list magnitude (* magnitude aim) aim))
    ((string= direction "down")
     (list 0 0 (+ aim magnitude)))
    ((string= direction "up")
     (list 0 0 (- aim magnitude)))
    (t (list 0 0 0))))

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
        (process-commands-with-aim *commands*))
