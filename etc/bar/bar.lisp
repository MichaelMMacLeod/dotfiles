(require "asdf")

(asdf:operate 'asdf:load-op 'cl-ppcre)

(defvar *background* #x00000000)
(defvar *foreground* #xffffffff)
(defvar *underline* #xffffffff)
(defvar *alignment* "l")

(defparameter +clock+ #xf017)
(defparameter +calandar+ #xf073)
(defparameter +volume-up+ #xf028)
(defparameter +volume-down+ #xf027)
(defparameter +volume-off+ #xf026)
(defparameter +wifi+ #xf1eb)
(defparameter +desktop+ #xf108)
(defparameter +solid-right-arrow+ #xe0b0)
(defparameter +firefox+ #xf269)
(defparameter +terminal+ #xf120)
(defparameter +angle-double-up+ #xf102)
(defparameter +battery-full+ #xf240)

(defun bash (program-str &key (discard-newline t))
  (let ((input (make-string-input-stream program-str)))
    (unwind-protect
      (let ((output (with-output-to-string (output)
                      (sb-ext:run-program "bash" nil 
                                          :search t 
                                          :input input 
                                          :output output))))
        (if (and discard-newline (> (length output) 0))
          (subseq output 0 (- (length output) 1))
          output))
      (close input))))

;; Create a banner which scrolls if its text is past a cutoff point.
;;
;; keyword arguments;
;;   :cutoff is the maximum length of the resulting string. It defaults to 30.
;;   :seperator is a string which is placed after the end and before the start of a
;;              rotating banner. It defaults to a space.
;;
;; usage:
;;   (let ((banner (make-banner "abcde" :cutoff 4 :seperator "|")))
;;     (funcall banner)  ; "abcd"
;;     (funcall banner)  ; "bcde"
;;     (funcall banner)  ; "cde|"
;;     (funcall banner)) ; "de|a"
;;
;; The closure returned by make-banner takes an optional string as an argument. If 
;; provided, the closure will update its banner to be that new string.
(defun make-banner (string &key (cutoff 30) (seperator " "))
  (let ((n 0))
    (lambda (&optional new-string)
      (unless (or (not new-string) (string= string new-string))
        (setf string new-string)
        (setf n 0))
      (let ((result
              (if (< (length string) cutoff)
                string
                (subseq (format nil "~a~a~a" (subseq string n) seperator (subseq string 0 n))
                        0 cutoff))))
        (if (= n (1- (length string)))
          (setf n 0)
          (incf n))
        result))))

(defun get-date () 
  (bash "date +'%A, %b %d'"))

(defun get-time () 
  (bash "date +'%H:%M'"))

(defun get-volume () 
  (bash "awk -F '[][]' '{ print $2 }' <(amixer -D pulse sget Master) | tail -n 1"))

(let ((banner (make-banner "")))
  (defun get-focused-window ()
    (let* ((window-name (bash "xtitle $(bspc query -N -n) | cut -d ':' -f 2")))
      (funcall banner window-name))))

(defun get-battery ()
  (let ((lines (cl-ppcre:split
                 #\Newline
                 (bash "awk -F ': ' '{ print $2 }' <(acpi)"))))
    (if (< (length (first lines)) (length (second lines)))
      (first lines)
      (second lines))))

(defun get-wifi ()
  (let ((wifi-status (cl-ppcre:split
                       ":"
                       (bash "nmcli --terse device wifi | grep '*'"))))
    (format nil "~a ~a" (nth 1 wifi-status) (nth 4 wifi-status))))

(defun get-focused-desktop ()
  (bash "xprop -root _NET_CURRENT_DESKTOP | cut -d ' ' -f 3"))

(defun bar (&rest xs)
  (princ
    (with-output-to-string (s)
      (dolist (x xs)
        (princ x s))
      (princ #\Newline s))))

(defun foreground (color &rest xs)
  (with-output-to-string (s)
    (let ((old-foreground *foreground*))
      (setf *foreground* color)
      (format s "%{F#~8,'0x}" *foreground*)
      (dolist (x xs)
        (princ x s))
      (setf *foreground* old-foreground)
      (format s "%{F#~8,'0x}" *foreground*))))

(defun background (color &rest xs)
  (with-output-to-string (s)
    (let ((old-background *background*))
      (setf *background* color)
      (format s "%{B#~8,'0x}" *background*)
      (dolist (x xs)
        (princ x s))
      (setf *background* old-background)
      (format s "%{B#~8,'0x}" *background*))))

(defun underline (color &rest xs)
  (with-output-to-string (s)
    (let ((old-underline *underline*))
      (setf *underline* color)
      (format s "%{U#~8,'0x}%{+u}" *underline*)
      (dolist (x xs)
        (princ x s))
      (setf *underline* old-underline)
      (format s "%{U#~8,'0x}%{-u}" *underline*))))

(defun left (&rest xs)
  (with-output-to-string (s)
    (let ((old-alignment *alignment*))
      (setf *alignment* "l")
      (format s "%{~a}" *alignment*)
      (dolist (x xs)
        (princ x s))
      (setf *alignment* old-alignment)
      (format s "%{~a}" *alignment*))))

(defun center (&rest xs)
  (with-output-to-string (s)
    (let ((old-alignment *alignment*))
      (setf *alignment* "c")
      (format s "%{~a}" *alignment*)
      (dolist (x xs)
        (princ x s))
      (setf *alignment* old-alignment)
      (format s "%{~a}" *alignment*))))

(defun right (&rest xs)
  (with-output-to-string (s)
    (let ((old-alignment *alignment*))
      (setf *alignment* "r")
      (format s "%{~a}" *alignment*)
      (dolist (x xs)
        (princ x s))
      (setf *alignment* old-alignment)
      (format s "%{~a}" *alignment*))))

(defun bar-symbol (symbol)
  (code-char symbol))

(defun bar-string (string)
  string)

(defun bar-padding (&optional (n 1))
  (with-output-to-string (s)
    (dotimes (i n)
      (princ " " s))))

(defun command (command &rest xs)
  (with-output-to-string (s)
    (format s "%{A:~a:}" command)
    (dolist (x xs)
      (princ x s))
    (format s "%{A}")))

;(defun foreground (color &rest xs)
;  (with-output-to-string (s)
;    (let ((old-foreground *foreground*))
;      (setf *foreground* color)
;      (format s "%{F#~8,'0x}" *foreground*)
;      (dolist (x xs)
;        (princ x s))
;      (setf *foreground* old-foreground)
;      (format s "%{F#~8,'0x}" *foreground*))))
;
;(defun background (color &rest xs)
;  (with-output-to-string (s)
;    (let ((old-background *background*))
;      (setf *background* color)
;      (format s "%{B#~8,'0x}" *background*)
;      (dolist (x xs)
;        (princ x s))
;      (setf *background* old-background)
;      (format s "%{B#~8,'0x}" *background*))))

(defun make-module (&key background foreground format function)
  (lambda ()
    (foreground
     foreground
     (background
      background
      (format nil format (funcall function))))))

(defvar *desktop-module* (make-module :background #xff761c0d
                                      :foreground #xfffefef3
                                      :format " ~a "
                                      :function #'get-focused-desktop))


(defmacro bar+ (&key left)
  `(bar
    (left
     (progn
       ,@(loop for module in left collect `(funcall ,module))))))

;(loop
;  (bar+ :left (*desktop-module*))
;  (sleep 0.5))

;;; (bar+ :left (*desktop-module* *another-module*))
;;; (loop
;;;   (left
;;;     (funcall *desktop-module*)
;;;     (funcall *another-module*))
;;;   (sleep 0.5))


(loop
  (bar
    (left
      (foreground
        #xfffefef3
        (background
          #x00000000
          (background
            #xff761c0d
            (bar-padding)
            (bar-symbol +desktop+)
            (bar-padding)
            (bar-string (get-focused-desktop))
            (bar-padding))
          (bar-padding)
          (background
            #xff393130
            (bar-padding)
            (bar-string (get-focused-window))
            (bar-padding)))))
    (center
      (foreground
        #xfffefef3
        (background
          #x00000000
          (background
            #xff393130
            (bar-padding)
            (bar-symbol +calandar+)
            (bar-padding)
            (bar-string (get-date))
            (bar-padding))
          (bar-padding)
          (background
            #xff393130
            (bar-padding)
            (bar-symbol +clock+)
            (bar-padding)
            (bar-string (get-time))
            (bar-padding)))))
    (right
      (foreground
        #xfffefef3
        (background
          #x00000000
          (background
            #xff393130
            (bar-padding)
            (bar-symbol +volume-up+)
            (bar-padding)
            (bar-string (get-volume))
            (bar-padding))
          (bar-padding)
          (background
            #xff393130
            (bar-padding)
            (bar-symbol +wifi+)
            (bar-padding)
            (bar-string (get-wifi))
            (bar-padding))
          (bar-padding)
          (background
            #xff393130
            (bar-padding)
            (bar-symbol +battery-full+)
            (bar-padding)
            (bar-string (get-battery))
            (bar-padding))))))
  (sleep 0.5))
