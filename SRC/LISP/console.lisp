(in-package :cl-user)

(defpackage :curses
  (:use :common-lisp)
  (:export :*portnum* :create-console :send-message :connect-console
           :close-console :erase :refresh :move
           :addch :mvaddch :addstr :mvaddstr
           :printw :mvprintw :attron :attroff 
           :getch :mvgetch :getyx :attrset))

(in-package :curses)

(defparameter *portnum* 8889)
(defparameter *console* nil)
(defparameter *forcesend* nil)
(defparameter *m-buffer* "")

(defun connect-console ()
  (setf *console* (socket:socket-connect *portnum* "localhost" 
                        :timeout 5 :external-format :dos)))

(defun create-console (exe)
  (ext:run-program exe :wait nil) (connect-console))

(defun send-message (str)
  (if *forcesend*
      (progn
        (when (> (length *m-buffer*) 0) (princ *m-buffer* *console*))
        (princ str *console*)
        (setf *forcesend* nil *m-buffer* ""))
      (if (> (+ (length str) (length *m-buffer*)) 250)
          (progn (princ *m-buffer* *console*)
                 (setf *m-buffer* str))
          (setf *m-buffer* (concatenate 'string *m-buffer* str)))))

(defun close-console () (setf *forcesend* t) (send-message "(QUIT)"))
(defun erase () (send-message "(ERASE)"))
(defun refresh () (setf *forcesend* t) (send-message "(REFRESH)"))

(defun move (y x)
  (send-message (format nil "~s" (list 'move y x))))

(defun addch (ch)
  (if (characterp ch) (setf ch (char-code ch)))
  (send-message (format nil "~s" (list 'addch ch))))

(defun mvaddch (y x ch)
  (if (characterp ch) (setf ch (char-code ch)))
  (send-message (format nil "~s" (list 'mvaddch y x ch))))

(defun addstr (str)
  (send-message (format nil "~s" (list 'addstr str))))

(defun mvaddstr (y x str)
  (send-message (format nil "~s" (list 'mvaddstr y x str))))

(defun printw (str)
  (send-message (format nil "~s" (list 'printw str))))

(defun mvprintw (y x str)
  (send-message (format nil "~s" (list 'mvprintw y x str))))

(defun attron (attr)
  (send-message (format nil "~s" (list 'attron attr))))

(defun attroff (attr)
  (send-message (format nil "~s" (list 'attroff attr))))

(defun getch ()
  (setf *forcesend* t)
  (send-message "(GETCH)")
  ;(format t "Getch sent...")
  (read *console*))

(defun mvgetch (y x)
  (setf *forcesend* t)
  (send-message (format nil "~s" (list 'mvgetch y x)))
  (read *console*))

(defun getyx ()
  (setf *forcesend* t)
  (send-message "(GETYX)")
  (read *console*))

(defun attrset (attr)
  (send-message (format nil "~s" (list 'attrset attr))))



