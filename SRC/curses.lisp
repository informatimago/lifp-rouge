
(in-package :cl-user)

(defpackage :curses
  (:use :common-lisp :cffi)
  (:export :refresh :erase :move
           :create-console :close-console :connect-console
           :echo :noecho
           :addch :mvaddch :addstr :mvaddstr
           :printw :mvprintw :attron :Attroff
           :attrset :getch :mvgetch :getyx :attrset
           :getstr :mvgetstr :getnstr :mvgetnstr
           ))

(in-package :curses)

(define-foreign-library curses
  (:unix (:default "ncurses.so"))
  (t (:default "curses")))

;(eval-when (:compile-toplevel :load-toplevel :execute)

(use-foreign-library curses);)

(defctype chtype :long)

(defcfun "initscr" :pointer)

(defcfun "noecho" :int)

(defcfun "echo" :int)

(defcfun "nonl" :int)

(defcfun "refresh" :int)

(defcfun "erase" :int)

(defcfun "keypad" :int (wnd :pointer) (bool :boolean))

(defcvar "stdscr" :pointer)

(defcfun "cbreak" :int)

(defcfun "nocbreak" :int)

(defcfun "start_color" :int)

(defcfun "intrflush" :int (wnd :pointer) (bool :boolean))

(defcfun "endwin" :int)

(defcfun "nodelay" :int (wnd :pointer) (bool :boolean))


(defcfun "init_pair" :int (pair :short) 
         (foreground :short) (background :short))


;;Default colors
(defconstant +color-black+ 0)
(defconstant +color-blue+ #x0001)
(defconstant +color-green+ #x0002)
(defconstant +color-red+ #x0004)
(defconstant +color-cyan+ (logior +color-blue+ +color-green+))
(defconstant +color-magenta+ (logior +color-red+ +color-blue+))
(defconstant +color-yellow+ (logior +color-red+ +color-green+))
(defconstant +color-white+ (logior +color-red+ +color-blue+ +color-green+))
(defconstant +a-bold+ #x00800000)

(defun init-colors ()
  (init-pair +color-black+ +color-black+ +color-black+)
  (init-pair +color-blue+ +color-blue+ +color-black+)
  (init-pair +color-green+ +color-green+ +color-black+)
  (init-pair +color-red+ +color-red+ +color-black+)
  (init-pair +color-cyan+ +color-cyan+ +color-black+)
  (init-pair +color-magenta+ +color-magenta+ +color-black+)
  (init-pair +color-yellow+ +color-yellow+ +color-black+)
  (init-pair +color-white+ +color-white+ +color-black+))

;;Compatibility layer for old socket interface
(defun connect-console ()
  (use-foreign-library curses)
  (initscr)
  (keypad *stdscr* t)
  (nonl) (cbreak) (noecho)
  (intrflush *stdscr* nil)
  (start-color)
  (init-colors))

(defun create-console (exe)
  (declare (ignore exe))
  (connect-console))

(defun close-console () (endwin))

;;Functions that do something interesting

(defcfun "move" :int (y :int) (x :int))

(defcfun ("addch" c-addch) :int (char chtype))

(defun addch (char)
  (c-addch (char-code char)))

(defcfun ("mvaddch" c-mvaddch) :int (y :int) (x :int) (char chtype))

(defun mvaddch (y x char)
  (c-mvaddch y x (char-code char)))

(defcfun "addstr" :int (string :string))

(defcfun "mvaddstr" :int (y :int) (x :int) (string :string))

;;The next two functions are just synonyms for the two above. If you want
;;control strings, use format.

(defun printw (str) (addstr str))

(defun mvprintw (y x str) (mvaddstr y x str))

;;Attributes conversion

;(defconstant +a-attributes+ #xffff0000L)

(defconstant +a-attributes+ #xffffff00)

(defun color-pair (n)
  (ash n 24))

(defun color-code (attr)
  (case attr
    (:cblack (color-pair +color-black+))
    (:cblue (color-pair +color-blue+))
    (:CDGREEN (color-pair +color-GREEN+))
    (:CCYAN (color-pair +color-CYAN+))
    (:CRED (color-pair +color-RED+))
    (:CPURPLE (color-pair +color-MAGENTA+))
    (:CBROWN (color-pair +color-YELLOW+))
    (:CGRAY (color-pair +color-WHITE+))
    (:CDARK (+ (color-pair +color-BLACK+) +a-bold+))
    (:CLBLUE (+ (color-pair +color-BLUE+) +a-bold+))
    (:CGREEN (+ (color-pair +color-GREEN+) +a-bold+))
    (:CSKY (+ (color-pair +color-CYAN+) +a-bold+))
    (:CROSE (+ (color-pair +color-RED+) +a-bold+))
    (:CPINK (+ (color-pair +color-MAGENTA+) +a-bold+))
    (:CYELLOW (+ (color-pair +color-YELLOW+) +a-bold+))
    (:CWHITE (+ (color-pair +color-WHITE+) +a-bold+))
    (t 0)))

(defcfun ("attron" c-attron) :int (attr :int))

(defcfun ("attroff" c-attroff) :int (attr :int))



(defun attron (attr)
  (c-attron (color-code attr)))

(defun attroff (attr)
  (c-attroff (color-code attr)))

;;curses.dll doesn't have attrset for some reason
(defcfun ("wattrset" c-attrset) :int (window :pointer) (attr :int))

(defun attrset (attr)
  (c-attrset *stdscr* (color-code attr)))


;(defcfun ("getch" c-getch) :int)

(defcfun "wgetch" :int (window :pointer))

(defun getch ()
  (refresh)
  (let ((c (wgetch *stdscr*)))
    (mvprintw 24 1 (format nil "[~d]" c)) (refresh)
    c))

(defcfun ("mvgetch" c-mvgetch) :int (y :int) (x :int))

(defun mvgetch (y x)
  (let ((c (c-mvgetch y x)))
    (mvprintw 24 1 (format nil "[~d]" c)) (refresh)
    c))

(defcstruct window
  (cury :int)
  (curx :int)
  (maxy :int)
  (maxx :int))

(defun getyx ()
  (with-foreign-slots ((cury curx) *stdscr* window)
    (list cury curx)))

;;Getstr

(defcfun ("getstr" c-getstr) :int (str :pointer))

(defun getstr ()
  (prog2
      (echo)
      (with-foreign-pointer-as-string (str 80)
        (c-getstr str))
    (noecho)))

(defcfun ("mvgetstr" c-mvgetstr) :int (y :int) (x :int) (str :pointer))

(defun mvgetstr (y x)
  (prog2
      (echo)
      (with-foreign-pointer-as-string (str 80)
        (c-mvgetstr y x str))
    (noecho)))

;;curses.dll doesn't have getnstr and mvgetnstr for some reason

(defcfun ("wgetnstr" c-getnstr) :int (window :pointer) (str :pointer) (n :int))

(defun getnstr (n) 
  (prog2
      (echo)
      (with-foreign-pointer-as-string (str 80);n)
        (c-getnstr *stdscr* str n))
    (noecho)))

;(defcfun ("wmvgetstr" c-mvgetnstr) :int (window :pointer) (y :int) (x :int) 
;         (str :pointer) (n :int))

(defun mvgetnstr (y x n) 
  (move y x)
  (prog2
      (echo)
      (with-foreign-pointer-as-string (str 80);n)
        (c-getnstr *stdscr* str n))
    (noecho)))