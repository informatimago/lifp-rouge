;;The Rougelike!

#|
Ideas and notes:
*the game should be finite
**difficulty increases as the time goes by +
***admins sometimes snap and go on deleting rampage (not in this version...)
**trolls +
**other methods to lose other than dying +
***Wikipedia closes if vandalism is too big +
****WikiDefCon!!! +
***Jimbo leaves? +
*hi-score +
*make it possible to configure keybindings +
|#

;;Table of contents:
;;SECTION 1: Declarations + controls
;;SECTION 1a: Stray class definitions
;;SECTION 2: Tiles
;;SECTION 3: Monsters
;;SECTION 3a: Player
;;SECTION 3b: Noob
;;SECTION 3c: Vandal
;;SECTION 3d: Normal User
;;SECTION 3e: Admin
;;SECTION 3f: Bureaucrat
;;SECTION 3g: Jimbo
;;SECTION 3h: Troll
;;SECTION 4: Items
;;SECTION 5: Actions
;;SECTION 5a: Combat
;;SECTION 6: "LOS" and Events
;;SECTION 7: Stack processing
;;SECTION 8: Generating dungeon
;;SECTION 9: Printing routines
;;SECTION 10: Main sequence

(in-package :cl-user)

(defpackage :rougelike
  (:use :common-lisp :curses :md5)
  (:shadow :time :speed :position))

(in-package :rougelike)

;;SECTION 1: Declarations + controls

(defparameter *map* nil)
(defparameter *dx* 2) (defparameter *dy* 1)
(defparameter *roompoints* nil)
(defparameter *monsterstack* nil
  "Each element is (monster . time)")
(defparameter *player* nil)
(defparameter *controls* (make-hash-table :test #'eql))
(defparameter *quitflag* nil)
(defparameter *messages* nil)
(defparameter *curmonster* nil)
(defparameter *zero-time-action* nil)
(defparameter *colors* '(:cblue :cdgreen :ccyan :cred :cpurple :cbrown :cgray
                         :cdark :clblue :cgreen :csky :crose :cpink :cyellow
                         :cwhite))
(defparameter *allmessages* nil)

(defparameter *diffmod* 1)

(defun diffmod (n)
  (floor (* n *diffmod*)))



(defun control-bind (action &rest keys)
  (dolist (i keys)
    (setf (gethash i *controls*) action)))

(defun init-controls ()
  (setf *controls* (make-hash-table :test #'eql))
  (when (probe-file "controls.cfg")
    (with-open-file (i "controls.cfg")
      (let* ((*read-eval* nil)
             (remapping (read i)))
        (loop for remap in remapping
             do (apply #'control-bind
                       (apply #'make-instance (car remap))
                       (cdr remap)))))))

(defgeneric print-char (obj)
  (:documentation "Returns two values: a character that
  represents tile or monster and it's color (keyword)"))

(defgeneric description (obj)
  (:documentation "Returns a string, describing a tile or monster")
  (:method (x) (declare (ignore x)) "No description yet."))

(defun random-list (list)
  (nth (random (length list)) list))

;;SECTION 1a: Stray class definitions

(defclass event ()
  ((interesting :initform t :initarg :interesting :accessor interesting)
   (x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defclass e-step (event)
  ((interesting :initform nil)
   (monster :initarg :monster :accessor monster)))

(defclass e-item (event)
  ((monster :initarg :monster :accessor monster)
   (item :initarg :item :initform nil :accessor item)))

(defclass e-delete-item (e-item) ())

(defclass e-delete-good (e-delete-item) ())

(defclass e-delete-junk (e-delete-item) ())

(defclass e-delete-userbox (e-delete-item) ())

(defclass e-combat (event)
  ((attacker :initarg :attacker :accessor attacker)
   (defender :initarg :defender :accessor defender)))

(defclass e-c-miss (e-combat) ())

(defclass e-c-hit (e-combat)
  ((damage :initarg :damage :accessor damage)))

;;SECTION 2: Tiles

(defclass tile ()
  ((x :initarg :x :accessor x) (y :initarg :y :accessor y)
   (monster :initarg :monster :initform nil :accessor monster)
   (passable :initform nil :reader passable)
   (bloodied :initform nil :accessor bloodied)
   (items :initform nil :accessor items)))

(defmethod print-char ((tile tile)) (values #\? :CGRAY))

(defmethod print-char :around ((tile tile))
  (if (monster tile) (print-char (monster tile))
      (if (items tile) (print-char (car (items tile)))
          (call-next-method))))

(defmethod description :around ((tile tile))
  (if (monster tile)
      (format nil "~a ~a" (call-next-method) (description (monster tile)))
      (call-next-method)))

(defclass concrete-floor (tile) ((passable :initform t)))

(defmethod print-char ((tile concrete-floor))
  (values #\. (if (bloodied tile) :CRED :CGRAY)))

(defmethod description ((tile concrete-floor))
  "Concrete floor.")

(defclass wall (tile) ())

(defmethod print-char ((tile wall))
  (values #\# (if (bloodied tile) :CRED :CGRAY)))

(defmethod description ((tile wall))
  "Concrete wall.")

(defclass staircase (concrete-floor)
  ((direction :initarg :direction :reader direction)))

(defmethod print-char ((tile staircase))
  (values (ecase (direction tile) (:u #\<) (:d #\>))
          (if (bloodied tile) :CRED :CDARK)))

(defmethod description ((tile staircase))
  (format nil "A staircase leading ~a." (ecase (direction tile) (:u "up") (:d "down"))))

;;SECTION 3: Monsters

(defgeneric ai (monster)
  (:documentation "When called, returns an action that monster
  attempts to do."))

(defgeneric do-action (monster action)
  (:documentation "Execute an action"))

(defgeneric kill (monster)
  (:documentation "Kill teh monster!"))

(defgeneric react (monster event)
  (:documentation "How a monster would react to an event.")
  (:method (m e) (declare (ignore m e)) nil))


;(defmethod react :before (mon event)
;  (format t "Monster ~a reacts to event ~a.~%" mon event))

(defclass monster ()
  ((name :initform "monster" :initarg :name :accessor name)
   (x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (hp :initform 0 :initarg :hp :accessor hp)
   (max-hp :initarg :max-hp :accessor max-hp)
   (inventory :initform nil :accessor inventory)
   (hat :initform nil :accessor hat)
   (shirt :initform nil :accessor shirt)
   (boots :initform nil :accessor boots)
   (weapon :initform nil :accessor weapon)
   (speed :initform 1 :accessor speed :documentation "speed modifier")
   (state :initform nil :accessor state :documentation "AI state")
   (target :initform nil :accessor target)
   (targetpath :initform nil :accessor targetpath)
   (strength :initform 2 :accessor strength)
   (dexterity :initform 2 :accessor dexterity)
   (awareness :initform 4 :accessor awareness)
   (karma :initform 0 :accessor karma)
   (rouge :initform 0 :accessor rouge)))

(defmethod initialize-instance :after ((monster monster) &key)
  (setf (hp monster) (max-hp monster)))

(defmethod print-char ((monster monster)) (values #\? :CPINK))

(defmethod kill ((monster monster))
  (cast (make-instance 'e-dead :monster monster))
  (setf *monsterstack* (delete monster *monsterstack* :key #'car))
  (setf (monster (aref *map* (y monster) (x monster))) nil)
  (out-char (aref *map* (y monster) (x monster))))


(defun neighbors (y x)
  (list (cons (1+ y) x) (cons (1- y) x)
        (cons (1+ y) (1+ x)) (cons (1+ y) (1- x))
        (cons y (1+ x)) (cons y (1- x))
        (cons (1- y) (1+ x)) (cons (1- y) (1- x))))

(defun free-neighbor-deltas (y x)
  (loop for n in (remove-if (lambda (a) (occupied (car a) (cdr a)))
                            (neighbors y x))
       collecting (cons (- (car n) y) (- (cdr n) x))))

(defun near (monster1 monster2)
  (and (<= (abs (- (x monster1) (x monster2))) 1)
       (<= (abs (- (y monster1) (y monster2))) 1)
       (> (hp monster1) 0) (> (hp monster2) 0)))

(defun nearby-monsters (monster)
  (loop for tile in (neighbors (y monster) (x monster))
        for m = (monster (aref *map* (car tile) (cdr tile)))
        when m collect m))

(defun find-nearby-monster (monster type)
  (let ((mons (remove-if (complement (lambda (x) (typep x type)))
                         (nearby-monsters monster))))
    (if mons (random-list mons) nil)))

(defun follow (monster1 monster2)
  (let ((x1 (x monster1)) (y1 (y monster1))
        (x2 (x monster2)) (y2 (y monster2)))
    (let ((possible-moves (remove-if (lambda (a) (occupied (car a) (cdr a)))
                                     (neighbors y1 x1))))
      (when possible-moves
      (loop with bestm = (car possible-moves)
         with temp = 1000
         for m in possible-moves
         for d = (distance (car m) (cdr m) y2 x2)
         when (< d temp) do (setf temp d bestm m)
         finally (return bestm))))))

(defun anger (monster target)
  (unless (eql (state monster) :angry)
    ;;(format t "~a gets angry.~%" monster)
    (setf (state monster) :angry (target monster) target)
    (cast (make-instance 'e-m-angry :monster monster))))

;;SECTION 3a: Player

(defclass player (monster)
  ((name :initform "you")
   (max-hp :initform 10)
   (speed :initform 1)
   (inventory :initform (list (make-instance 'banhammer)))))

(defmethod print-char ((monster player)) (values #\@ :CPINK))

(defmethod description ((monster player)) "You are here.")

(defmethod ai ((monster player))
  ;(format t "sending getch...")
  (move (+ *dy* (y monster)) (+ *dx* (x monster))) (refresh)
  (let ((key (getch)))
    (multiple-value-bind (action present-p) (gethash key *controls*)
      (if present-p
          (progn (if (zerop (time action)) (setf *zero-time-action* t))
                 action)
          (make-instance 'no-action :key key)))))

(defmethod ai :before ((monster player))
  (show-status) (show-items (y *player*) (x *player*))
  (if *zero-time-action* (setf *zero-time-action* nil)
      (progn (setf *messages* (nreverse *messages*))
             (if *messages*
                 (display-message (pop *messages*))
                 (clean-line 0)))))

(defmethod ai :after ((monster player))
  ;(format t "<~a>~%" *zero-time-action*)
  (unless *zero-time-action* (setf *messages* nil)))

(defmethod kill ((monster player))
  (setf *quitflag* :kill)
  (call-next-method))

(defmethod react ((monster player) event)
  (when (interesting event)
    (message (description event))))

(defmethod react ((monster player) (event e-step))
  (with-slots (monster y x) event
    (when (eql monster *player*)
      (show-items y x))))

;;SECTION 3b: Noob

(defclass noob (monster)
  ((name :initform "n00b")
   (max-hp :initform 1)
   (speed :initform 9/10)
   (state :initform :clueless)))
   ;;(state :initform :angry)
   ;;(target :initform *player*)));;:clueless)))

(defmethod print-char ((monster noob)) (values #\n :CGREEN))

(defmethod description ((monster noob)) "A clueless n00b.")

(defmethod ai ((monster noob) &aux temp)
  "Noob can be either clueless or angry."
  (case (state monster)
    (:clueless ;;Walk around randomly, creating junk articles and
               ;;screwing existing ones.
     (if (items (aref *map* (y monster) (X monster)))
         (when (and (setf temp (find-good-article (y monster) (x monster)))
                    (zerop (random 100)))
           (return-from ai (make-instance 'vandalize :article temp)))
         (progn
           (case (random 300)
             (0 (return-from ai (make-instance 'spawn-junk)))
             (1 (return-from ai (make-instance 'spawn-good)))
             (2 (return-from ai (make-instance 'spawn-userbox))))))
     (let ((dx (1- (random 3)))
           (dy (1- (random 3))))
       (if (= dx dy 0) (return-from ai (ai monster))
           (make-instance 'move-to :dx dx :dy dy))))
    (:angry
     (when (near monster (target monster))
       ;;(format t "ATTACK!~%")
       (return-from ai (make-instance 'attack :monster (target monster))))
     (when (or (<= (hp (target monster)) 0) (zerop (random 40)))
       ;;Cooling off
       (setf (state monster) :clueless) (setf (target monster) nil)
       (cast (make-instance 'e-m-calm :monster monster))
       (return-from ai (ai monster)))
     (destructuring-bind (yy . xx) (follow monster (target monster))
       (return-from ai (make-instance 'move-to
                                      :dx (- xx (x monster))
                                      :dy (- yy (y monster))))))))



(defmethod react ((monster noob) (event e-delete-junk))
  (when (zerop (random 3))
    (anger monster (monster event))))

(defmethod react ((monster noob) (event e-delete-good))
  (anger monster (monster event)))

(defmethod react ((monster noob) (event e-delete-userbox))
  (anger monster (monster event)))

(defmethod react ((monster noob) (event e-c-miss))
  (when (typep (defender event) 'noob)
    (anger monster (attacker event))))

(defmethod react ((monster noob) (event e-c-hit))
  (when (typep (defender event) 'noob)
    (anger monster (attacker event))))

;;SECTION 3c: Vandal

(defclass vandal (monster)
  ((name :initform "vandal")
   (max-hp :initform (diffmod 3))
   (speed :initform 8/10)
   (state :initform :evil)))

(defmethod print-char ((monster vandal)) (values #\v :CROSE))

(defmethod description ((monster vandal)) "A malicious vandal.")

(defmethod ai ((monster vandal) &aux temp)
  (case (state monster)
    (:evil
     (if (items (aref *map* (y monster) (X monster)))
         (when (setf temp (find-good-article (y monster) (x monster)))
           (return-from ai (make-instance 'vandalize :article temp :severe t)))
         (progn
           (case (random 50)
             (0 (return-from ai (make-instance 'spawn-junk))))))
     (let ((fnd (free-neighbor-deltas (y monster) (x monster))))
       (if fnd
           (let ((d (random-list fnd)))
             (make-instance 'move-to :dx (cdr d) :dy (car d)))
           (make-instance 'wait))))
    (:angry
     (when (near monster (target monster))
       ;;(format t "ATTACK!~%")
       (return-from ai (make-instance 'attack :monster (target monster))))
     (when (or (<= (hp (target monster)) 0) (zerop (random 40)))
       ;;Cooling off
       (setf (state monster) :evil) (setf (target monster) nil)
       (cast (make-instance 'e-m-calm :monster monster))
       (return-from ai (ai monster)))
     (destructuring-bind (yy . xx) (follow monster (target monster))
       (return-from ai (make-instance 'move-to
                                      :dx (- xx (x monster))
                                      :dy (- yy (y monster))))))))

(defmethod react ((monster vandal) (event e-c-miss))
  (when (typep (defender event) 'vandal)
    (anger monster (attacker event))))

(defmethod react ((monster vandal) (event e-c-hit))
  (when (typep (defender event) 'vandal)
    (anger monster (attacker event))))

(defclass uber-vandal (vandal)
  ((name :initform "vandal")
   (max-hp :initform (diffmod 10))
   (speed :initform 8/10)
   (state :initform :evil)))

(defmethod print-char ((monster uber-vandal)) (values #\V :CROSE))

(defmethod description ((monster uber-vandal)) "A very malicious vandal.")

(defmethod ai ((monster uber-vandal) &aux temp)
  (case (state monster)
    (:evil
     (if (items (aref *map* (y monster) (x monster)))
         (when (setf temp (find-good-article (y monster) (x monster)))
           (return-from ai (make-instance 'vandalize :article temp :severe t)))
         (progn
           (case (random 50)
             (0 (return-from ai (make-instance 'spawn-junk))))))
     (let ((fnd (free-neighbor-deltas (y monster) (x monster))))
       (if fnd
           (let ((d (random-list fnd)))
             (if (= (random 20) 12)
                 (make-instance 'spawn-sockpuppet
                                :position (cons (+ (y monster) (car d))
                                                (+ (x monster) (cdr d))))
                  (make-instance 'move-to :dx (cdr d) :dy (car d))))
           (make-instance 'wait))))
    (:angry
     (when (near monster (target monster))
       ;;(format t "ATTACK!~%")
       (return-from ai (make-instance 'attack :monster (target monster))))
     (when (or (<= (hp (target monster)) 0) (zerop (random 40)))
       ;;Cooling off
       (setf (state monster) :evil) (setf (target monster) nil)
       (cast (make-instance 'e-m-calm :monster monster))
       (return-from ai (ai monster)))
     (destructuring-bind (yy . xx) (follow monster (target monster))
       (return-from ai (make-instance 'move-to
                                      :dx (- xx (x monster))
                                      :dy (- yy (y monster))))))))

(defclass sockpuppet (vandal)
  ((name :initform "sockpuppet")
   (max-hp :initform (diffmod 1))
   (speed :initform 8/10)
   (state :initform :evil)))

(defmethod print-char ((monster sockpuppet)) (values #\s :CROSE))

(defmethod description ((monster sockpuppet)) "A trouble-making sockpuppet.")

(defclass vandalbot (vandal)
  ((name :initform "vandalbot")
   (max-hp :initform (diffmod 4))
   (speed :initform 16/10)
   (state :initform :evil)))

(defmethod print-char ((monster vandalbot)) (values #\b :CROSE))

(defmethod description ((monster vandalbot)) "An evil vandalbot.")

(defmethod ai ((monster vandalbot) &aux temp)
  (case (state monster)
    (:evil
     (if (items (aref *map* (y monster) (x monster)))
         (when (setf temp (find-good-article (y monster) (x monster)))
           (return-from ai (make-instance 'vandalize :article temp :severe t)))
         (progn
           (case (random 3)
             (0 (return-from ai (make-instance 'spawn-junk))))))
     (let ((fnd (free-neighbor-deltas (y monster) (x monster))))
       (if fnd
           (let ((d (random-list fnd)))
             (make-instance 'move-to :dx (cdr d) :dy (car d)))
           (make-instance 'wait))))))

(defmethod react ((monster vandalbot) (event e-c-miss)) nil)

(defmethod react ((monster vandalbot) (event e-c-hit)) nil)

;;SECTION 3d: Normal User


(defclass user (monster)
  ((name :initform "user")
   (max-hp :initform 5)
   (speed :initform 1)
   (state :initform :normal)
   (karma :initform 10)))
   ;;(state :initform :angry)
   ;;(target :initform *player*)));;:clueless)))

(defmethod update-instance-for-different-class ((prev noob) (new user) &key)
  (setf (max-hp new) 5 (hp new) 5 (speed new) 1
        (karma new) (if (> (karma prev) 0) (karma prev) 0)
        (name new) "user"
        (state new) (if (eql (state prev) :angry) :angry :normal)))

(defmethod print-char ((monster user)) (values #\u :CWHITE))

(defmethod description ((monster user)) "A normal user.")

(defmethod ai ((monster user) &aux temp)
  (case (state monster)
    (:normal
     (if (items (aref *map* (y monster) (X monster)))
         (when (setf temp (find-vandalized-article (y monster) (x monster)))
           (return-from ai (make-instance 'rollback :article temp)))
         (progn
           (case (random 300)
             ((0 1 3) (return-from ai (make-instance 'spawn-good)))
             (2 (return-from ai (make-instance 'spawn-userbox))))))
     (let ((fnd (free-neighbor-deltas (y monster) (x monster))))
       (if fnd
           (let ((d (random-list fnd)))
             (make-instance 'move-to :dx (cdr d) :dy (car d)))
           (make-instance 'wait))))
    (:angry
     (when (near monster (target monster))
       ;;(format t "ATTACK!~%")
       (return-from ai (make-instance 'attack :monster (target monster))))
     (when (or (<= (hp (target monster)) 0) (zerop (random 30)))
       ;;Cooling off
       (setf (state monster) :normal) (setf (target monster) nil)
       (cast (make-instance 'e-m-calm :monster monster))
       (return-from ai (ai monster)))
     (destructuring-bind (yy . xx) (follow monster (target monster))
       (return-from ai (make-instance 'move-to
                                      :dx (- xx (x monster))
                                      :dy (- yy (y monster))))))))

(defmethod react ((monster user) (event e-delete-good))
  (anger monster (monster event)))

(defmethod react ((monster user) (event e-delete-userbox))
  (when (zerop (random 2))
    (anger monster (monster event))))

(defmethod react ((monster user) (event e-c-miss))
  (when (or (typep (defender event) 'noob) (typep (defender event) 'user))
    (anger monster (attacker event))))

(defmethod react ((monster user) (event e-c-hit))
  (when (or (typep (defender event) 'noob) (typep (defender event) 'user))
    (anger monster (attacker event))))

(defmethod react ((monster user) (event e-step))
  (when (and (not (eql (monster event) monster))
             (< (karma (monster event)) 0)
             (< (random 20) (- (karma (monster event)))))
    (anger monster (monster event))))


;;SECTION 3e: Admin

(defclass admin (monster)
  ((name :initform "admin")
   (max-hp :initform 10)
   (speed :initform 1)
   (state :initform :normal)
   (karma :initform 10)
   (inventory :initform (list (make-instance 'banhammer)))
   (pro-userbox :initform (if (zerop (random 8)) t nil) :accessor pro-userbox)))

(defmethod initialize-instance :after ((monster admin) &key)
  (setf (weapon monster) (car (inventory monster))))

(defmethod update-instance-for-different-class ((prev user) (new admin) &key)
  (setf (max-hp new) 10 (hp new) 10 (speed new) 1
        (karma new) (if (> (karma prev) 0) (karma prev) 0)
        (name new) "admin"
        (pro-userbox new) (if (zerop (random 8)) t nil))
  (push (make-instance 'banhammer) (inventory new))
  (setf (weapon new) (car (inventory new))))

(defmethod print-char ((monster admin)) (values #\A :CWHITE))

(defmethod description ((monster admin)) "An admin.")

(defmethod ai ((monster admin) &aux temp)
  (case (state monster)
    (:normal
     (if (items (aref *map* (y monster) (X monster)))
         (progn
           (when (setf temp (find-vandalized-article (y monster) (x monster)))
             (return-from ai (make-instance 'rollback :article temp)))
           (when (setf temp (find-junk-article (y monster) (x monster)))
             (return-from ai (make-instance 'delete-item :item temp)))
           (when (and (not (pro-userbox monster))
                      (setf temp (find-userbox (y monster) (x monster))))
             (when (zerop (random 2))
               (return-from ai (make-instance 'delete-item :item temp)))))
         (progn
           (case (random 200)
             (0 (return-from ai (make-instance 'spawn-good))))))
     (when (setf temp (find-nearby-monster monster 'noob))
       (when (zerop (random 4))
         (return-from ai (make-instance 'welcome :monster temp))))
     (let ((fnd (free-neighbor-deltas (y monster) (x monster))))
       (if fnd
           (let ((d (random-list fnd)))
             (make-instance 'move-to :dx (cdr d) :dy (car d)))
           (make-instance 'wait))))
    (:angry
     (when (near monster (target monster))
       ;;(format t "ATTACK!~%")
       (return-from ai (make-instance 'attack :monster (target monster))))
     (when (or (<= (hp (target monster)) 0) (zerop (random 50)))
       ;;Cooling off
       (setf (state monster) :normal) (setf (target monster) nil)
       (cast (make-instance 'e-m-calm :monster monster))
       (return-from ai (ai monster)))
     (destructuring-bind (yy . xx) (follow monster (target monster))
       (return-from ai (make-instance 'move-to
                                      :dx (- xx (x monster))
                                      :dy (- yy (y monster))))))))

(defmethod react ((monster admin) (event e-delete-good))
  (anger monster (monster event)))

(defmethod react ((monster admin) (event e-delete-userbox))
  (when (and (pro-userbox monster) (zerop (random 3)))
    (anger monster (monster event))))

(defmethod react ((monster admin) (event e-c-miss))
  (unless (or (typep (defender event) 'vandal) (typep (defender event) 'troll))
    (cond
      ((eql monster (defender event)) (anger monster (attacker event)))
      ((eql monster (attacker event)) (anger monster (defender event)))
      (t (if (<= (karma (attacker event)) (karma (defender event)))
                 (anger monster (attacker event)))))))


(defmethod react ((monster admin) (event e-c-hit))
  (unless (or (typep (defender event) 'vandal) (typep (defender event) 'troll))
    (cond
      ((eql monster (defender event)) (anger monster (attacker event)))
      ((eql monster (attacker event)) (anger monster (defender event)))
      (t (if (<= (karma (attacker event)) (karma (defender event)))
                 (anger monster (attacker event)))))))

(defmethod react ((monster admin) (event e-step))
  (when (and (not (eql (monster event) monster))
             (< (karma (monster event)) 0)
             (< (random 20) (- (karma (monster event)))))
    (anger monster (monster event))))


(defclass rouge-admin (monster)
  ((name :initform "rouge admin")
   (max-hp :initform 10)
   (speed :initform 1)
   (state :initform :rouge)
   (inventory :initform (list (make-instance 'banhammer)))))

(defmethod initialize-instance :after ((monster rouge-admin) &key)
  (setf (weapon monster) (car (inventory monster))))

(defmethod update-instance-for-different-class ((prev admin) (new rouge-admin) &key)
  (setf (name new) "rouge admin"))

(defmethod print-char ((monster rouge-admin)) (values #\A :CPINK))

(defmethod description ((monster rouge-admin)) "A rouge admin.")





;;SECTION 3f: Bureaucrat


(defclass bureaucrat (monster)
  ((name :initform "bureaucrat")
   (max-hp :initform 30)
   (speed :initform 1)
   (state :initform :normal)
   (karma :initform 50)
   (inventory :initform (list (make-instance 'banhammer)))))

(defmethod initialize-instance :after ((monster bureaucrat) &key)
  (setf (weapon monster) (car (inventory monster))))

(defmethod print-char ((monster bureaucrat)) (values #\B :CWHITE))

(defmethod description ((monster bureaucrat)) "A bureaucrat.")

(defmethod ai ((monster bureaucrat) &aux temp)
  (case (state monster)
    (:normal
     (if (items (aref *map* (y monster) (X monster)))
         (progn
           (when (setf temp (find-vandalized-article (y monster) (x monster)))
             (return-from ai (make-instance 'rollback :article temp)))
           (when (setf temp (find-junk-article (y monster) (x monster)))
             (return-from ai (make-instance 'delete-item :item temp))))
         )
     (when (setf temp (find-nearby-monster monster 'user))
       (when (and (> (karma temp) 50) (zerop (random 4)))
         (return-from ai (make-instance 'promote :monster temp))))
     (let ((fnd (free-neighbor-deltas (y monster) (x monster))))
       (if fnd
           (let ((d (random-list fnd)))
             (make-instance 'move-to :dx (cdr d) :dy (car d)))
           (make-instance 'wait))))
    (:angry
     (when (near monster (target monster))
       ;;(format t "ATTACK!~%")
       (return-from ai (make-instance 'attack :monster (target monster))))
     (when (or (<= (hp (target monster)) 0) (zerop (random 20)))
       ;;Cooling off
       (setf (state monster) :normal)
       (cast (make-instance 'e-m-calm :monster monster))
       (return-from ai (ai monster)))
     (destructuring-bind (yy . xx) (follow monster (target monster))
       (return-from ai (make-instance 'move-to
                                      :dx (- xx (x monster))
                                      :dy (- yy (y monster))))))))

;(defmethod react ((monster bureaucrat) (event e-delete-good))
;  (anger monster (monster event)))

(defmethod react ((monster bureaucrat) (event e-c-miss))
  (when (typep (defender event) 'bureaucrat)
    (cond
      ((eql monster (defender event)) (anger monster (attacker event)))
      ((eql monster (attacker event)) (anger monster (defender event)))
      (t (if (<= (karma (attacker event)) (karma (defender event)))
                 (anger monster (attacker event)))))))

(defmethod react ((monster bureaucrat) (event e-c-hit))
  (when (typep (defender event) 'bureaucrat)
    (cond
      ((eql monster (defender event)) (anger monster (attacker event)))
      ((eql monster (attacker event)) (anger monster (defender event)))
      (t (if (<= (karma (attacker event)) (karma (defender event)))
                 (anger monster (attacker event)))))))


;;SECTION 3g: Jimbo

(defclass jimbo (monster)
  ((name :initform "Jimbo")
   (max-hp :initform 100)
   (speed :initform 1)
   (state :initform :normal)
   (karma :initform 10000)
   (inventory :initform (list (make-instance 'jimbo-banhammer)))))

(defmethod initialize-instance :after ((monster jimbo) &key)
  (setf (weapon monster) (car (inventory monster))))

(defmethod print-char ((monster jimbo)) (values #\J :CYELLOW))

(defmethod description ((monster jimbo)) "Jimbo Wales, the Godking")

(defmethod ai ((monster jimbo) &aux temp)
  (case (state monster)
    (:normal
     (if (items (aref *map* (y monster) (X monster)))
         (progn
           (when (setf temp (find-good-article (y monster) (x monster)))
             (when (zerop (random 10)) ;;[[WP:OFFICE]] :)
               (return-from ai (make-instance 'delete-item :item temp))))
           )
         )
     (let ((fnd (free-neighbor-deltas (y monster) (x monster))))
       (if fnd
           (let ((d (random-list fnd)))
             (make-instance 'move-to :dx (cdr d) :dy (car d)))
           (make-instance 'wait))))
    (:angry
     (when (near monster (target monster))
       ;;(format t "ATTACK!~%")
       (return-from ai (make-instance 'attack :monster (target monster))))
     (when (or (<= (hp (target monster)) 0) (zerop (random 50)))
       ;;Cooling off
       (setf (state monster) :normal) (setf (target monster) nil)
       (cast (make-instance 'e-m-calm :monster monster))
       (return-from ai (ai monster)))
     (destructuring-bind (yy . xx) (follow monster (target monster))
       (return-from ai (make-instance 'move-to
                                      :dx (- xx (x monster))
                                      :dy (- yy (y monster))))))))

(defmethod react ((monster jimbo) (event e-c-miss))
  (when (typep (defender event) 'jimbo)
    (cond
      ((eql monster (defender event)) (anger monster (attacker event)))
      ((eql monster (attacker event)) (anger monster (defender event)))
      (t (if (<= (karma (attacker event)) (karma (defender event)))
                 (anger monster (attacker event)))))))

(defmethod react ((monster jimbo) (event e-c-hit))
  (when (typep (defender event) 'jimbo)
    (cond
      ((eql monster (defender event)) (anger monster (attacker event)))
      ((eql monster (attacker event)) (anger monster (defender event)))
      (t (if (<= (karma (attacker event)) (karma (defender event)))
                 (anger monster (attacker event)))))))

(defmethod kill ((monster jimbo))
  (setf *quitflag* :jimbo)
  (display-simple-message "OMG! Jimbo has closed Wikipedia!")
  (call-next-method))

;;SECTION 3h: Troll

(defclass troll (monster)
  ((name :initform "troll")
   (max-hp :initform (diffmod 4))
   (speed :initform 1)
   (state :initform :trollish)
   (karma :initform 5)))

(defmethod print-char ((monster troll)) (values #\t :CRED))

(defmethod description ((monster troll)) "A rampaging troll.")

(defmethod ai ((monster troll))
  (case (state monster)
    (:trollish
     (unless (target monster) (return-from ai
       (let ((fnd (free-neighbor-deltas (y monster) (x monster))))
         (if fnd
             (let ((d (random-list fnd)))
               (make-instance 'move-to :dx (cdr d) :dy (car d)))
             (make-instance 'wait)))))
     (when (near monster (target monster))
       ;;(format t "ATTACK!~%")
       (return-from ai (make-instance 'attack :monster (target monster))))
     (when (or (<= (hp (target monster)) 0) (zerop (random 10)))
       ;;Cooling off
       (setf (target monster) nil)
       (return-from ai (ai monster)))
     (destructuring-bind (yy . xx) (follow monster (target monster))
       (return-from ai (make-instance 'move-to
                                      :dx (- xx (x monster))
                                      :dy (- yy (y monster))))))
    (:angry
     (when (near monster (target monster))
       ;;(format t "ATTACK!~%")
       (return-from ai (make-instance 'attack :monster (target monster))))
     (when (or (<= (hp (target monster)) 0) (zerop (random 30)))
       ;;Cooling off
       (setf (target monster) nil) (setf (state monster) :trollish)
       (cast (make-instance 'e-m-calm :monster monster))
       (return-from ai (ai monster)))
     (destructuring-bind (yy . xx) (follow monster (target monster))
       (return-from ai (make-instance 'move-to
                                      :dx (- xx (x monster))
                                      :dy (- yy (y monster))))))))

(defmethod react ((monster troll) (event e-c-miss))
  (when (or (typep (defender event) 'troll) (typep (defender event) 'vandal))
    (anger monster (attacker event))))

(defmethod react ((monster troll) (event e-c-hit))
  (when (or (typep (defender event) 'troll) (typep (defender event) 'user))
    (anger monster (attacker event))))

(defmethod react ((monster troll) (event e-step))
  (unless (or (target monster) (typep (monster event) 'troll) (typep (monster event) 'vandal))
    (setf (target monster) (monster event))))


;;SECTION 4: Items

(defclass item ()
  ((name :initarg :name :accessor name)
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (parent :initarg :parent :initform nil :accessor parent)))

(defmethod x ((item item))
  (with-slots (x parent) item
    (if parent (x parent) x)))

(defmethod y ((item item))
  (with-slots (y parent) item
    (if parent (y parent) y)))

(defmethod print-char ((item item)) (values #\~ :CROSE))

(defclass userbox (item) ((name :initform (userbox-name))
                          (color :initarg :color
                                 :initform (random-list *colors*)
                                 :accessor color)))

(defun userbox-name ()
  (format nil "Userbox: \"This user ~a ~a\""
          (random-list '("likes" "hates"))
          (random-list '("bananas" "apples" "oranges" "everything"
                         "Jimbo" "Wikipedia" "admins" "people"
                         "userboxes"))))

(defmethod print-char ((item userbox)) (values #\* (color item)))

(defclass junk-article (item)
  ((name :initform "junk article")))

(defmethod print-char ((item junk-article)) (values #\? :CPINK))

(defclass good-article (item)
  ((name :initform "good article")
   (quality :initform t :accessor quality)))

(defmethod print-char ((item good-article))
  (values #\? (if (quality item) :CGREEN :CROSE)))

(defun find-article-or-userbox (y x)
  (let ((items (items (aref *map* y x))))
    (loop for x in items
          when (or (typep x 'good-article)
                   (typep x 'junk-article)
                   (typep x 'userbox)) do (return x)
          finally (return nil))))

(defun find-userbox (y x)
  (let ((items (items (aref *map* y x))))
    (loop for x in items
          when (typep x 'userbox) do (return x)
          finally (return nil))))

(defun find-good-article (y x)
  (let ((items (items (aref *map* y x))))
    (loop for x in items
          when (and (typep x 'good-article) (quality x)) do (return x)
          finally (return nil))))

(defun find-junk-article (y x)
  (let ((items (items (aref *map* y x))))
    (loop for x in items
          when (typep x 'junk-article) do (return x)
          finally (return nil))))

(defun find-vandalized-article (y x)
  (let ((items (items (aref *map* y x))))
    (loop for x in items
          when (and (typep x 'good-article) (not (quality x))) do (return x)
          finally (return nil))))

(defclass weapon (item) ())

(defclass banhammer (weapon)
  ((name :initform "banhammer")))

(defmethod damage ((weapon banhammer)) (1+ (random 5)))

(defmethod to-hit ((weapon banhammer)) 4)

(defmethod attack-word ((weapon banhammer) form)
  (if (eql form :he) "attempts to block"
      "attempt to block"))

(defclass jimbo-banhammer (banhammer)
  ((name :initform "Jimbo's banhammer")))

(defmethod damage ((weapon jimbo-banhammer)) (+ 5 (random 5)))

;;SECTION 5: Actions

(defclass action ()
  ((time :initform 5 :initarg :time :accessor time)))

(defclass move-to (action)
  ((dx :initarg :dx :accessor dx)
   (dy :initarg :dy :accessor dy)))

(defmethod initialize-instance :after ((self move-to) &key)
  (setf (time self) (if (or (> (abs (dy self)) 1) (> (abs (dx self)) 1)) 0
                        (if (= 2 (+ (abs (dx self)) (abs (dy self)))) 4 3))))

(defmethod do-action (monster (move-to move-to))
  (with-slots (y x) monster
    (with-slots (dy dx) move-to
      (unless (occupied (+ y dy) (+ x dx))
        (setf (monster (aref *map* y x)) nil)
        (out-char (aref *map* y x))
        (incf y dy) (incf x dx)
        (setf (monster (aref *map* y x)) monster)
        (out-char (aref *map* y x))
        (refresh)
        (cast (make-instance 'e-step :monster monster :y y :x x))
        t))))

(defmethod do-action ((monster player) (move-to move-to))
  (with-slots (y x) monster
    (with-slots (dy dx) move-to
      (when (or (> (abs dy) 1) (> (abs dx) 1)) (return-from do-action nil))
      (let ((monster? (monster (aref *map* (+ y dy) (+ x dx)))))
        (if monster?
            (if (or (eql (state monster?) :angry)
                    (eql (state monster?) :evil)
                    (eql (state monster?) :trollish)
                    (yes-or-no "Do you really want to attack?"))
                (do-action monster
                  (make-instance 'attack :monster monster?))
                nil)
            (call-next-method))))))

(defclass wait (action) ((time :initform 5)))

(defmethod do-action (monster (action wait))
  (declare (ignore monster action))
  nil)

(defclass quit-game (action) ((time :initform 0)))

(defmethod do-action ((monster player) (quit quit-game))
  (setf *zero-time-action* t)
  (setf *quitflag* :quit))

(defclass no-action (action)
  ((key :initarg :key :initform 0 :accessor key)
   (time :initform 0)))

(defmethod do-action ((monster player) (action no-action))
  (setf *zero-time-action* nil)
  (message "No action bound to key: ~a" (key action)))

(defclass next-message (action) ((time :initform 0)))

(defmethod do-action ((monster player) (action next-message))
  (setf *zero-time-action* t)
  ;(format t "~a~%" *messages*)
  (display-message (pop *messages*)))

(defclass recap (action) ((time :initform 0)))

(defmethod do-action ((monster player) (action recap))
  (recap-messages)
  (getch)
  (redraw-screen))

(defclass look-around (action) ((time :initform 0)))

(defmethod do-action ((monster player) (action look-around))
  (let ((xx (x monster)) (yy (y monster)))
    (move (+ yy *dy*) (+ xx *dx*)) (refresh)
    (loop (case (getch)
            ((261 54 454) (incf xx))
            ((260 52 452) (decf xx))
            ((259 56 450) (decf yy))
            ((258 50 456) (incf yy))
            ((51 457) (incf xx) (incf yy))
            ((49 455) (decf xx) (incf yy))
            ((57 451) (incf xx) (decf yy))
            ((55 449) (decf xx) (decf yy))
            ((27 113 169 108 164 13 32) (return)))
       (when (< xx 0) (setf xx 0))
       (when (< yy 0) (setf yy 0))
       (when (> xx 75) (setf xx 75))
       (when (> yy 20) (setf yy 20))
       (display-simple-message (description (aref *map* yy xx)))
       (show-items yy xx)
       (move (+ yy *dy*) (+ xx *dx*)) (refresh))
    (show-items yy xx)
    (move (+ yy *dy*) (+ xx *dx*)) (refresh)))

(defclass spawn-stuff (action)
  ((stuff :initarg :stuff :accessor stuff)))

(defclass spawn-userbox (spawn-stuff)
  ((time :initform 5)
   (stuff :initform 'userbox)))

(defclass spawn-junk (spawn-stuff)
  ((time :initform 5)
   (stuff :initform 'junk-article)))

(defclass spawn-good (spawn-stuff)
  ((time :initform 10)
   (stuff :initform 'good-article)))

(defmethod do-action (monster (action spawn-stuff))
  (with-slots (y x) monster
    (push (make-instance (stuff action) :x x :y y)
          (items (aref *map* y x)))))

(defmethod do-action :after (monster (action spawn-userbox))
  (cast (make-instance 'e-spawn-userbox :monster monster)))

(defmethod do-action :after (monster (action spawn-junk))
  (cast (make-instance 'e-spawn-junk :monster monster))
  (decf (karma monster) 1)
  (incf (rouge monster) 1))

(defmethod do-action :after (monster (action spawn-good))
  (cast (make-instance 'e-spawn-good :monster monster))
  (incf (karma monster) 10))

(defclass spawn-monster (action)
  ((monster :initarg :monster :accessor monster)
   (position :initarg :position :accessor position)))

(defclass spawn-sockpuppet (spawn-monster)
  ((monster :initform 'sockpuppet)
   (time :initform 5)))

(defmethod do-action (monster (action spawn-monster))
  (declare (ignore monster))
  (spawn (make-instance (monster action)) :position (position action)))

(defclass vandalize (action)
  ((time :initform 4)
   (article :initarg :article :accessor article)
   (severe :initarg :severe :initform nil :accessor severe)))

(defmethod do-action (monster (action vandalize))
  (declare (ignore monster))
  (with-slots (article) action
    (setf (quality article) nil
          (name article) "vandalized article")))

(defmethod do-action :after (monster (action vandalize))
  (cast (make-instance 'e-vandalize :monster monster))
  (decf (karma monster) 5)
  (incf (rouge monster) 2))

(defclass rollback (action)
  ((time :initform 5)
   (article :initarg :article :accessor article)))

(defmethod do-action (monster (action rollback))
  (declare (ignore monster))
  (with-slots (article) action
    (setf (quality article) t
          (name article) "good article")))

(defmethod do-action :after (monster (action rollback))
  (unless (typep action 'rollback-on-spot)
    (cast (make-instance 'e-rollback :monster monster))
    (incf (karma monster) 5)))

(defclass rollback-on-spot (rollback)
  ((item :initform nil :accessor item)))

(defmethod do-action ((monster player) (action rollback-on-spot))
  (setf (item action) (find-vandalized-article (y *player*) (x *player*)))
  (if (item action) (do-action monster
                      (make-instance 'rollback :article (item action)))
      (message "Rollback is not applicable.")))

(defclass delete-item (action)
  ((time :initform 6)
   (item :initarg :item :accessor item)))

(defmethod do-action (monster (action delete-item))
  (declare (ignore monster))
  (with-slots (item) action
    (let ((tile (aref *map* (y item) (x item))))
      (setf (items tile) (delete item (items tile))))))

(defmethod do-action :after (monster (action delete-item))
  (unless (typep action 'delete-item-on-spot)
  (typecase (item action)
    (good-article
     (if (typep monster 'jimbo)
         (cast (make-instance 'e-office
                              :monster monster
                              :item (item action)))
         (progn (cast (make-instance 'e-delete-good
                                     :monster monster
                                     :item (item action)))
                (decf (karma monster) 15) (incf (rouge monster) 10))))
    (junk-article (cast (make-instance 'e-delete-junk
                                       :monster monster
                                       :item (item action)))
                  (incf (karma monster) 5))
    (userbox (cast (make-instance 'e-delete-userbox
                                  :monster monster
                                  :item (item action)))
                  (decf (karma monster) 0) (incf (rouge monster) 5)))))


(defclass delete-item-on-spot (delete-item)
  ((item :initform nil :accessor item)))

(defmethod do-action ((monster player) (action delete-item-on-spot))
  (setf (item action) (find-article-or-userbox (y *player*) (x *player*)))
  (if (item action) (do-action monster
                      (make-instance 'delete-item :item (item action)))
      (message "Nothing to delete here.")))


(defclass welcome (action)
  ((monster :initarg :monster :accessor monster)
   (time :initform 5)))

(defmethod do-action :before (monster (action welcome))
  (cast (make-instance 'e-welcome
                       :monster1 monster
                       :monster2 (monster action))))

(defmethod do-action (monster (action welcome))
  (declare (ignore monster))
  (when (typep (monster action) 'noob)
    (change-class (monster action) 'user)
    (out-char (monster action))))

(defmethod do-action :after (monster (action welcome))
  (incf (karma monster) 5)
  (decf (rouge monster) 8))

(defclass welcome-dir (action)
  ((time :initform 5)))

(defmethod do-action ((monster player) (action welcome-dir))
  (let ((dir (choose-direction)))
    (if dir
        (let ((mon (monster (aref *map*
                                  (+ (y monster) (car dir))
                                  (+ (x monster) (cdr dir))))))
          (if (typep mon 'noob)
              (do-action monster (make-instance 'welcome :monster mon))
              (message "No suitable noob here.")))
        (message "You decide not to welcome anyone."))))



(defclass promote (action)
  ((monster :initarg :monster :accessor monster)
   (time :initform 10)))

(defmethod do-action :before (monster (action promote))
  (cast (make-instance 'e-promote
                       :monster1 monster
                       :monster2 (monster action))))

(defmethod do-action (monster (action promote))
  (declare (ignore monster))
  (when (typep (monster action) 'user)
    (change-class (monster action) 'admin)
    (out-char (monster action))))

;;SECTION 5a: Combat

(defclass attack (action)
  ((monster :initarg :monster :accessor monster)
   (time :initform 6)))

(defgeneric damage (x)
  (:documentation "Returns a damage that a monster or a weapon deals"))

(defmethod damage ((monster monster))
  (if (weapon monster) (damage (weapon monster))
      (1+ (random (strength monster)))))

(defgeneric to-hit (x)
  (:documentation "Returns a to-hit value of a monster or a weapon"))

(defmethod to-hit ((monster monster))
  (+ (if (weapon monster) (to-hit (weapon monster)) 0)
     (dexterity monster)))

(defgeneric armor-dv (x)
  (:documentation "Returns a DV value of a monster or an armor item"))

(defgeneric armor-pv (x)
  (:documentation "Returns a PV value of a monster or an armor item"))

(defmethod armor-dv ((monster monster))
  (+ (if (shirt monster) (armor-dv (shirt monster)) 0)
     (if (boots monster) (armor-dv (boots monster)) 0)
     (if (hat monster) (armor-dv (hat monster)) 0)
     (dexterity monster)))

(defmethod armor-pv ((monster monster))
  (+ (if (shirt monster) (armor-pv (shirt monster)) 0)
     (if (boots monster) (armor-pv (boots monster)) 0)
     (if (hat monster) (armor-pv (hat monster)) 0)
     (dexterity monster)))

(defun hit-p (mon1 mon2)
  (when (>= (random (to-hit mon1)) (random (armor-dv mon2))) t))

(defun hit (mon1 mon2)
  (let ((dam (damage mon1))
        (prev (random (armor-pv mon2))))
    (if (> dam prev)
        (progn (decf (hp mon2) (- dam prev)) (- dam prev))
        0)))

(defmethod do-action :before (monster (attack attack))
  (let ((banhammer-mod (if (typep (weapon monster) 'banhammer) 2 1))
        (monster-mod (typecase (monster attack)
                       (player 0)
                       (noob 1)
                       (vandal -1)
                       (troll -1)
                       (user 2)
                       (admin 4)
                       (bureaucrat 8)
                       (jimbo 16)
                       ))
        (defending-mod (if (eql (state monster) :angry) 1 4)))
    (decf (karma monster) (* banhammer-mod monster-mod defending-mod))
    (incf (rouge monster) (* banhammer-mod (abs monster-mod) defending-mod))))

(defmethod do-action (monster (attack attack))
  (with-slots ((monster2 monster)) attack
    ;;(format t "~a attacks ~a. ~%" monster monster2)
    (if (hit-p monster monster2)
        (let ((dam (hit monster monster2)))
          (if (> dam 0) (cast (make-instance 'e-c-hit
                                             :attacker monster
                                             :defender monster2
                                             :damage dam))
              (cast (make-instance 'e-c-miss
                                   :attacker monster
                                   :defender monster2)))
          (when (<= (hp monster2) 0) (kill monster2)))
        (cast (make-instance 'e-c-miss
                             :attacker monster
                             :defender monster2)))))


(defclass switch-weapon (action)
  ((time :initform 3)))

(defun find-weapon (monster)
  (loop for x in (inventory monster)
       when (typep x 'weapon) do (return x)
       finally (return nil)))

(defmethod do-action ((monster player) (action switch-weapon))
  (if (weapon monster)
      (progn (setf (weapon monster) nil)
             (message "You are now using word as a primary weapon."))
      (progn (setf (weapon monster) (find-weapon monster))
             (if (weapon monster)
                 (message "You are now using ~a as a primary weapon."
                      (name (weapon monster)))
                 (message "You have no weapons.")))))


;;SECTION 6: "LOS" and Events

(defun distance (y1 x1 y2 x2)
  (let ((dy (abs (- y1 y2)))
        (dx (abs (- x1 x2))))
    (/ (+ (* 4 (min dx dy)) (* 3 (abs (- dx dy)))) 3)))

(defun aware-p (monster y x)
  "Whether the monster is aware of the point (x,y)"
  (<= (distance (y monster) (x monster) y x) (awareness monster)))

(defclass event ()
  ((interesting :initform t :initarg :interesting :accessor interesting)
   (x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defgeneric cast (event)
  (:documentation "Make all monsters that feel the event react to it."))

(defmethod cast :before (event)
  (declare (ignorable event))
  ;(format t "Event ~A casted.~%" event)
  (push (cons *curmonster* 0) *monsterstack*))

(defmethod cast (event)
  (loop for m in (remove-duplicates *monsterstack* :key #'car)
        for mon = (car m)
        when (aware-p mon (y event) (x event))
        do (react mon event)))

(defmethod cast :after (event)
  (declare (ignore event))
  (pop *monsterstack*))

(defclass e-combat (event)
  ((attacker :initarg :attacker :accessor attacker)
   (defender :initarg :defender :accessor defender)))

(defmethod cast ((event e-combat))
  (with-slots (attacker defender) event
    (loop for m in (remove-duplicates *monsterstack* :key #'car)
       for mon = (car m)
       when (or (aware-p mon (y attacker) (x attacker))
                (aware-p mon (y defender) (x defender)))
       do (react mon event))))

(defgeneric combat-situation (attacker defender result)
  (:documentation "Describe a combat situation"))


(defgeneric attack-word (weapon form)
  (:documentation "Returns a verb that would be used in a combat message")
  (:method (weapon form) (declare (ignore weapon))
           (if (eql form :he) "hits" "hit")))

(defmethod attack-word ((weapon (eql nil)) form)
  (if (eql form :he) "makes a personal attack on"
      "make a personal attack on"))

(defmethod combat-situation (att def result)
  (format nil "~@(~a~) ~a ~a ~a."
                 (name att) (attack-word (weapon att) :he)
                 (name def)
                 (if (eql result :hit) "and succeeds"
                     (format nil "but ~a is not impressed" (name def)))))

(defmethod combat-situation ((att player) def result)
  (format nil "You ~a ~a ~a."
                 (attack-word (weapon att) :you)
                 (name def)
                 (if (eql result :hit) "and succeed"
                     (format nil "but ~a is not impressed" (name def)))))

(defmethod combat-situation (att (def player) result)
  (format nil "~@(~a~) ~a you ~a."
                 (name att) (attack-word (weapon att) :he)
                 (if (eql result :hit) "and succeeds"
                     (format nil "but you are not impressed" (name def)))))

(defclass e-c-hit (e-combat)
  ((damage :initarg :damage :accessor damage)))

(defmethod description ((event e-c-hit))
  (with-slots ((att attacker) (def defender)) event
    (combat-situation att def :hit)))

(defclass e-c-miss (e-combat) ())

(defmethod description ((event e-c-miss))
  (with-slots ((att attacker) (def defender)) event
    (combat-situation att def :miss)))

(defclass e-monster (event)
  ((monster :initarg :monster :accessor monster)))

(defmethod cast ((event e-monster))
  (loop for m in (remove-duplicates *monsterstack* :key #'car)
        for mon = (car m)
        when (aware-p mon (y (monster event)) (x (monster event)))
        do (react mon event)))

(defclass e-dead (e-monster) ())

(defmethod description ((event e-dead))
  (if (eql (monster event) *player*)
      (display-simple-message "That's it. You're OUT!")
      (let ((dead-message
             (list "~@(~a~) takes an indefinite wikibreak."
                   "~@(~a~) will be missed."
                   "Not even Esperanza could save ~a."
                   "~@(~a~) joins the company of Lir and MSK on Wikipedia Review."
                   "~@(~a~) has left the building."
                   "~@(~a~) leaves and vows never to return again.")))
        (format nil (nth (random (length dead-message)) dead-message)
                (name (monster event))))))

(defclass e-step (event)
  ((interesting :initform nil)
   (monster :initarg :monster :accessor monster)))

(defclass e-item (event)
  ((monster :initarg :monster :accessor monster)
   (item :initarg :item :initform nil :accessor item)))

(defmethod cast ((event e-item))
  (loop for m in (remove-duplicates *monsterstack* :key #'car)
        for mon = (car m)
        when (aware-p mon (y (monster event)) (x (monster event)))
        do (react mon event)))

(defclass e-spawn-userbox (e-item) ())

(defmethod description ((event e-spawn-userbox))
  (format nil "~a create~a a pesky userbox."
          (if (eql (monster event) *player*)
              "You" (name (monster event)))
          (if (eql (monster event) *player*)
              "" "s")))

(defclass e-spawn-junk (e-item) ())

(defmethod description ((event e-spawn-junk))
  (format nil "~a create~a ~a."
          (if (eql (monster event) *player*)
              "You" (name (monster event)))
          (if (eql (monster event) *player*)
              "" "s")
          (case (random 4)
            (0 "an article about himself")
            (1 "an article about some unknown band")
            (2 "an article advertising the new \"cool\" webforum")
            (3 "an article revealing someones sexual orientation in its title"))))

(defclass e-spawn-good (e-item) ())

(defmethod description ((event e-spawn-good))
  (format nil "~a create~a a new article."
          (if (eql (monster event) *player*)
              "You" (name (monster event)))
          (if (eql (monster event) *player*)
              "" "s")))

(defclass e-vandalize (e-item) ((severe :initarg :severe :initform nil
                                        :accessor severe)))

(defmethod description ((event e-vandalize))
  (if (severe event)
      (format nil "~a ~a."
              (if (eql (monster event) *player*)
                  "You" (name (monster event)))
              (format nil
                      (case (random 4)
                        (0 "insert~a a giant picture of a human penis into an article")
                        (1 "move~a an article to its \"ON WHEELS\" destination")
                        (2 "turn~a an article into a thorough guide to pelican shit")
                        (3 "blank~a an article, replacing it with hammer and sickle picture"))
                      (if (eql (monster event) *player*) "" "s")))
      (format nil "~a make~a a \"test\" edit to an article."
              (if (eql (monster event) *player*)
                  "You" (name (monster event)))
              (if (eql (monster event) *player*)
                  "" "s"))))

(defclass e-rollback (e-item) ())

(defmethod description ((event e-rollback))
  (format nil "~a revert~a an act of vandalism."
          (if (eql (monster event) *player*)
              "You" (name (monster event)))
          (if (eql (monster event) *player*)
              "" "s")))

(defclass e-delete-item (e-item) ())

(defmethod description ((event e-delete-item))
  (format nil "~a delete~a a ~a."
          (if (eql (monster event) *player*)
              "You" (name (monster event)))
          (if (eql (monster event) *player*)
              "" "s")
          (name (item event))))

(defclass e-delete-good (e-delete-item) ())

(defclass e-office (e-delete-item) ())

(defmethod description ((event e-office))
  (format nil "~a delete~a an article per WP:OFFICE!"
          (if (eql (monster event) *player*)
              "You" (name (monster event)))
          (if (eql (monster event) *player*)
              "" "s")))

(defclass e-delete-junk (e-delete-item) ())

(defclass e-delete-userbox (e-delete-item) ())

(defclass e-m-angry (e-monster) ())

(defmethod description ((event e-m-angry))
  (format nil "~@(~a~) gets angry." (name (monster event))))

(defclass e-m-calm (e-monster) () )

(defmethod description ((event e-m-calm))
  (format nil "~@(~a~) calms down." (name (monster event))))

(defclass e-interaction (event)
  ((monster1 :initarg :monster1 :accessor monster1)
   (monster2 :initarg :monster2 :accessor monster2)))

(defmethod cast ((event e-interaction))
  (with-slots (monster1 monster2) event
    (loop for m in (remove-duplicates *monsterstack* :key #'car)
       for mon = (car m)
       when (or (aware-p mon (y monster1) (x monster1))
                (aware-p mon (y monster2) (x monster2)))
       do (react mon event))))

(defclass e-welcome (e-interaction) ())

(defmethod description ((event e-welcome))
  (format nil "~a welcome~a ~a. ~@(~a~) is now a dedicated user."
          (if (eql (monster1 event) *player*)
              "You" (name (monster1 event)))
          (if (eql (monster1 event) *player*)
              "" "s")
          (name (monster2 event)) (name (monster2 event))))

(defclass e-promote (e-interaction) ())

(defmethod description ((event e-promote))
  (format nil "~a promote~a ~a. ~@(~a~) is now an admin."
          (if (eql (monster1 event) *player*)
              "You" (name (monster1 event)))
          (if (eql (monster1 event) *player*)
              "" "s")
          (name (monster2 event)) (name (monster2 event))))


;;SECTION 7: Stack processing

(defun count-vandals ()
  (if *monsterstack*
      (loop for (monster . nil) in *monsterstack*
         counting (typep monster 'vandal) into vandals
         counting (not (typep monster 'troll)) into monsters
         finally (return (/ vandals monsters)))
      0))

(defun occupied (y x)
  (let ((tile (aref *map* y x)))
    (not (and (passable tile) (not (monster tile))))))

(defun random-free-position ()
  (loop for x = (1+ (random 74))
        for y = (1+ (random 19))
        while (occupied y x)
        finally (return (cons y x))))


(defun add-to-stack (monster time)
  (let ((first (first *monsterstack*)))
    (when (or (not first) (> (cdr first) time) (zerop time))
      (push (cons monster time) *monsterstack*)
      (return-from add-to-stack))
    (setf *monsterstack* (nconc *monsterstack* (list (cons monster time))))
    (setq *monsterstack*
          (stable-sort *monsterstack* #'< :key #'cdr))))

(defun spawn (monster &key (position (random-free-position)))
  (add-to-stack monster 1)
  (let* ((x (cdr position)) (y (car position))
         (tile (aref *map* y x)))
    (setf (y monster) y (x monster) x
          (monster tile) monster))
  monster)

(defun increase-difficulty ()
  (incf *diffmod* 0.01))

(defun generate-monster ()
  (case (random 22)
    ((0 1 2 3 8 9 10 13 14 21) (spawn (make-instance 'noob)))
    ((4 5 15 16) (spawn (make-instance 'vandal)))
    ((6 17) (spawn (make-instance 'uber-vandal)))
    ((7 18) (spawn (make-instance 'vandalbot)))
    ((11 12 19 20) (spawn (make-instance 'troll))))
  (increase-difficulty))

(defun run-ai (monster)
  (let ((action (ai monster)))
    ;;(format t "~a doing action ~a~%" monster action)
    (do-action monster action)
    (when (and (eql monster *player*)
               (< (random 100) (time action)))
      (generate-monster))
    (when (and (< (random 200) (time action))
               (< (hp monster) (max-hp monster)))
      (incf (hp monster)))
    (add-to-stack monster (* (time action) (speed monster)))))

(defun run-stack ()
  (unless *monsterstack* (return-from run-stack :quit))
  (let ((min-time (cdar *monsterstack*)))
    (run-ai (setf *curmonster* (car (pop *monsterstack*))))
    (loop for x in *monsterstack* do (decf (cdr x) min-time))
    (return-from run-stack :ok)))

;;SECTION 8: Generating dungeon

(defun make-room ()
  (let ((width (+ (random 10) 4))
        (height (+ (random 5) 3)))
    (let ((start-x  (random (- 74 width)))
          (start-y  (random (- 19 height))))
      (loop for x from (+ start-x 1) to (+ start-x width)
           do (loop for y from (+ start-y 1) to (+ start-y height)
                 do (setf (aref *map* y x) (make-instance 'concrete-floor :x x :y y))))
      (push (cons (+ start-y 1 (random height)) (+ start-x 1 (random width))) *roompoints*))))

(defun tonnel (p1 p2)
  (when (= (car p1) (car p2))
      (loop for x from (min (cdr p1) (cdr p2)) to (max (cdr p1) (cdr p2))
         do (setf (aref *map* (car p1) x) (make-instance 'concrete-floor :x x :y (car p1))))
      (return-from tonnel t))
  (when (= (cdr p1) (cdr p2))
      (loop for y from (min (car p1) (car p2)) to (max (car p1) (car p2))
         do (setf (aref *map* y (cdr p1)) (make-instance 'concrete-floor :x (cdr p1) :y y)))
      (return-from tonnel t))
  (let ((p3 (nth (random 2) (list (cons (car p1) (cdr p2)) (cons (car p2) (cdr p1))))))
    (tonnel p1 p3) (tonnel p3 p2)))

(defun generate-map (&key stairs player &aux upstair downstair)
  (setf *map* (make-array '(21 76)))
  (setf *monsterstack* nil)
  (loop for x from 0 to 75
        do (loop for y from 0 to 20
                do (setf (aref *map* y x)
                         (make-instance 'wall :x x :y y))))
  (setf *roompoints* nil)
  (loop for x from 1 to (+ 10 (random 10))
     do (make-room))
;;Connecting...
  (let* ((n (length *roompoints*))
         (ar (make-array n :initial-element nil)))
    (flet ((finish-p () (loop for i from 0 to (1- n) always (aref ar i)))
           (connect (a b)
             (let ((p1 (nth a *roompoints*))
                   (p2 (nth b *roompoints*)))
               (tonnel p1 p2)
               (setf (aref ar a) t (aref ar b) t))))
      (loop for a = (random n)
            for b = (random n)
            if (/= a b) do (connect a b)
            until (finish-p))))
;;Staircases
  (let ((n (length *roompoints*)))
    (ecase stairs
      ((nil) nil)
      ((:u :d) (destructuring-bind (y . x) (nth (random n) *roompoints*)
                 (setf (aref *map* y x)
                       (make-instance 'staircase :direction stairs :y y :x x))
                 (if (eql stairs :u) (setf upstair (cons y x))
                     (setf downstair (cons y x)))))
      (:ud (loop for p1 = (nth (random n) *roompoints*)
                 for p2 = (nth (random n) *roompoints*)
                 while (equal p1 p2)
                 finally
                 (setf (aref *map* (car p1) (cdr p1))
                       (make-instance 'staircase
                                      :direction :u :y (car p1) :x (cdr p1)))
                 (setf upstair p1)
                 (setf (aref *map* (car p2) (cdr p2))
                       (make-instance 'staircase
                                      :direction :d :y (car p2) :x (cdr p2)))
                 (setf downstair p2)))))
;;Spawning player
  (setf *player*
        (ecase player
          ((nil) nil)
          (:u (spawn (make-instance 'player) :position upstair))
          (:d (spawn (make-instance 'player) :position downstair))
          (:random (spawn (make-instance 'player)))))
;;Spawning monsters
  (loop for x from 1 to (+ 8 (random 5))
       do (case (random 2)
            (0 (spawn (make-instance 'noob)))
            (1 (spawn (make-instance 'user)))))
  (loop for x from 1 to 5
       do (spawn (make-instance 'admin)))
  (loop for x from 1 to 2
       do (spawn (make-instance 'bureaucrat)))
  (spawn (make-instance 'jimbo))
)


;;SECTION 9: Printing routines

(defun out-char (obj)
  "Actually outputs the character of tile, monster or item"
  (multiple-value-bind (char color) (print-char obj)
    (attrset color)
    (mvaddch (+ *dy* (y obj)) (+ *dx* (x obj)) char)))

(defun print-map ()
  (loop for x from 0 to 75
        do (loop for y from 0 to 20
                do (out-char (aref *map* y x)))))

;;(mvaddstr 0 0 (make-string 79 :initial-element #\Space));
;;doesn't work due to unknown bug

(defun clean-line (line)
  (loop for i from 0 to 79 do (mvaddch line i #\Space)))

(defun clean-column (column)
  (loop for i from 1 to 23 do (mvaddch i column #\Space)))

(defun display-message (str)
  (if str
      (progn
        (attrset :cdark)
        (clean-line 0)
        (safe-print 0 1 str)
        (when *messages*
          (attrset :cyellow)
          (mvprintw 0 70 "[MORE]"))
        (refresh))
      (display-simple-message "No more messages.")))

(defun display-simple-message (str)
  (attrset :cdark)
  (clean-line 0)
  (safe-print 0 1 str)
  (refresh) "")

(defun message (str &rest args)
  (let ((s (apply #'format nil str args)))
    (push s *messages*) (push s *allmessages*)))

(defun show-items (y x)
  (clean-line 23)
  (let ((items (items (aref *map* y x)))
        (curpos 1))
    (loop for i in items
          for n = (name i)
          do (attrset (nth-value 1 (print-char i))) (mvprintw 23 curpos n)
           (incf curpos (+ (length n) 2)))))

(defun show-status ()
  (clean-line 22)
  (attrset :CDARK)
  (mvprintw 22 2 (format nil "HP:~d/~d  STR:~d  DEX:~d  AWA:~d"
                         (hp *player*) (max-hp *player*)
                         (strength *player*)
                         (dexterity *player*)
                         (awareness *player*)))
  (attrset :CGREEN)
  (mvprintw 22 50 (format nil "Karma:~d" (karma *player*)))
  (attrset :CPINK)
  (mvprintw 22 63 (format nil "Rouge:~d" (rouge *player*)))
;;Wdefcon
  (clean-column 1)
  (attrset :CSKY) (mvaddch 20 0 #\5)
  (attrset :CGREEN) (mvaddch 19 0 #\4)
  (attrset :CYELLOW) (mvaddch 18 0 #\3)
  (attrset :CROSE) (mvaddch 17 0 #\2)
  (attrset :CDARK) (mvaddch 16 0 #\1)
  (let* ((v (count-vandals))
         (level (cond
                  ((< v 1/10) 5)
                  ((< v 1/5) 4)
                  ((< v 1/3) 3)
                  ((< v 1/2) 2)
                  ((< v 3/4) 1)
                  (t (progn (message "Vandals have overrun Wikipedia!")
                            (setf *quitflag* :vandals)
                            0)))))
    (attrset :CGRAY) (mvaddch (+ 15 level) 1 #\])))


(defun redraw-screen ()
  (erase)
  (print-map)
  (show-status)
  (show-items (y *player*) (x *player*))
  (move (+ *dy* (y *player*)) (+ *dx* (x *player*)))
  (refresh))

(defun recap-messages ()
  (erase)
  (attrset :cgray)
  (loop for x = *allmessages* then (cdr x)
     for i from 1 to 20
     do (safe-print i 1 (if (car x) (car x) "-"))))

(defun safe-print (y x str)
  "Workaround for a bug"
  (loop for c across str
        for xx = x then (1+ xx)
        do (mvaddch y xx c)))

(defun yes-or-no (str)
  (display-simple-message (format nil "~a (y/n)" str))
  (case (getch)
    ((121 173 89 141) t)
    ((110 78 226 146) nil)))

(defun choose-direction ()
  (display-simple-message "Choose direction [12346789]")
  (case (getch)
    ((261 54 454) (cons 0 1))
    ((260 52 452) (cons 0 -1))
    ((259 56 450) (cons -1 0))
    ((258 50 456) (cons 1 0))
    ((51 457) (cons 1 1))
    ((49 455) (cons 1 -1))
    ((57 451) (cons -1 1))
    ((55 449) (cons -1 -1))
    (t nil)))

;;SECTION 10: Main sequence

(defun splash-screen ()
  (attrset :cpink)
  (mvprintw 1 1 "The Rougelike! 1.6")
  (attrset :cwhite)
  (mvprintw 2 1 "(c) 2006 Timofei Shatrov")
  (mvprintw 24 50 "[press any key to continue]")
  (refresh)
  (getch))

(defun genmd5 (object)
  (coerce (md5sum-sequence (let ((*print-readably* t)) (format nil "~s" object)))
          'list))

(defun load-hiscore ()
  (unless (probe-file "hiscore") (return-from load-hiscore nil))
  (with-open-file (in "hiscore"
                      :external-format #+clisp charset:utf-8 #+sbcl :utf-8)
    (let* ((*read-eval* nil)
           (hashentry (read in))
           (hiscore (read in)))
      (if (equal (genmd5 hiscore) hashentry) hiscore :error))))

(defun save-hiscore (hiscore)
  (with-open-file (out "hiscore" :direction :output :if-exists :supersede
                       :external-format #+clisp charset:utf-8 #+sbcl :utf-8)
    (let ((*print-readably* t))
      (format out "~s~%~s " (genmd5 hiscore) hiscore))))

(defun hiscore (&aux hiscore (number 0))
  (setf hiscore (load-hiscore))
  (when (eql hiscore :error)
    (erase)
    (attrset :cred)
    (mvprintw 1 1 "Your hiscore file is corrupted. Please delete it.")
    (attrset :cwhite)
    (mvprintw 24 50 "[press any key to quit]")
    (getch)
    (return-from hiscore))

  (setf hiscore (nreverse hiscore))
  (setf hiscore (stable-sort (nreverse (push (cons :player (rouge *player*))
                                             hiscore))
                             #'> :key #'cdr))
  (when (> (length hiscore) 10)
    (setf hiscore (nbutlast hiscore)))
  (let ((ask (loop for entry in hiscore
                   do (incf number)
                   when (eql (car entry) :player) do (loop-finish)
                   finally (if (eql (car entry) :player) (return entry) nil))))
             ;(find :player hiscore :key #'car)))
    (when ask
      (erase) (attrset :cyellow)
      (mvprintw 1 1 "Congratulations! You made the top ten!")
      (attrset :grey) (mvprintw 2 1 "Enter your name: ")
      (setf (car ask) (getnstr 12)))
    (unless ask (setf number 0)))

  (erase)
  (attrset :cpink)
  (mvprintw 1 1 "Top Rouge Admins:")
  (attrset :cwhite)
  (loop for i from 1 to 10
        for entry in hiscore
        when (= i number) do (attrset :cyellow)
        do (mvprintw (+ 2 i) 1 (format nil "~a. ~15a~a"
                                       i (car entry) (cdr entry)))
        when (= i number) do (attrset :cwhite))
  (save-hiscore hiscore)

  (mvprintw 24 50 "[press any key to quit]")
  (refresh)
  (getch))

(defun runtime ()
  (setf *quitflag* nil)
  (loop
     (run-stack)
     (when *quitflag*
       (unless (eql *quitflag* :quit) (getch))
       (hiscore)
       (return))))

(defun start-game (&key debug)
  (setf *random-state* (make-random-state t)
        *allmessages* nil
        *messages* nil)
  (if debug (connect-console)
   (create-console "textconsole.exe"))
  (erase)
  (splash-screen)
  (unwind-protect
       (progn
         (when (init-controls) (error "Cannot find control file. This sucks :("))
               (erase)
               (generate-map :stairs nil :player :random)
               (print-map)
               (refresh)
               (runtime))
    (close-console))
    (unless debug #+clisp (ext:quit) #+sbcl (sb-ext:quit)))

#+clisp
(defun make-exec ()
  (ext:saveinitmem "rouge.exe"
                   :quiet t :norc t :init-function #'start-game
                   :start-package :rougelike :executable t))
