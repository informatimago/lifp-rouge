(in-package :cl-user)

#+clisp
(defun make-exec ()
  (ext:saveinitmem #+unix "rouge"
                   #-unix "rouge.exe"
                   :quiet t
                   :norc t
                   :init-function (lambda ()
                                    (handler-case
                                        (progn
                                          (start-game)
                                          (ext:quit 0))
                                      (error (err)
                                             (format *error-output* "~%~A~%" err)
                                             (finish-output  *error-output*)
                                             (ext:quit 1))))
                   :start-package :rougelike
                   :executable t))
