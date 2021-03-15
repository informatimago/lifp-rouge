;; -*- mode:lisp -*-

(use-package :asdf)

(defsystem rouge
  :name "rouge"
  :author "Timofei Shatrov <grue@mail.ru>"
  :description "The Rougelike"
  :components
  ((:file "curses")
   (:file "rouge" :depends-on ("curses")))
  :depends-on (:cffi :md5 :trivial-gray-streams))

