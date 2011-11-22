;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :root.system.utility.qpj2
  (:export :make-project)
  (:nicknames :qpj2))

(defpackage :root.system.utility.qpj2.internal
  (:use :qpj1
        :cl
        ;; :fiveam
        ))


