;;;; qpj2.lisp

(cl:in-package :root.system.utility.qpj2.internal)

;(def-suite qpj2)

;(in-suite qpj2)

;;; "qpj2" goes here. Hacks and glory await!

(defun keyword-symbolize (name)
  "Return an keyword symbol named after NAME, which is treated as a
string designator and upcased."
  (intern (string-upcase name) :keyword))

#|(test keyword-symbolize
  (is (eq (keyword-symbolize "foo")
          :foo)))|#

(defun write-package-file (name file)
  (quickproject::with-new-file (stream file)
    (quickproject::file-comment-header stream)
    (format stream "(cl:in-package :cl-user)~%")
    (terpri stream)
    (format stream "(defpackage ~S~%" (keyword-symbolize name))
    (format stream "  (:use)~%")
    (format stream "  (:export))~%")
    (terpri stream)
    (format stream "(defpackage ~S.internal~%" (keyword-symbolize name))
    (format stream "  (:use ~S :cl :named-readtables :fiveam))~%"
            (keyword-symbolize name))
    (terpri stream)))

(defun write-system-form (name &key
                               depends-on ((:stream *standard-output*)
                                           *standard-output* ))
  "Write an asdf defsystem form for NAME to STREAM."
  (let ((sys (format nil ":~A" name))
        (sys-i (format nil ":~A.internal" name)))
    (format t "(cl:in-package :asdf)~%")
    (terpri)
    (princ
     `(defsystem ,(format nil ":~A" name)
        ":serial" t
        ":depends-on"
        ,(list* ":fiveam"
                ":named-readtables"
                (when depends-on (mapcar (lambda (x) (format nil ":~A" x))
                                         depends-on)) )
        ":components" ((":file" "\"package\"")
                     (":file" "\"readtable\"")
                     (":file" ,(string-downcase name)) )))
    (terpri)
    (terpri)
    (princ
     `(defmethod perform ((o test-op) (c (eql (find-system ,sys))))
        (load-system ,sys)
        (flet ((mksym (pkg sym)
                 (intern (symbol-name sym) (find-package pkg)) ))
          (let ((result (funcall (mksym ":fiveam" ":run")
                                 (mksym ,sys-i ,sys) )))
            (or (progn
                  (funcall (mksym ":fiveam" ":explain!") result)
                  (funcall (mksym ":fiveam" ":results-status") result) )
                (error \""test-op failed\"") )))) )))

(defun write-system-file (name file &key depends-on)
  (quickproject::with-new-file (stream file)
    (quickproject::file-comment-header stream)
    (write-system-form name
                       :depends-on depends-on
                       :stream stream)
    (terpri stream)))

(defun write-application-file (name file)
  (quickproject::with-new-file (stream file)
    (quickproject::file-comment-header stream)
    (format stream "(cl:in-package ~S.internal)~%" (keyword-symbolize name))
    (format stream ";; (in-readtable ~S)~%" (keyword-symbolize name))
    (terpri stream)
    (format stream "(def-suite ~A)~%" (string-downcase name))
    (terpri stream)
    (format stream "(in-suite ~A)~%" (string-downcase name))
    (terpri stream)
    (format stream ";;; ~S goes here. Hacks and glory await!~%" name)
    (terpri stream) ))

(defun write-readtable-file (name file)
  (quickproject::with-new-file (stream file)
    (quickproject::file-comment-header stream)
    (format stream "(cl:in-package ~S.internal)~%" (keyword-symbolize name))
    (format stream "(in-readtable :common-lisp)~%")
    (terpri stream)
    (format stream "#|(defreadtable ~S" (keyword-symbolize name))
    (format stream "  (:merge :standard)~%")
    (format stream "  (:macro-char char fctn opt...)~%")
    (format stream "  (:syntax-from readtable to-char from-char)~%")
    (format stream "  (:case :upcase))|#~%")))

(defun qpj2:make-project (pathname &key
                                   depends-on
                                   (name (quickproject::pathname-project-name pathname)))
  "Create a project skeleton for NAME in PATHNAME. If DEPENDS-ON is provided,
it is used as the asdf defsystem depends-on list."
  (labels ((relative (file)
             (merge-pathnames file pathname))
           (nametype (type)
             (relative (make-pathname :name name :type type))))
    (ensure-directories-exist pathname)
    (quickproject::write-readme-file name (relative "README.org"))
    (write-system-file name (nametype "asd") :depends-on depends-on)
    (write-package-file name (relative "package.lisp"))
    (write-readtable-file name (relative "readtable.lisp"))
    (write-application-file name (nametype "lisp"))
    (pushnew (truename pathname) asdf:*central-registry*
             :test #'equal)
    name))

;;; eof