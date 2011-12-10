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

(defun write-system-form (name &key depends-on (stream *standard-output*))
  "Write an asdf defsystem form for NAME to STREAM."
  (let ((*print-case* :downcase))
    (format stream "(cl:in-package :asdf)~%~%")
    (format stream "(defsystem ~S~%" (keyword-symbolize name))
    (format stream "  :serial t~%")
    (format stream "  :depends-on (~{~S~^~%~15T~})~%"
            (list* :fiveam :named-readtables
                   (when depends-on (mapcar #'keyword-symbolize depends-on))))
    (format stream "  :components ((:file \"package\")~%")
    (format stream "               (:file \"readtable\")~%")
    (format stream "               (:file ~S)))~%~%" name)

    (format stream "(defmethod perform ((o test-op) (c (eql (find-system ~S))))~%" (keyword-symbolize name))
    (format stream "  (load-system ~S)~%" (keyword-symbolize name))
    (format stream "  (or (flet ((_ (pkg sym)~%")
    (format stream "               (intern (symbol-name sym) (find-package pkg))))~%")
    (format stream "         (let ((result (funcall (_ :fiveam :run) (_ ~S.internal ~:*~S))))~%" (keyword-symbolize name))
    (format stream "           (funcall (_ :fiveam :explain!) result)~%")
    (format stream "           (funcall (_ :fiveam :results-status) result)))~%")
    (format stream "      (error \"test-op failed\") ))~%" ) ))

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