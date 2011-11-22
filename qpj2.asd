;;;; qpj2.asd

(cl:in-package :asdf)

(defsystem :qpj2
  :serial t
  :depends-on (:quickproject
               ;; :fiveam
               )
  :components ((:file "package")
               (:file "qpj2")))

#|(defmethod perform ((o test-op) (c (eql (find-system :qpj2))))
  (load-system :qpj2)
  (flet ((mksym (pkg sym)
           (intern (symbol-name sym) (find-package pkg))))
    (let ((result (funcall (mksym :fiveam :run)
                           (mksym :root.system.utility.qpj2.internal :qpj2))))
      (or (progn
            (funcall (mksym :fiveam :explain!) result)
            (funcall (mksym :fiveam :results-status) result))
          (error "test-op failed") ))))|#

