(defsystem mat-ops
  :name "mat-ops"
  :author "Thomas HOULLIER"
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "mat-ops" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "mat-ops/test"))))

(defsystem mat-ops/test
  :name "mat-ops/test"
  :depends-on ("rove" "mat-ops")
  :components
  ((:module "test"
    :components ((:file "rove-suite"))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
