(asdf:defsystem "complexity-analyzer"
  :depends-on ("machine-description")
  :pathname #p"../libs/complexity-analyzer/"
  :components ((:file "package")
               (:file "complexity-analyzer" :depends-on ("package")))
  :in-order-to ((asdf:test-op (asdf:test-op "complexity-analyzer/tests"))))

(asdf:defsystem "complexity-analyzer/tests"
  :depends-on ("complexity-analyzer" "fiveam")
  :pathname #p"../libs/complexity-analyzer/"
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "tests" :depends-on ("package")))))
  :perform (asdf:test-op
            (o c)
            (uiop:symbol-call
             :fiveam :run!
             (find-symbol* '#:complexity-analyzer-tests :complexity-analyzer/tests))))
