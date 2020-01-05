;;;; clinch-test.asd


(asdf:defsystem #:clinch-test
  :description "Describe clinch-test here"
  :author "Feuer <feuer@feuerx.net>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:clinch #:cl-arrows #:clinch-freeimage)
  :components ((:file "package")
               (:file "clinch-test")))
