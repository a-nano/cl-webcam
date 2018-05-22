#|
  This file is a part of cl-webcam project.
  Copyright (c) 2018 Akihide Nano (an74abc@gmail.com)
|#

#|
  web camera api

  Author: Akihide Nano (an74abc@gmail.com)
|#

(defsystem "cl-webcam"
  :version "0.1.0"
  :author "Akihide Nano"
  :license "LLGPL"
  :depends-on ("cffi")
  :components ((:module "src"
		:serial t
                :components
                ((:file "package")
		 (:file "escapi")
		 (:file "main"))))
  :description "web camera api"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "cl-webcam-test"))))

