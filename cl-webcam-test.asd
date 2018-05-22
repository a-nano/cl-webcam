#|
  This file is a part of cl-webcam project.
  Copyright (c) 2018 Akihide Nano (an74abc@gmail.com)
|#

(defsystem "cl-webcam-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Akihide Nano"
  :license "LLGPL"
  :depends-on ("cl-webcam"
               "prove")
  :components ((:module "tests"
		:serial t
		:components
		((:test-file "escapi")
		 (:test-file "main"))))
  :description "Test system for cl-webcam"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
