(defpackage cl-webcam/tests/escapi
  (:use :cl
	:cl-webcam
	:prove))
(in-package :cl-webcam/tests/escapi)

(setf *enable-colors* nil)

(plan nil)

(subtest "load-library test"
  (ok (load-library)))

(subtest "escapi-dll-version test"
  (ok (escapi-dll-version)))

(subtest "escapi-version test"
  (ok (escapi-version)))

(finalize)
