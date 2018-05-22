;; This software is Copyright (c) 2018 Akihide Nano (an74abc@gmail.com).
;; Akihide Nano (an74abc@gmail.com) grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

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
