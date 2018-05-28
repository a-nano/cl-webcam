;; This software is Copyright (c) 2018 Akihide Nano (an74abc@gmail.com).
;; Akihide Nano (an74abc@gmail.com) grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage cl-webcam/tests/main
  (:use :cl
        :cl-webcam
        :prove))
(in-package :cl-webcam/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-webcam)' in your Lisp.

(setf *enable-colors* nil)

(defparameter +height+ 480)
(defparameter +width+ 640)
(defparameter +fps+ 10.0)
(defvar *device-no*)

(setf *device-no*
      (let ((no (count-capture-devices)))
	(if (zerop no)
	    (error "Camera device is not connected.")
	    (1- no))))

(plan nil)

(subtest "load-library test"
  (ok (load-library)))

(subtest "single-capture test"
  (ok (single-capture *device-no*))
  (ok (single-capture *device-no* :rgb nil)))

(subtest "make-capture-parameter test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (ok param)
      (clear-capture-parameters param))))

(subtest "clear-capture-parameters test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (ok (clear-capture-parameters param))))

(subtest "get-buffer test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (ok (get-buffer param))
	   (ok (get-buffer param :rgb nil)))
      (clear-capture-parameters param))))

(subtest "get-height test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (is (get-height param) +height+)
    (clear-capture-parameters param)))

(subtest "get-width test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (is (get-width param) +width+)
      (clear-capture-parameters param))))

(subtest "get-fps test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect 
	 (is (get-fps param) +fps+)
      (clear-capture-parameters param))))

(subtest "count-capture-devices test"
  (ok (count-capture-devices)))

(subtest "init-capture test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (ok (init-capture *device-no* param))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "deinit-capture test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (init-capture *device-no* param)
      (ok (deinit-capture *device-no*))
      (clear-capture-parameters param))))

(subtest "do-capture test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (ok (do-capture *device-no*))
	   (wait-capture-done *device-no*))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "is-capture-done test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (do-capture *device-no*)
	   (wait-capture-done *device-no*)
	   (ok (is-capture-done *device-no*)))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "wait-capture-done test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (do-capture *device-no*)
	   (ok (wait-capture-done *device-no*)))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "get-capture-device-name test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (ok (get-capture-device-name *device-no*)))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "get-capture-device-name-list test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (ok (get-capture-device-name-list)))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "get-capture-property-value test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (ok (get-capture-property-value *device-no* 0)))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "get-capture-property-auto test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (ok (get-capture-property-auto *device-no* 0)))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "set-capture-property test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (ok (set-capture-property *device-no* 1 1.0 0)))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "get-capture-error-line test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (ok (get-capture-error-line *device-no*)))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(subtest "get-capture-error-code test"
  (let ((param (make-capture-parameter +height+ +width+ +fps+)))
    (unwind-protect
	 (progn
	   (init-capture *device-no* param)
	   (ok (get-capture-error-code *device-no*)))
      (deinit-capture *device-no*)
      (clear-capture-parameters param))))

(finalize)
