(defpackage cl-webcam/tests/main
  (:use :cl
        :cl-webcam
        :prove))
(in-package :cl-webcam/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-webcam)' in your Lisp.

(setf *enable-colors* nil)

(load-library)

(plan nil)

(subtest "single-capture test"
  (ok (single-capture 0))
  (ok (single-capture 0 :rgb nil)))

(subtest "make-capture-parameter test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (ok param)
      (clear-capture-parameters param))))

(subtest "clear-capture-parameters test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (ok (clear-capture-parameters param))))

(subtest "get-buffer test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (ok (get-buffer param))
	   (ok (get-buffer param :rgb nil)))
      (clear-capture-parameters param))))

(subtest "get-height test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (is (get-height param) 480)
    (clear-capture-parameters param)))

(subtest "get-width test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (is (get-width param) 640)
      (clear-capture-parameters param))))

(subtest "get-fps test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect 
	 (is (get-fps param) 10.0)
      (clear-capture-parameters param))))

(subtest "count-capture-devices test"
  (ok (count-capture-devices)))

(subtest "init-capture test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (ok (init-capture 0 param))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "deinit-capture test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (init-capture 0 param)
      (ok (deinit-capture 0))
      (clear-capture-parameters param))))

(subtest "do-capture test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (ok (do-capture 0))
	   (wait-capture-done 0))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "is-capture-done test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (do-capture 0)
	   (wait-capture-done 0)
	   (ok (is-capture-done 0)))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "wait-capture-done test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (do-capture 0)
	   (ok (wait-capture-done 0)))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "get-capture-device-name test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (ok (get-capture-device-name 0)))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "get-capture-device-name-list test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (ok (get-capture-device-name-list)))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "get-capture-property-value test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (ok (get-capture-property-value 0 0)))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "get-capture-property-auto test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (ok (get-capture-property-auto 0 0)))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "set-capture-property test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (ok (set-capture-property 0 1 1.0 0)))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "get-capture-error-line test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (ok (get-capture-error-line 0)))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(subtest "get-capture-error-code test"
  (let ((param (make-capture-parameter 480 640 10.0)))
    (unwind-protect
	 (progn
	   (init-capture 0 param)
	   (ok (get-capture-error-code 0)))
      (deinit-capture 0)
      (clear-capture-parameters param))))

(finalize)
