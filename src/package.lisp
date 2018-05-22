(in-package :common-lisp)

(defpackage cl-webcam
  (:use :cl)
  (:import-from :cffi
		:defcfun
		:with-foreign-objects
		:with-foreign-pointer-as-string
		:mem-aref
		:foreign-alloc
		:foreign-slot-value
		:foreign-type-size
		:foreign-string-to-lisp
		:foreign-free
		:foreign-array-free
		:pointerp
		:*foreign-library-directories*)
  ;; cffi
  (:export :*foreign-library-directories*)
  ;; escapi.lisp
  (:export :load-library
	   :escapi-dll-version
	   :escapi-version)
  ;; main.lisp
  (:export :single-capture
	   :make-capture-parameter
	   :clear-capture-parameters
	   :get-buffer
	   :get-height
	   :get-width
	   :get-fps
	   :count-capture-devices
	   :init-capture
	   :deinit-capture
	   :do-capture
	   :is-capture-done
	   :wait-capture-done
	   :get-capture-device-name
	   :get-capture-device-name-list
	   :get-capture-property-value
	   :get-capture-property-auto
	   :set-capture-property
	   :get-capture-error-line
	   :get-capture-error-code))
