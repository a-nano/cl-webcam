(in-package :common-lisp)

(defpackage cl-webcam
  (:use :cl)
  (:import-from :cffi
		:defcfun
		:with-foreign-objects
		:mem-aref
		:foreign-alloc
		:foreign-slot-value)
  (:export :get-capture))

