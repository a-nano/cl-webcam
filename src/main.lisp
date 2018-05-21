(in-package :cl-webcam)

(defconstant +nconp-rgb+ 3)

(defun convert-rgb (buf height width)
  (declare (type integer height)
	   (type integer width))
  (let ((img-buf (make-array (list (* height width +nconp-rgb+)) :initial-element 0 :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8)) img-buf))
    (loop for i fixnum below (* height width)
       do (setf (aref img-buf (* i 3)) (mem-aref buf :uchar (* i 4))
		(aref img-buf (+ (* i 3) 1)) (mem-aref buf :uchar (+ (* i 4) 1))
		(aref img-buf (+ (* i 3) 2)) (mem-aref buf :uchar (+ (* i 4) 2))))
    img-buf))


(defun get-capture (device-no &key (height 480) (width 640) (fps 10.0))
  (with-foreign-objects ((c '(:struct simple-cap-params))
			 (target-buf :int (1+ (* height width))))
    (setf (foreign-slot-value c '(:struct simple-cap-params) 'm-target-buf) target-buf)
    (setf (foreign-slot-value c '(:struct simple-cap-params) 'm-width) width)
    (setf (foreign-slot-value c '(:struct simple-cap-params) 'm-height) height)
    (setf (foreign-slot-value c '(:struct simple-cap-params) 'm-fps) fps)
    (unwind-protect 
	 (progn (init-capture device-no c)
		(do-capture device-no)
		(loop
		   if (zerop (is-capture-done device-no))
		   do (sleep 0.001)
		   else
		   return 'done)
		(is-capture-done device-no)
		(convert-rgb target-buf height width))
      (deinit-capture device-no))))
