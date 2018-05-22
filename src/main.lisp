(in-package :cl-webcam)

(defconstant +nconp-rgb+ 3)
(defconstant +buffer-size+ 256)

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

(defun get-raw-data (buf height width)
  (declare (type integer height)
	   (type integer width))
  (loop for i fixnum below (* height width (foreign-type-size :int))
       collect (mem-aref buf :uchar i)))

(defun single-capture (device-no &key (height 480) (width 640) (fps 10.0) (rgb t))
  (with-foreign-objects ((parameter '(:struct simple-cap-params))
			 (target-buf :int (1+ (* height width))))
    (setf (foreign-slot-value parameter '(:struct simple-cap-params) 'm-target-buf) target-buf)
    (setf (foreign-slot-value parameter '(:struct simple-cap-params) 'm-width) width)
    (setf (foreign-slot-value parameter '(:struct simple-cap-params) 'm-height) height)
    (setf (foreign-slot-value parameter '(:struct simple-cap-params) 'm-fps) fps)
    (unwind-protect 
	 (progn (escapi-init-capture device-no parameter)
		(escapi-do-capture device-no)
		;; Wait until capture is finish.
		;; todo more better code.. 
		(loop
		   if (zerop (escapi-is-capture-done device-no))
		   do (sleep 0.001)
		   else
		   return 'done)
		(if rgb
		    (convert-rgb target-buf height width)
		    (get-raw-data target-buf height width)))
      (escapi-deinit-capture device-no))))

(defun make-capture-parameter (height width &optional (fps 10.0))
  (let ((parameter (foreign-alloc '(:struct simple-cap-params)))
	(target-buf (foreign-alloc :int :count (1+ (* height width)))))
    (setf (foreign-slot-value parameter '(:struct simple-cap-params) 'm-target-buf) target-buf)
    (setf (foreign-slot-value parameter '(:struct simple-cap-params) 'm-height) height)
    (setf (foreign-slot-value parameter '(:struct simple-cap-params) 'm-width) width)
    (setf (foreign-slot-value parameter '(:struct simple-cap-params) 'm-fps) fps)
    parameter))

(defun clear-capture-parameters (&rest parameters)
  (let ((pointers (remove-if-not #'pointerp parameters)))
    (and pointers (mapc #'(lambda (pointer)
			    (foreign-array-free (foreign-slot-value pointer '(:struct simple-cap-params) 'm-target-buf))
			    (foreign-free pointer))
			pointers)
	 t)))

(defun get-buffer (parameter &key (rgb t))
  (let ((buffer (foreign-slot-value parameter '(:struct simple-cap-params) 'm-target-buf))
	(height (foreign-slot-value parameter '(:struct simple-cap-params) 'm-height))
	(width (foreign-slot-value parameter '(:struct simple-cap-params) 'm-width)))
    (if rgb
	(convert-rgb buffer height width)
	(get-raw-data buffer height width))))
	 
(defun get-height (parameter)
  (foreign-slot-value parameter '(:struct simple-cap-params) 'm-height))

(defun get-width (parameter)
  (foreign-slot-value parameter '(:struct simple-cap-params) 'm-width))

(defun get-fps (parameter)
  (foreign-slot-value parameter '(:struct simple-cap-params) 'm-fps))

(defun count-capture-devices ()
  (let ((result (escapi-count-capture-devices)))
    (and (not (zerop result)) result)))

(defun init-capture (device-no parameter)
  (let ((result (escapi-init-capture device-no parameter)))
    (and (not (zerop result)) t)))

(defun deinit-capture (device-no)
  (escapi-deinit-capture device-no)
  t)

(defun do-capture (device-no)
  (escapi-do-capture device-no)
  t)

(defun is-capture-done (device-no)
  (let ((result (escapi-is-capture-done device-no)))
    (and (not (zerop result)) t)))

(defun wait-capture-done (device-no &key (limit-time 3000))
  (loop with i = 0
     if (is-capture-done device-no)
     return 'done
     else
     do (progn
	  (sleep 0.001)
	  (incf i))
     if (>= i limit-time)
     do (error "wait capture time is over."))) 

(defun get-capture-device-name (device-no)
  (with-foreign-pointer-as-string (foreign-string +buffer-size+)
    (escapi-get-capture-device-name device-no foreign-string +buffer-size+)
    (foreign-string-to-lisp foreign-string)))

(defun get-capture-device-name-list ()
  (loop for i below (count-capture-devices)
     collect (cons i (get-capture-device-name i))))

(defun get-capture-property-value (device-no prop)
  (escapi-get-capture-property-value device-no prop))

(defun get-capture-property-auto (device-no prop)
  (escapi-get-capture-property-auto device-no prop))

(defun set-capture-property (device-no prop value autoval)
  (escapi-set-capture-property device-no prop value autoval))

(defun get-capture-error-line (device-no)
  (let ((result (escapi-get-capture-error-line device-no)))
    (or (zerop result) result)))

(defun get-capture-error-code (device-no)
  (let ((result (escapi-get-capture-error-code device-no)))
    (or (zerop result) result)))

;; end of file
