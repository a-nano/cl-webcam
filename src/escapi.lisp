(in-package :cl-webcam)

(cffi:load-foreign-library "escapi")

(cffi:defcstruct simple-cap-params
                 "target buffer.
                  Must be at least m-width * m-height * sizeof(int) of size
                 "
                 (m-target-buf :pointer)
                 (m-width :int)
                 (m-height :int))

(cffi:defcfun ("setupESCAPI" setup-escapi) :int)

(cffi:defcfun ("countCaptureDevices" count-capture-devices) :int)

(cffi:defcfun ("initCapture" init-capture) :int
              (deviceno :uint)
              (aparams :pointer))
