(in-package :cl-webcam)

(cffi:defcstruct simple-cap-params
  ;; target buffer.
  ;; Must be at least m-width * m-height * sizeof(int) of size
  (m-target-buf :pointer)
  ;; Buffer width
  (m-width :int)
  ;; Buffer height
  (m-height :int)
  ;; Fps
  (m-fps :float))

#-win32
(defun load-library ()
  (warn "This library is use only Windows.")
  (return-from load-library))

#+win32
(defun load-library ()
  "Load escapi.dll library.
   For use Windows only.
" 
  (cffi:load-foreign-library "escapi")

(cffi:defcstruct simple-cap-params
  ;; target buffer.
  ;; Must be at least m-width * m-height * sizeof(int) of size
  (m-target-buf :pointer)
  ;; Buffer width
  (m-width :int)
  ;; Buffer height
  (m-height :int)
  ;; Fps
  (m-fps :float))

#|
  (cffi:defcfun ("setupESCAPI" setup-escapi) :int
    "Sets up the ESCAPI DLL and the function pointers below. Call this first!
     Returns number of capture devices found (same as countCaptureDevices, berlow
  ")
|#
  
  (cffi:defcfun ("countCaptureDevices" escapi-count-capture-devices) :int
    "return the number of capture devices found")
  
  (cffi:defcfun ("initCapture" escapi-init-capture) :int
    "initCapture tries to open the video capture device.
     Retrun 0 on failuer, 1 on success.
     Note: Capture parameter values must not change while capture device
         is in use (i.e between initCapture and deinitCapture).
         Do *not* free the target buffer, or change its pointer!
  "
    (device-no :uint)
    (a-params :pointer))

  (cffi:defcfun ("deinitCapture" escapi-deinit-capture) :void
    "deinitcapture closes the video capture device."
    (device-no :uint))

  (cffi:defcfun ("doCapture" escapi-do-capture) :void
    "doCapture requests video frame to be captured."  
    (device-no :uint))

  (cffi:defcfun ("isCaptureDone" escapi-is-capture-done) :int
    "isCaptureDone return 1 when the requested frame has been captured."
    (device-no :uint))

  (cffi:defcfun ("getCaptureDeviceName" escapi-get-capture-device-name) :void
    "Get the user-friendly name of capture device."
    (device-no :uint)
    (name-buffer :pointer)
    (buffer-length :int))

  (cffi:defcfun ("ESCAPIDLLVersion" escapi-dll-version) :int
    "Retruns the ESCAPI DLL version. 0x200 for 2.0 and later (for legacy support)")

  (cffi:defcfun ("ESCAPIVersion" escapi-version) :int
    "Retruns the ESCAPI version. 0x300 for 3.0.
   When checking the version, accept higher versions too.
  ")

  (cffi:defcfun ("getCapturePropertyValue" escapi-get-capture-property-value) :float
    "Gets value (0..1) of a camera property (see CAPTURE_PROPERITES, above)"
    (device-no :uint)
    (prop :int))

  (cffi:defcfun ("getCapturePropertyAuto" escapi-get-capture-property-auto) :int
    "Gets whther the property is set to automatic (see CAPTURE_PROPERITES, above)"
    (device-no :uint)
    (prop :int))

  (cffi:defcfun ("setCaptureProperty" escapi-set-capture-property) :int
    "Set camera property to a value (0..1) and whether it should be set to auto."
    (device-no :uint)
    (prop :int)
    (value :float)
    (autoval :int))

  (cffi:defcfun ("getCaptureErrorLine" escapi-get-capture-error-line) :int
    "Retrun line number of error, or 0 if no catastrophic error has occured."
    (device-no :uint))

  (cffi:defcfun ("getCaptureErrorCode" escapi-get-capture-error-code) :int
    "Return HRESULT of the catastrophic error, or 0 if none."
    (device-no :uint))

  t)

;; end of file
