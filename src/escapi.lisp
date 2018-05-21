(in-package :cl-webcam)

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


(cffi:defcfun ("setupESCAPI" setup-escapi) :int
  " Sets up the ESCAPI DLL and the function pointers below. Call this first!
 Returns number of capture devices found (same as countCaptureDevices, berlow
")


(cffi:defcfun ("countCaptureDevices" count-capture-devices) :int
  "return the number of capture devices found")


(cffi:defcfun ("initCapture" init-capture) :int
  "initCapture tries to open the video capture device.
   Retrun 0 on failuer, 1 on success.
   Note: Capture parameter values must not change while capture device
         is in use (i.e between initCapture and deinitCapture).
         Do *not* free the target buffer, or change its pointer!
"
  (device-no :uint)
  (a-params :pointer))


(cffi:defcfun ("deinitCapture" deinit-capture) :void
  "deinitcapture closes the video capture device."
  (device-no :uint))


(cffi:defcfun ("doCapture" do-capture) :void
  "doCapture requests video frame to be captured."  
(device-no :uint))


(cffi:defcfun ("isCaptureDone" is-capture-done) :int
  "isCaptureDone return 1 when the requested frame has been captured."
  (device-no :uint))

;; Get the user-friendly name of capture device.
(cffi:defcfun ("getCaptureDeviceName" get-capture-device-name) :void
  (device-no :uint)
  (name-buffer :pointer)
  (buffer-length :int))

;; Retruns the ESCAPI DLL version. 0x200 for 2.0 and later (for legacy support)
(cffi:defcfun ("ESCAPIDLLVersion" escapi-dll-version) :int)

(cffi:defcfun ("ESCAPIVersion" escapi-version) :int
  "Retruns the ESCAPI version. 0x300 for 3.0.
   When checking the version, accept higher versions too.
")


(cffi:defcfun ("getCapturePropertyValue" get-capture-property-value) :float
  "Gets value (0..1) of a camera property (see CAPTURE_PROPERITES, above)"
  (device-no :uint)
  (prop :int))


(cffi:defcfun ("getCapturePropertyAuto" get-capture-property-auto) :int
  "Gets whther the property is set to automatic (see CAPTURE_PROPERITES, above)"
  (device-no :uint)
  (prop :int))


(cffi:defcfun ("setCaptureProperty" set-capture-property) :int
  "Set camera property to a value (0..1) and whether it should be set to auto."
  (device-no :uint)
  (prop :int)
  (value :float)
  (autoval :int))


(cffi:defcfun ("getCaptureERRORLine" get-capture-error-line) :int
  "Retrun line number of error, or 0 if no catastrophic error has occured."
  (device-no :uint))


(cffi:defcfun ("getCaptureErrorCode" get-capture-error-code) :int
  "Return HRESULT of the catastrophic error, or 0 if none."
  (device-no :uint))

;; end of file
