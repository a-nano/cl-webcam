(in-package :cl-webcam)

(cffi:load-foreign-library "escapi")

(cffi:defcstruct simple-cap-params
                 "target buffer.
                  Must be at least m-width * m-height * sizeof(int) of size
                 "
                 (m-target-buf :pointer)
                 (m-width :int)
                 (m-height :int))

;; Sets up the ESCAPI DLL and the function pointers below. Call this first!
;; Returns number of capture devices found (same as countCaptureDevices, berlow)
(cffi:defcfun ("setupESCAPI" setup-escapi) :int)

;; return the number of capture devices found
(cffi:defcfun ("countCaptureDevices" count-capture-devices) :int)

;; initCapture tries to open the video capture device.
;; Retrun 0 on failuer, 1 on success.
;; Note: Capture parameter values must not change while capture device
;;       is in use (i.e between initCapture and deinitCapture).
;;       Do *not* free the target buffer, or change its pointer!
(cffi:defcfun ("initCapture" init-capture) :int
              (device-no :uint)
              (a-params :pointer))

;; deinitcapture closes the video capture device.
(cffi:defcfun ("deinitCapture" deinit-capture) :void
              (device-no :uint))

;; doCapture requests video frame to be captured.
(cffi:defcfun ("doCapture" do-capture) :void
              (device-no :uint))

;; isCaptureDone return 1 when the requested frame has been captured.
(cffi:defcfun ("isCaptureDone" is-capture-done) :int
              (device-no :uint))

;; Get the user-friendly name of capture device.
(cffi:defcfun ("getCaptureDeviceName" get-capture-device-name) :void
              (device-no :uint)
              (name-buffer :pointer)
              (buffer-length :int))

;; Retruns the ESCAPI DLL version. 0x200 for 2.0 and later (for legacy support)
(cffi:defcfun ("ESCAPIDLLVersion" escapi-dll-version) :int)

;; Retruns the ESCAPI version. 0x300 for 3.0.
;; When checking the version, accept higher versions too.
(cffi:defcfun ("ESCAPIVersionProc" escapi-version-proc) :int)

;; Gets value (0..1) of a camera property (see CAPTURE_PROPERITES, above)
(cffi:defcfun ("getCapturePropertyValueProc" get-capture-property-value-proc) :float
              (device-no :uint)
              (prop :int))

;; Gets whther the property is set to automatic (see CAPTURE_PROPERITES, above)
(cffi:defcfun ("getCapturePropertyAutoProc" get-capture-property-auto-proc) :int
              (device-no :uint)
              (prop :int))

;; Set camera property to a value (0..1) and whether it should be set to auto.
(cffi:defcfun ("setCapturePropertyProc" set-capture-property-proc) :int
              (device-no :uint)
              (prop :int)
              (value :float)
              (autoval :int))

;; Retrun line number of error, or 0 if no catastrophic error has occured.
(cffi:defcfun ("getCaptureERRORLineProc" get-capture-error-line-proc) :int
              (device-no :uint))

;; Return HRESULT of the catastrophic error, or 0 if none.
(cffi:defcfun ("getCaptureErrorCodeProc" get-capture-error-code-proc) :int
              (device-no :uint))
